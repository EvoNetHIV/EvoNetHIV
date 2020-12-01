#' @title PrEP module 
#'
#' @description PrEP module parameterized with MSM values
#'
#' @param x A number.
#' @param y A number.
#' @return return value here.
#' @details
#' Additional details here
#' @examples
#' example function call here

#' @export
social_treatement_prep_SES<-function(dat,at){
  
  #partner list code doesn't work on first timestep
  if(at<3){return(dat)}
  
  #convert fast edgelist indices to evonet indices
  aa<- dat$el[[1]]
  col1=dat$attr$id[aa[,1]]
  col2=dat$attr$id[aa[,2]]
  
  #---------------------------------------------
  ## start prep treatment campaign
  if(at < dat$param$start_prep_campaign[1]){return(dat)}
  
  #---------------------------------------------
  # keep track of duration of relationship with positive partner & diagnosed/disclosed partners
  # for hiv- agents
  status1= dat$attr$Status[col1]
  status2= dat$attr$Status[col2]
  #diag1 <- dat$attr$diag_status[col1]
  #diag2 <- dat$attr$diag_status[col2]
  disc1 <- dat$attr$disclosure_status[col1]
  disc2 <- dat$attr$disclosure_status[col2]
  #V1    <- dat$attr$V[col1]
  #V2    <- dat$attr$V[col2]
  txt1  <- dat$attr$treated[col1]
  txt2  <- dat$attr$treated[col2]
  condom_use1 <- dat$attr$individual_condom_prob[col1]
  condom_use2 <- dat$attr$individual_condom_prob[col2]
  
  ix1=which(status1==1 & status2==0 )
  ix2=which(status1==0 & status2==1 )
  ix3=which(status1==0 & status2==0 )
  
  jx1=which(disc1==1 & is.na(disc2))
  jx2=which(is.na(disc1) & disc2==1 )
  jx3=which(is.na(disc1) & is.na(disc2))
  
  #---------------------------------------------
  #keep track of number of partners for given time period #condition 3
  dat$attr$no_partners_past_prep = 1*(dat$attr$last_ts_relationship>at) + 1*(dat$attr$last_ts_multiple_relationships>at)
  #---------------------------------------------
  #track whether at least one disclosed hiv+ partner #condition 5
  disclose1=which(status1==0 & status2==1 & disc2==1)
  disclose2=which(status1==1 & status2==0 & disc1==1)
  dat$attr$have_disclosed_partner[col1[disclose1]] <- 1
  dat$attr$have_disclosed_partner[col2[disclose2]] <- 1
  #---------------------------------------------
  # track partnerships with recently tested partners #condition 4
  tested1 <- (dat$attr$last_neg_test[col1]*(dat$attr$last_neg_test[col1]>0))+((abs(dat$attr$last_neg_test[col1])+at)*(dat$attr$last_neg_test[col1]<0))
  tested2 <- (dat$attr$last_neg_test[col2]*(dat$attr$last_neg_test[col2]>0))+((abs(dat$attr$last_neg_test[col2])+at)*(dat$attr$last_neg_test[col2]<0))
  t1=which(abs(at - tested2)<dat$param$prep_recent_test)
  t2=which(abs(at - tested1)<dat$param$prep_recent_test)
  dat$attr$partner_recent_test[col1[t1]] <- 1
  dat$attr$partner_recent_test[col1[t2]] <- 1
  #---------------------------------------------
  #track whether at least one unknown status partner #condition 6
  unknown1=which((status1==0 & status2==1 & disc2==0)|(abs(at - tested2)>dat$param$prep_recent_test))
  unknown2=which((status1==1 & status2==0 & disc1==0)|(abs(at - tested1)>dat$param$prep_recent_test))
  dat$attr$have_unknown_status_partner[col1[unknown1]] <- 1
  dat$attr$have_unknown_status_partner[col2[unknown2]] <- 1
  #---------------------------------------------
  #track whether agent uses condoms #conditions 6 & 7
  condom_user1=which(condom_use1>=0.95)
  condom_user2=which(condom_use2>=0.95)
  dat$attr$agent_condom_user[col1[condom_user1]] <- 1
  dat$attr$agent_condom_user[col2[condom_user2]] <- 1
  #or if their monogamous partner does
  #condom_partner1=which(condom_use2>=0.95 & dat$attr$no_partners_now_prep==1)
  #condom_partner2=which(condom_use1>=0.95 & dat$attr$no_partners_now_prep==1)
  #dat$attr$agent_condom_user[col1[condom_partner1]] <- 1
  #dat$attr$agent_condom_user[col2[condom_partner1]] <- 1
  #---------------------------------------------
  # track partnerships with supressed partners #re-add later? not in CDC scheme now
  #s1=which(status1==0 & status2==1 & txt2==1 & (V2<dat$param$vl_undetectable))
  #s2=which(status1==1 & status2==0 & txt1==1 & (V1<dat$param$vl_undetectable))
  #dat$attr$have_suppressed_partner[col1[s1]] <- 1
  #dat$attr$have_suppressed_partner[col2[s2]] <- 1
  
  #---------------------------------------------
  #Eligible for PrEP  
  prep_ix <-  which(
      is.na(dat$attr$on_prep) &                                            # not currently on PrEP
      (dat$attr$last_neg_test == at) &                                     # 1. at HIV test visit 
      is.na(dat$attr$diag_status) &                                        # 2. does not have diagnosed HIV
      (dat$attr$no_partners_past_prep >= dat$param$min_past_partners_prep) & # 3. has had at least 1 partner in last 6 months
      ((dat$attr$no_partners_past_prep==1 & dat$attr$partner_recent_test==1)==F) &  # 4. not in monogamous recently tested partnership
      # AND ONE OF THE FOLLOWING
      ((dat$attr$have_disclosed_partner==1) |  # 5. in relationship with known positive partner 
         (dat$attr$have_unknown_status_partner==1 & is.na(dat$attr$agent_condom_user) & dat$attr$no_partners_past_prep==1) | # OR 6. condomless acts with 1 status unknown partner
         (is.na(dat$attr$agent_condom_user) & dat$attr$no_partners_past_prep>1)  # OR 7. condomless acts with 2+ partners 
      ))
  #Eligible for PrEP and generic attr  
  if(!is.logical(dat$param$generic_nodal_att_percent_eligible_on_prep)){
    prep_ix_1 <-  which(
        dat$attr$att1==1 &
        is.na(dat$attr$on_prep) &                                            # not currently on PrEP
        (dat$attr$last_neg_test == at) &                                     # 1. at HIV test visit 
        is.na(dat$attr$diag_status) &                                        # 2. does not have diagnosed HIV
        (dat$attr$no_partners_past_prep >= dat$param$min_past_partners_prep) & # 3. has had at least 1 partner in last 6 months
        ((dat$attr$no_partners_past_prep==1 & dat$attr$partner_recent_test==1)==F) &  # 4. not in monogamous recently tested partnership
        # AND ONE OF THE FOLLOWING
        ((dat$attr$have_disclosed_partner==1) |  # 5. in relationship with known positive partner 
           (dat$attr$have_unknown_status_partner==1 & is.na(dat$attr$agent_condom_user) & dat$attr$no_partners_past_prep==1) | # OR 6. condomless acts with 1 status unknown partner
           (is.na(dat$attr$agent_condom_user) & dat$attr$no_partners_past_prep>1)  # OR 7. condomless acts with 2+ partners 
        ))
  prep_ix_2 <-  which(
      dat$attr$att1==2 &
      is.na(dat$attr$on_prep) &                                            # not currently on PrEP
      (dat$attr$last_neg_test == at) &                                     # 1. at HIV test visit 
      is.na(dat$attr$diag_status) &                                        # 2. does not have diagnosed HIV
      (dat$attr$no_partners_past_prep >= dat$param$min_past_partners_prep) & # 3. has had at least 1 partner in last 6 months
      ((dat$attr$no_partners_past_prep==1 & dat$attr$partner_recent_test==1)==F) &  # 4. not in monogamous recently tested partnership
      # AND ONE OF THE FOLLOWING
      ((dat$attr$have_disclosed_partner==1) |  # 5. in relationship with known positive partner 
         (dat$attr$have_unknown_status_partner==1 & is.na(dat$attr$agent_condom_user) & dat$attr$no_partners_past_prep==1) | # OR 6. condomless acts with 1 status unknown partner
         (is.na(dat$attr$agent_condom_user) & dat$attr$no_partners_past_prep>1)  # OR 7. condomless acts with 2+ partners 
      ))}
  #Not eligible for PrEP
  prep_not_ix <-  which(
      (dat$attr$on_prep==1) &
      (dat$attr$last_neg_test == at) &                                     # 1. at HIV test visit 
      ((dat$attr$diag_status==1) |                                         # 2. has diagnosed HIV
      (dat$attr$no_partners_past_prep < dat$param$min_past_partners_prep) | # 3. too few partners in last 6 months
      ((dat$attr$no_partners_past_prep==1 & dat$attr$partner_recent_test==1)==T) |  # 4. in monogamous recently tested partnership
      # AND ONE OF THE FOLLOWING
      ((dat$attr$have_disclosed_partner!=1) &  # 5. in relationship with known positive partner 
         (dat$attr$have_unknown_status_partner!=1 | dat$attr$agent_condom_user==1 ) & # OR 6. condomless acts with 1 status unknown partner
         (dat$attr$agent_condom_user==1 | dat$attr$no_partners_past_prep<2)  # OR 7. no condomless acts with 2+ partners 
      ))) 
  
  prep_seroconverter <-  which(dat$attr$on_prep == -1 & dat$attr$diag_time == at)
  
  ### if on prep and no longer eligible
  dat$attr$on_prep[prep_not_ix] <- NA
  
  ### eligibles for prep       
  dat$attr$eligible_for_prep[prep_not_ix] <- NA
  dat$attr$eligible_for_prep[prep_ix] <- 1
  if(!is.logical(dat$param$generic_nodal_att_percent_eligible_on_prep)){
    dat$attr$eligible_for_prep_1[prep_not_ix] <- NA
    dat$attr$eligible_for_prep_2[prep_not_ix] <- NA
    dat$attr$eligible_for_prep_1[prep_ix_1] <- 1
    dat$attr$eligible_for_prep_2[prep_ix_2] <- 1}
  
  ### spaces in prep program
  possible_spaces = round(dat$param$percent_eligible_on_prep * length(which(dat$attr$eligible_for_prep==1)))
  spaces_taken = length(which(dat$attr$on_prep==1))
  spaces_vacant = possible_spaces - spaces_taken
  eligible_not_on_prep = length(prep_ix)+1
  percent_new_eligibles_on_prep = spaces_vacant/eligible_not_on_prep
  if(percent_new_eligibles_on_prep< 0) percent_new_eligibles_on_prep <- 0
  if(percent_new_eligibles_on_prep> 1) percent_new_eligibles_on_prep <- 1
  
  ### eligibles go on prep
  dat$attr$on_prep[prep_ix]<- sample(c(1,NA),
                                    length(prep_ix),
                                    replace=T,
                                    prob=c(percent_new_eligibles_on_prep, 1-percent_new_eligibles_on_prep))
  dat$attr$prep_decrease[prep_ix]<- sample(dat$param$prep_risk_decrease, #follows Jenness et al JID 2016
                                          length(prep_ix),
                                          replace=T,
                                          prob=dat$param$prep_adherance_prob)
  
  dat$attr$prep_init_time[prep_ix]<- at*dat$attr$on_prep[prep_ix]
  dat$attr$prep_discontinue_time[prep_not_ix]<- at
  dat$attr$prep_discontinue_time[prep_seroconverter]<- at
  
  ### with generic attribute 1
  if(!is.logical(dat$param$generic_nodal_att_percent_eligible_on_prep)){
    possible_spaces = round(dat$param$percent_eligible_on_prep * length(which(dat$attr$eligible_for_prep_1==1)))
    spaces_taken = length(which(dat$attr$on_prep==1 & dat$attr$att1==1))
    spaces_vacant = possible_spaces - spaces_taken
    eligible_not_on_prep = length(prep_ix_1)+1
    percent_new_eligibles_on_prep = spaces_vacant/eligible_not_on_prep
    if(percent_new_eligibles_on_prep< 0) percent_new_eligibles_on_prep <- 0
    if(percent_new_eligibles_on_prep> 1) percent_new_eligibles_on_prep <- 1
    dat$attr$on_prep[prep_ix_1]<- sample(c(1,NA),
                                        length(prep_ix_1),
                                        replace=T,
                                        prob=c(percent_new_eligibles_on_prep, 1-percent_new_eligibles_on_prep))
    dat$attr$prep_decrease[prep_ix_1]<- sample(dat$param$prep_risk_decrease,
                                            length(prep_ix_1),
                                            replace=T,
                                            prob=dat$param$prep_adherance_prob)
    if(dat$param$risk_group_prep_adherance_link==T){
      dat$attr$prep_decrease[prep_ix_1]<- sample(dat$param$prep_risk_decrease,
                                                length(prep_ix_1),
                                                replace=T,
                                                prob=dat$param$main_group_prep_adherance_prob)
    }
    dat$attr$prep_init_time[prep_ix_1]<- at*dat$attr$on_prep[prep_ix_1]
  ### with generic attribute 2
    possible_spaces = round(dat$param$generic_nodal_att_percent_eligible_on_prep * length(which(dat$attr$eligible_for_prep_2==1)))
    spaces_taken = length(which(dat$attr$on_prep==1 & dat$attr$att1==2))
    spaces_vacant = possible_spaces - spaces_taken
    eligible_not_on_prep = length(prep_ix_2)+1
    percent_new_eligibles_on_prep = spaces_vacant/eligible_not_on_prep
    if(percent_new_eligibles_on_prep< 0) percent_new_eligibles_on_prep <- 0
    if(percent_new_eligibles_on_prep> 1) percent_new_eligibles_on_prep <- 1
  dat$attr$on_prep[prep_ix_2]<- sample(c(1,NA),
                                    length(prep_ix_2),
                                    replace=T,
                                    prob=c(percent_new_eligibles_on_prep, 1-percent_new_eligibles_on_prep))
  dat$attr$prep_decrease[prep_ix_2]<- sample(dat$param$prep_risk_decrease,
                                          length(prep_ix_2),
                                          replace=T,
                                          prob=dat$param$prep_adherance_prob)
  if(dat$param$risk_group_prep_adherance_link==T){
    dat$attr$prep_decrease[prep_ix_2]<- sample(dat$param$prep_risk_decrease,
                                              length(prep_ix_2),
                                              replace=T,
                                              prob=dat$param$risk_group_prep_adherance_prob)
  }
  dat$attr$prep_init_time[prep_ix_2]<- at*dat$attr$on_prep[prep_ix_2]}
  
  dat$attr$prep_list <- dat$attr$on_prep
  dat$attr$prep_list[is.na(dat$attr$prep_list)] <- 0
  
  dat$attr$prep_eligible_list <- dat$attr$eligible_for_prep
  dat$attr$prep_eligible_list[is.na(dat$attr$prep_eligible_list)] <- 0
  
  #if(at<900){return(dat)}else{browser()}
  return(dat)
}
