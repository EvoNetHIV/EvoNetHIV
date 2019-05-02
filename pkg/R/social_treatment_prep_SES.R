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
   status1= dat$pop$Status[col1]
   status2= dat$pop$Status[col2]
   #diag1 <- dat$pop$diag_status[col1]
   #diag2 <- dat$pop$diag_status[col2]
   disc1 <- dat$pop$disclosure_status[col1]
   disc2 <- dat$pop$disclosure_status[col2]
   #V1    <- dat$pop$V[col1]
   #V2    <- dat$pop$V[col2]
   txt1  <- dat$pop$treated[col1]
   txt2  <- dat$pop$treated[col2]
   
   ix1=which(status1==1 & status2==0 )
   ix2=which(status1==0 & status2==1 )
   ix3=which(status1==0 & status2==0 )
   
   jx1=which(disc1==1 & is.na(disc2))
   jx2=which(is.na(disc1) & disc2==1 )
   jx3=which(is.na(disc1) & is.na(disc2))
   
   #discordant couples
   #dat$pop$pos_partner_duration[col1[ix2]] <- dat$pop$pos_partner_duration[col1[ix2]]+1
   #dat$pop$pos_partner_duration[col2[ix1]] <- dat$pop$pos_partner_duration[col2[ix1]]+1
   #known discordant couples
   #dat$pop$known_pos_partner_duration[col1[jx2]] <- dat$pop$known_pos_partner_duration[col1[jx2]]+1
   #dat$pop$known_pos_partner_duration[col2[jx1]] <- dat$pop$known_pos_partner_duration[col2[jx1]]+1
   #both partners hiv-
   #ix3b <- which(!is.element(ix3, c(ix1,ix2)))
   #dat$pop$pos_partner_duration[ix3[ix3b]] <- 0
   #isolates (not in partnership)
   #ix4=1:length(dat$pop$Status)
   #ix5=which(!is.element(ix4,unique(c(col1,col2))))
   #dat$pop$pos_partner_duration[ix5] <- 0
   #---------------------------------------------
   #keep track of number of partners for given time period #condition 3
   partnerlist <- summary_partner_list(dat)
   length(dat$pop$Status)
   partner_thresh <- at-dat$param$no_past_partners_time_prep
   
   new_part_list<- lapply(1:length(partnerlist),
                          function(x){ length(which(partnerlist[[x]]>partner_thresh))})
   new_part_vec <- unlist(new_part_list)
   #qaqc here
   #if(length(new_part_vec)!=length(dat$pop$Status)){browser()}
   dat$pop$no_partners_past_prep <- new_part_vec
   #---------------------------------------------
   #keep track of current number of partners #condition 4
   partner_count_now <- table(c(col1,col2))
   partner_count_vec <- rep(0, length(dat$pop$Status))
   partner_count_vec[as.numeric(names(partner_count_now))] <- as.numeric(partner_count_now)
   dat$pop$no_partners_now_prep <- partner_count_vec
   #---------------------------------------------
   #track whether at least one diagnosed hiv+ partner
   #d1=which(status1==0 & status2==1 & diag2==1)
   #d2=which(status1==1 & status2==0 & diag1==1)
   #dat$pop$have_diag_partner[col1[d1]] <- 1
   #dat$pop$have_diag_partner[col2[d2]] <- 1
   #---------------------------------------------
   #track whether at least one disclosed hiv+ partner #condition 5
   disclose1=which(status1==0 & status2==1 & disc2==1)
   disclose2=which(status1==1 & status2==0 & disc1==1)
   dat$pop$have_disclosed_partner[col1[disclose1]] <- 1
   dat$pop$have_disclosed_partner[col2[disclose2]] <- 1
   #---------------------------------------------
   # track partnerships with recently tested partners #condition 4
   tested1 <- (dat$pop$last_neg_test[col1]*(dat$pop$last_neg_test[col1]>0))+((abs(dat$pop$last_neg_test[col1])+at)*(dat$pop$last_neg_test[col1]<0))
   tested2 <- (dat$pop$last_neg_test[col2]*(dat$pop$last_neg_test[col2]>0))+((abs(dat$pop$last_neg_test[col2])+at)*(dat$pop$last_neg_test[col2]<0))
   t1=which(abs(at - tested2)<dat$param$prep_recent_test)
   t2=which(abs(at - tested1)<dat$param$prep_recent_test)
   dat$pop$partner_recent_test[col1[t1]] <- 1
   dat$pop$partner_recent_test[col1[t2]] <- 1
   #---------------------------------------------
   #track whether at least one unknown status partner #condition 6
   unknown1=which((status1==0 & status2==1 & disc2==0)|(abs(at - tested2)>dat$param$prep_recent_test))
   unknown2=which((status1==1 & status2==0 & disc1==0)|(abs(at - tested1)>dat$param$prep_recent_test))
   dat$pop$have_unknown_partner[col1[unknown1]] <- 1
   dat$pop$have_unknown_partner[col2[unknown2]] <- 1
   #---------------------------------------------
   # track partnerships with supressed partners #re-add later? not in CDC scheme now
   #s1=which(status1==0 & status2==1 & txt2==1 & (V2<dat$param$vl_undetectable))
   #s2=which(status1==1 & status2==0 & txt1==1 & (V1<dat$param$vl_undetectable))
   #dat$pop$have_suppressed_partner[col1[s1]] <- 1
   #dat$pop$have_suppressed_partner[col2[s2]] <- 1
   
   #---------------------------------------------
   # more in line with social_treatment_prep
  # prep_ix <-  which((dat$pop$pos_partner_duration >= dat$param$min_pos_partner_duration | # in relationship with positive partner for x duration OR
  #       dat$pop$no_partners_past_prep >= dat$param$min_past_partners_prep |               # have had over y partners in last year OR
  #       dat$pop$no_partners_now_prep >= dat$param$min_current_partners_prep |             # are in over z concurrent partnerships at present OR
  #       dat$pop$have_disc_partner == 1 )  &                                               # current partner has disclosed HIV+ status
  #       is.na(dat$pop$diag_status))                                                       # AND does not have diagnosed HIV
  
   prep_ix <-  which(
       (dat$pop$last_neg_test == at) &                                     # 1. at HIV test visit 
       is.na(dat$pop$diag_status) &                                        # 2. does not have diagnosed HIV
       (dat$pop$no_partners_past_prep >= dat$param$min_past_partners_prep) & # 3. has had at least 1 partner in last 6 months
       ((dat$pop$no_partners_now_prep==1 & dat$pop$partner_recent_test==1)==F) &  # 4. not in monogamous recently tested partnership
       # AND ONE OF THE FOLLOWING
       (( dat$pop$have_disclosed_partner==1)  # 5. in relationship with known positive partner 
         # OR 6. condomless acts with 1 status unknown partner
         # OR 7. condomless acts with 2+ partners 
                   
                  ))
    
   ### change from social_treatment_prep: there dat$pop$eligible_for_prep is dat$pop$on_prep       
   dat$pop$eligible_for_prep[prep_ix] <- 1
   
   dat$pop$on_prep[prep_ix]<- sample(c(1,NA),
                                    length(prep_ix),
                                    replace=T,
                                    prob=c(dat$param$percent_eligible_on_prep, 1-dat$param$percent_eligible_on_prep))
   
   dat$pop$prep_decrease[prep_ix]<- sample(c(0.0,0.31,0.81,0.95),
                                           length(prep_ix),
                                           replace=T,
                                           prob=c(0.21,0.07,0.10,0.62))
   
   dat$pop$prep_list <- dat$pop$on_prep
   dat$pop$prep_list[is.na(dat$pop$prep_list)] <- 0
   
   #if(at<900){return(dat)}else{browser()}
   return(dat)
}