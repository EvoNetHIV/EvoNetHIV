#' @title Title
#'
#' @description Description
#'
#' @param x A number.
#' @param y A number.
#' @return return value here.
#' @details
#' Additional details here
#' @examples
#' example function call here

#' @export
summary_popsumm_deprec<-function(dat,at){

  #if summary stats calculations doesn't occur at this timesetp,
  #based on popsumm_frequency value, then leave function
  if( (at%%dat$param$popsumm_frequency!=0) & (at!=1)){return(dat)}
  
  #time_index is a time vector based on current value
  #of "at" and parameter value "popsumm_frequency"
  if(at==1)
    time_index <- 1
  else if(at==dat$param$popsumm_frequency)
    time_index <- (at-dat$param$popsumm_frequency+2):at
  else
    time_index<- (at-dat$param$popsumm_frequency+1):at
  
  #"popsumm_index" is an index for the "popsumm" vectors
  #based on value of "at" and paramter "popsumm_frequency"
  if(at==1)
  popsumm_index <- 1
  else
    if(dat$param$popsumm_frequency==1)
      popsumm_index <- at
    else
      popsumm_index <- (at/dat$param$popsumm_frequency)+1
  
  
  #logical vectors and indices helpful to calculate summary stats  
  inf_index     <-  dat$attr$Status == 1
  total_inf     <- length(which(inf_index))
  sus_index     <-  dat$attr$Status == 0
  care_index    <-  dat$attr$eligible_care == 1
  male_index    <- dat$attr$sex == "m" & dat$attr$Status >= 0
  female_index  <- dat$attr$sex == "f" & dat$attr$Status >= 0
  inf_male_index <- dat$attr$sex == "m" & inf_index
  inf_female_index <- dat$attr$sex == "f" & inf_index
  treated_inf_male_index <- dat$attr$sex == "m" & inf_index  & dat$attr$treated == 1
  treated_inf_female_index <- dat$attr$sex == "f" & inf_index  & dat$attr$treated == 1
  alive_index   <-  inf_index | sus_index
  total_alive <- length(which(alive_index))
  treated_index <-  dat$attr$treated == 1 & inf_index
  not_treated_index <-  dat$attr$treated == 0 & inf_index
  treated_undetectable <- treated_index & dat$attr$V<dat$param$vl_undetectable
  treated_agents <- which(treated_index)
  not_treated_agents <- which(not_treated_index)
  no_females_alive <- length(which(female_index & alive_index))
  no_males_alive <- length(which(male_index & alive_index))
  circum_prev <- length(which(male_index & dat$attr$circum == 1))/no_males_alive
  
  under30_index <- dat$attr$age < 30 & dat$attr$Status >= 0
  inf_under30_index <- dat$attr$age < 30 & inf_index
  treated_inf_under30_index <- dat$attr$age < 30 & inf_index & dat$attr$treated == 1
  agents30to50_index <- dat$attr$age >= 30 & dat$attr$age < 50 & dat$attr$Status >= 0 # Testing need to change 31 back to 50
  inf_agents30to50_index <- dat$attr$age >= 30 & dat$attr$age < 50 & inf_index
  treated_inf_agents30to50_index <- dat$attr$age >= 30 & dat$attr$age < 50 & inf_index & dat$attr$treated == 1
  over50_index <- dat$attr$age >= 50 & dat$attr$Status >= 0
  inf_over50_index <- dat$attr$age >= 50 & inf_index
  treated_inf_over50_index <- dat$attr$age >= 50 & inf_index & dat$attr$treated == 1
  agents_under30 <- which(under30_index)
  agents_30to50 <- which(agents30to50_index)
  agents_over50 <- which(over50_index)
  
  # Age vectors to be used in sex- and age-specific prevalence and treatment
  age_15to24 <- findInterval(dat$attr$age, c(15,25)) == 1
  age_15to49 <- findInterval(dat$attr$age, c(15,50)) == 1
  age_25to34 <- findInterval(dat$attr$age, c(25,35)) == 1
  age_35plus <- findInterval(dat$attr$age, c(35,100)) == 1
  
  # Prevalence vectors to be used in model fitting
  prev_15to24   <- length(which(inf_index & age_15to24))/length(which(alive_index & age_15to24))
  prev_15to49   <- length(which(inf_index & age_15to49))/length(which(alive_index & age_15to49))
  
  prev_f_15to24 <- length(which(inf_female_index & age_15to24))/length(which(female_index & age_15to24))
  prev_f_15to49 <- length(which(inf_female_index & age_15to49))/length(which(female_index & age_15to49))
  
  prev_m_15to24 <- length(which(inf_male_index & age_15to24))/length(which(male_index & age_15to24))
  prev_m_15to49 <- length(which(inf_male_index & age_15to49))/length(which(male_index & age_15to49))
  
  #browser()
  # Sex- and age-specific treatment coverage
  cd4_elig   <- dat$attr$CD4 %in% dat$param$cd4_treatment_threshold | dat$attr$CD4_at_trtmnt %in% dat$param$cd4_treatment_threshold
  
  # Proportion coverage among those eligible by CD4 threshold
  prop_trt_elig          <- length(which(inf_index & treated_index))/length(which(inf_index & cd4_elig))
  prop_f_trt_elig        <- length(which(inf_female_index & treated_index))/length(which(inf_female_index & cd4_elig))
  prop_m_trt_elig        <- length(which(male_index & inf_index & treated_index))/length(which(male_index & inf_index & cd4_elig))
  prop_f_15to24_trt_elig <- length(which(inf_female_index & age_15to24 & treated_index))/
                            length(which(inf_female_index & age_15to24 & cd4_elig))
  prop_f_25to34_trt_elig <- length(which(inf_female_index & age_25to34 & treated_index))/
                            length(which(inf_female_index & age_25to34 & cd4_elig))
  prop_f_35plus_trt_elig <- length(which(inf_female_index & age_35plus & treated_index))/
                            length(which(inf_female_index & age_35plus & cd4_elig))
  prop_m_15to24_trt_elig <- length(which(inf_male_index & age_15to24 & treated_index))/
                            length(which(inf_male_index & age_15to24 & cd4_elig))
  prop_m_25to34_trt_elig <- length(which(inf_male_index & age_25to34 & treated_index))/
                            length(which(inf_male_index & age_25to34 & cd4_elig))
  prop_m_35plus_trt_elig <- length(which(inf_male_index & age_35plus & treated_index))/
                            length(which(inf_male_index & age_35plus & cd4_elig))
  
  # Proportion coverage among all HIV+
  prop_trt               <- length(which(inf_index & treated_index))/length(which(inf_index))
  prop_f_trt             <- length(which(inf_female_index & treated_index))/length(which(inf_female_index))
  prop_m_trt             <- length(which(inf_male_index & treated_index))/length(which(inf_male_index))
  prop_f_15to24_trt      <- length(which(inf_female_index & age_15to24 & treated_index))/
                            length(which(inf_female_index & age_15to24))
  prop_f_25to34_trt      <- length(which(inf_female_index & age_25to34 & treated_index))/
                            length(which(inf_female_index & age_25to34))
  prop_f_35plus_trt      <- length(which(inf_female_index & age_35plus & treated_index))/
                            length(which(inf_female_index & age_35plus))
  prop_m_15to24_trt      <- length(which(inf_male_index & age_15to24 & treated_index))/
                            length(which(inf_male_index & age_15to24))
  prop_m_25to34_trt      <- length(which(inf_male_index & age_25to34 & treated_index))/
                            length(which(inf_male_index & age_25to34))
  prop_m_35plus_trt      <- length(which(inf_male_index & age_35plus & treated_index))/
                            length(which(inf_male_index & age_35plus))
  
  new_infections <- is.element(dat$attr$Time_Inf, time_index)
  new_infections_count <- length(which(is.element(dat$attr$Time_Inf, time_index)))
  new_infections_virus_vacc_sens_count <- length(which(is.element(dat$attr$Time_Inf, time_index)&
                                                         dat$attr$virus_sens_vacc==1))
  new_infections_virus_vacc_notsens_count <- length(which(is.element(dat$attr$Time_Inf, time_index)&
                                                         dat$attr$virus_sens_vacc==0))
  
  new_infections_virus_drug_sens_count <- length(which(is.element(dat$attr$Time_Inf, time_index)&
                                                         dat$attr$virus_sens_drug==1))
  new_infections_virus_drug_part_res_count <- length(which(is.element(dat$attr$Time_Inf, time_index)&
                                                         dat$attr$virus_part_res_drug==1))
  new_infections_virus_drug_3_plus_res_count <- length(which(is.element(dat$attr$Time_Inf, time_index) &
                                                         dat$attr$virus_3_plus_drug_muts==1))
  new_infections_virus_1_drug_muts <- length(which(is.element(dat$attr$Time_Inf, time_index) &
                                                             dat$attr$virus_3_plus_drug_muts==1))
  
  
  donor_time_inf  <- ifelse(new_infections_count>0,
                            dat$attr$Donors_Total_Time_Inf_At_Trans[new_infections],
                            NA)
  donor_acute_count <- ifelse(!is.na(donor_time_inf),
                              length(which(donor_time_inf<=dat$param$t_acute)),
                              NA)
  new_births <- is.element(dat$attr$arrival_time, time_index)
  cd4_aids <- dat$attr$CD4 == 4
  new_diagnoses <- dat$attr$diag_status == 1 &  is.element(dat$attr$diag_time,time_index)
  acute_phase_vec <- (at-dat$attr$Time_Inf)<dat$param$t_acute
  acute_phase <- !is.na(acute_phase_vec) & acute_phase_vec==T
  percent_virus_sensitive <- round(100*(length(which(dat$attr$virus_sens_vacc==1 & inf_index))/length(which(inf_index))))
  percentVaccinated <- round(100*(length(which(dat$attr$vaccinated == 1 & alive_index))/total_alive))
  
  #deaths
  just_died <- is.element(dat$attr$Time_Death,time_index)
  died_aids <- dat$attr$Status == -2 & just_died
  died_aids_mean_age <- mean(dat$attr$age[died_aids])
  died_non_aids <- dat$attr$Status == -1 & just_died
  died_non_aids_inf <- died_non_aids & !is.na(dat$attr$V)
  died_non_aids_sus <- died_non_aids & is.na(dat$attr$V)
  aged_out <- (dat$attr$age>=dat$param$max_age) & just_died
  
  #prep
  prop_on_prep <- length(which(alive_index & dat$attr$prep_list == 1))/total_alive
  
#browser()
  #network statistics
  # some of these can't be computed if we are in edgelist mode
  # so need to create a network from the edgelist
  if(!is.null(dat[['nw']])){
    nw <- dat[['nw']]
  } else {
    nw_summary    <-  NULL 
    number_edges <- nrow(dat$el[[1]]) 
    network_size <- attr(dat$el[[1]],'n') 
    total_nodes   <-  NULL 
    netattrs<-attributes(dat$el[[1]])
    nw <- as.network.matrix(dat$el[[1]], matrix.type='edgelist',
                            # TODO: ASSUMING THESE HAVE BEEN HARDCODED UPSTREAM
                            directed = FALSE,
                            bipartite = FALSE,
                            loops = FALSE
    )
  }
  nw_summary    <-  summary(nw~degree(0:1) + concurrent, at = at)
  number_edges <- network.edgecount(nw)
  network_size <- network.size(nw)
  total_nodes   <-  sum(nw_summary[1]+nw_summary[2]+nw_summary[3]) # This depends on nw_summary which I blanked out above
    
 
  #viral load values
  log10_vl_values  <-  log10(dat$attr$V[which(inf_index)]+dat$param$AbsoluteCut)
  spvl_untreated_values <- (
        dat$attr$LogSetPoint[which(inf_index & not_treated_index)])
  # todo: may be a faster way to calculate degree
  
  edges_by_agent <- unname(summary(nw ~ sociality(base = 0),at=at)) #use dat$attr$id for index on dat$pop
  edges_untreated <- edges_by_agent[dat$attr$id %in% not_treated_agents ]
  edges_treated <- edges_by_agent[dat$attr$id %in%  treated_agents]
  edges_under30 <- edges_by_agent[dat$attr$id %in% agents_under30]
  edges_30to50 <- edges_by_agent[dat$attr$id %in% agents_30to50]
  edges_over50 <- edges_by_agent[dat$attr$id %in% agents_over50]
  
  # Simple method for getting mean degrees for the generic attribute groups
  num_generic_attrs <- length(dat$param$generic_nodal_att_values)
  if (num_generic_attrs >= 2) {
    risk_group1 <- which(dat$attr$att1 == 1 & dat$attr$Status >=0)
    tot_grp1 <- length(risk_group1)
    edges_grp1 <-  edges_by_agent[dat$attr$id %in% risk_group1]
    
    risk_group2 <- which(dat$attr$att1 == 2 & dat$attr$Status >=0)
    tot_grp2 <- length(risk_group2)
    edges_grp2 <-  edges_by_agent[dat$attr$id %in% risk_group2]
    
    if (num_generic_attrs >= 3){
      risk_group3 <- which(dat$attr$att1 == 3 & dat$attr$Status >=0)
      tot_grp3 <- length(risk_group2)
      edges_grp3 <-  edges_by_agent[dat$attr$id %in% risk_group3]
    }
    if (num_generic_attrs >= 4){
      risk_group4 <- which(dat$attr$att1 == 4 & dat$attr$Status >=0)
      tot_grp4 <- length(risk_group4)
      edges_grp4 <-  edges_by_agent[dat$attr$id %in% risk_group4]
    }
    if (num_generic_attrs >= 5){
      risk_group5 <- which(dat$attr$att1 == 5 & dat$attr$Status >=0)
      tot_grp5 <- length(risk_group5)
      edges_grp5 <-  edges_by_agent[dat$attr$id %in% risk_group5]
    }
  }
  
   #aim3 mutations
   inf_undetect_ix <- (dat$attr$Status==1 & dat$attr$V> dat$param$vl_undetectable)
   no_inf_undect <- length(which(inf_undetect_ix))
  mutations0 <- length(which(inf_undetect_ix & dat$attr$aim3_no_muts==0))
  mutations1 <- length(which(inf_undetect_ix & dat$attr$aim3_no_muts>=1))
  mutations2 <- length(which(inf_undetect_ix & dat$attr$aim3_no_muts>=2))
  mutations3 <- length(which(inf_undetect_ix & dat$attr$aim3_no_muts>=3))
  mutations4 <- length(which(inf_undetect_ix & dat$attr$aim3_no_muts>=4))
  mutations5 <- length(which(inf_undetect_ix & dat$attr$aim3_no_muts>=5))
  
  mutations1exact <- length(which(inf_undetect_ix & dat$attr$aim3_no_muts==1))
  mutations2exact <- length(which(inf_undetect_ix & dat$attr$aim3_no_muts==2))
  mutations3exact <- length(which(inf_undetect_ix & dat$attr$aim3_no_muts==3))
  mutations4exact <- length(which(inf_undetect_ix & dat$attr$aim3_no_muts==4))
  
  mutations3plus_long <- length(which(inf_index & dat$attr$aim3_muations_long>=3)) 
  mutations4plus_long <- length(which(inf_index & dat$attr$aim3_muations_long>=4)) 
  mutations5_long <- length(which(inf_index & dat$attr$aim3_muations_long==5)) 
  
  mutations0all <- length(which( dat$attr$aim3_no_muts==0))
  mutations1all <- length(which( dat$attr$aim3_no_muts==1))
  mutations2all <- length(which( dat$attr$aim3_no_muts==2))
  mutations3all <- length(which(dat$attr$aim3_no_muts==3))
  mutations4all <- length(which(dat$attr$aim3_no_muts==4))
  mutations5all <- length(which(dat$attr$aim3_no_muts==5))

  mutations1plusall <- length(which( dat$attr$aim3_no_muts>=1))
  mutations2plusall <- length(which( dat$attr$aim3_no_muts>=2))
  mutations3plusall <- length(which(dat$attr$aim3_no_muts>=3))
  mutations4plusall <- length(which(dat$attr$aim3_no_muts>=4))

    #coital acts
  
  if(!is.null(dat$discord_coital_df)){
    number_coit_acts <- sum(tapply(dat$discord_coital_df$act_id_couple,
                            dat$discord_coital_df$couple_id,
                            max))
    acts_iev <- length(which(dat$discord_coital_df$iev==1))/2
    percent_iev <-  (acts_iev / number_coit_acts)
    transmission_opps_condom_percent <- (length(which(dat$discord_coital_df$condom==1)) / 
                                   nrow(dat$discord_coital_df) )
     trans_probs_mean <- mean(dat$discord_coital_df$trans_probs)
   
  }else{
    number_coit_acts <- 0
    percent_iev <- NA
    percent_condom <- NA
    trans_probs_mean <- NA
    transmission_opps_condom_percent <- NA
  }
    
    #actual calculation of summary stats based on indices and vectors from above
    # and functions for each variable in "popsumm_fxns"
    popsumm_vars=names(dat$popsumm)
  for(ii in 1:length(popsumm_vars)){
    temp_var<-popsumm_vars[ii]
    environment(dat$popsumm_fxns[[ii]])<-environment()
    dat$popsumm[[temp_var]][popsumm_index] <- dat$popsumm_fxns[[ii]]()
   }
  
    #calculation of generic attribute stats
    
    #what percent of alive agents are in each category
    #stat: generic_att_percent_cat_xx (xx=1,..,total number of attributes)
    
    #what percent of alive agents are infected in each category
    #stat: generic_att_percent_inf_cat_xx
    
    #stats for generic attribute values need to be treated separately
    #as the number of attributes may vary between model scenarios
    #note: objects below need to be renamed for clarity 
    temp_length <- length(dat$param$generic_nodal_att_values)
    if(temp_length>1){  
       
       #how many alive agents in each category
       temp_table=table(dat$attr$att1[alive_index])
       #how many alive and infected agents in each category
       temp_table2=table(dat$attr$att1[inf_index])
       # How many vaccinated in each category
       temp_table3 = table(dat$attr$att1[dat$attr$vaccinated == 1])
       #total agents
       sum_temp_table=sum(temp_table)
       #this vector makes sure categories from tables above are
       #arranged in ascending order (necessary if zero agents in a particular 
       #category, which would mean they are missing in tables above
       temp_match=match(names(temp_table),1:temp_length)
       
       
       for(zz in 1:length(temp_match)){
         namevec <- paste("generic_att_percent_cat_",temp_match[zz],sep="")
         dat$popsumm[[namevec]][popsumm_index]=temp_table[zz]/sum_temp_table
       }
       for(zz in 1:temp_length){
         namevec2 <- paste("generic_att_percent_inf_cat_",zz,sep="")
         
         ix1<- which(names(temp_table)==zz)
         ix2<- which(names(temp_table2)==zz)
         if(length(ix2)>0){
           val<- temp_table2[ix2]/temp_table[ix1]  
         }else{val<-0}
         dat$popsumm[[namevec2]][popsumm_index] <- val
       }
       for(zz in 1:temp_length) {
         namevec3 <- paste("generic_att_percent_vacc_cat_", zz, sep = "")
         ix1 <- which(names(temp_table) == zz)
         ix2 <- which(names(temp_table3) == zz)
         if(length(ix2) > 0) {
           val <- temp_table3[ix2]/temp_table[ix1]
         } else { val <- 0 }
         dat$popsumm[[namevec3]][popsumm_index] <- val
       }
    }
  # end of calculating summary stats for generic attributes    
return(dat)
}
