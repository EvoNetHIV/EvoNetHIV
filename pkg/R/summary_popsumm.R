summary_popsumm<-function(dat,at){

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
    inf_index     <-  dat$pop$Status == 1
    total_inf     <- length(which(inf_index))
    sus_index     <-  dat$pop$Status == 0
    care_index    <-  dat$pop$eligible_care == 1
    male_index    <- dat$pop$sex == "m" & dat$pop$Status >= 0
    female_index  <- dat$pop$sex == "f" & dat$pop$Status >= 0
    inf_male_index <- dat$pop$sex == "m" & inf_index
    inf_female_index <- dat$pop$sex == "f" & inf_index
    treated_inf_male_index <- dat$pop$sex == "m" & inf_index  & dat$pop$treated == 1
    treated_inf_female_index <- dat$pop$sex == "f" & inf_index  & dat$pop$treated == 1
    alive_index   <-  inf_index | sus_index
    total_alive <- length(which(alive_index))
    treated_index <-  dat$pop$treated == 1 & inf_index
    not_treated_index <-  dat$pop$treated == 0 & inf_index
    treated_undetectable <- treated_index & dat$pop$V<dat$param$vl_undetectable
    treated_agents <- which(treated_index)
    not_treated_agents <- which(not_treated_index)
    no_females_alive <- length(which(female_index & alive_index))
    no_males_alive <- length(which(male_index & alive_index))
    circum_prev <- length(which(male_index & dat$pop$circum == 1))/no_males_alive

    under30_index <- dat$pop$age < 30 & dat$pop$Status >= 0
    inf_under30_index <- dat$pop$age < 30 & inf_index
    treated_inf_under30_index <- dat$pop$age < 30 & inf_index & dat$pop$treated == 1
    agents30to50_index <- dat$pop$age >= 30 & dat$pop$age < 50 & dat$pop$Status >= 0 # Testing need to change 31 back to 50
    inf_agents30to50_index <- dat$pop$age >= 30 & dat$pop$age < 50 & inf_index
    treated_inf_agents30to50_index <- dat$pop$age >= 30 & dat$pop$age < 50 & inf_index & dat$pop$treated == 1
    over50_index <- dat$pop$age >= 50 & dat$pop$Status >= 0
    inf_over50_index <- dat$pop$age >= 50 & inf_index
    treated_inf_over50_index <- dat$pop$age >= 50 & inf_index & dat$pop$treated == 1
    agents_under30 <- which(under30_index)
    agents_30to50 <- which(agents30to50_index)
    agents_over50 <- which(over50_index)

     # Age vectors to be used in sex- and age-specific prevalence and treatment
    age_15to24 <- findInterval(dat$pop$age, c(15,25)) == 1
    age_15to49 <- findInterval(dat$pop$age, c(15,50)) == 1
    age_25to34 <- findInterval(dat$pop$age, c(25,35)) == 1
    age_35plus <- findInterval(dat$pop$age, c(35,100)) == 1

    # Prevalence vectors to be used in model fitting
    prev_15to24   <- length(which(inf_index & age_15to24))/length(which(alive_index & age_15to24))
    prev_15to49   <- length(which(inf_index & age_15to49))/length(which(alive_index & age_15to49))

    prev_f_15to24 <- length(which(inf_female_index & age_15to24))/length(which(female_index & age_15to24))
    prev_f_15to49 <- length(which(inf_female_index & age_15to49))/length(which(female_index & age_15to49))

    prev_m_15to24 <- length(which(inf_male_index & age_15to24))/length(which(male_index & age_15to24))
    prev_m_15to49 <- length(which(inf_male_index & age_15to49))/length(which(male_index & age_15to49))

    #browser()
    # Sex- and age-specific treatment coverage
    cd4_elig   <- dat$pop$CD4 %in% dat$param$cd4_treatment_threshold | dat$pop$CD4_at_trtmnt %in% dat$param$cd4_treatment_threshold

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

    new_infections <- is.element(dat$pop$Time_Inf, time_index)
    new_infections_count <- length(which(new_infections))
    new_infections_virus_vacc_sens_count <- length(which(is.element(dat$pop$Time_Inf, time_index)&
                                                           dat$pop$virus_sens_vacc==1))
    new_infections_virus_vacc_notsens_count <- length(which(is.element(dat$pop$Time_Inf, time_index)&
                                                              dat$pop$virus_sens_vacc==0))

    new_infections_virus_drug_sens_count <- length(which(is.element(dat$pop$Time_Inf, time_index)&
                                                           dat$pop$virus_sens_drug==1))
    new_infections_virus_drug_part_res_count <- length(which(is.element(dat$pop$Time_Inf, time_index)&
                                                               dat$pop$virus_part_res_drug==1))
    new_infections_virus_drug_3_plus_res_count <- length(which(is.element(dat$pop$Time_Inf, time_index) &
                                                                 dat$pop$virus_3_plus_drug_muts==1))
    new_infections_virus_1_drug_muts <- length(which(is.element(dat$pop$Time_Inf, time_index) &
                                                       dat$pop$virus_3_plus_drug_muts==1))


 if(new_infections_count>0){
   
  donor_time_inf <- dat$pop$Time_Inf[which(new_infections)]-dat$pop$Donors_Total_Time_Inf_At_Trans[which(new_infections)] 
  donor_time_inf_index <- which(donor_time_inf <= dat$param$t_acute)
  donor_acute_count <- length(donor_time_inf_index)  
  if(donor_acute_count>0){
    dat$popsumm$percent_donor_acute[popsumm_index]<- donor_acute_count/new_infections_count
  }else{
    dat$popsumm$percent_donor_acute[popsumm_index] <- 0
  }
  
 }
        
    new_births <- is.element(dat$pop$arrival_time, time_index)
    cd4_aids <- dat$pop$CD4 == 4
    new_diagnoses <- dat$pop$diag_status == 1 &  is.element(dat$pop$diag_time,time_index)
    percent_virus_sensitive <- round(100*(length(which(dat$pop$virus_sens_vacc==1 & inf_index))/length(which(inf_index))))
    percentVaccinated <- round(100*(length(which(dat$pop$vaccinated == 1 & alive_index))/total_alive))

    #deaths
    just_died <- is.element(dat$pop$Time_Death,time_index)
    died_aids <- dat$pop$Status == -2 & just_died
    died_aids_mean_age <- mean(dat$pop$age[died_aids])
    died_non_aids <- dat$pop$Status == -1 & just_died
    died_non_aids_inf <- died_non_aids & !is.na(dat$pop$V)
    died_non_aids_sus <- died_non_aids & is.na(dat$pop$V)
    aged_out <- (dat$pop$age>=dat$param$max_age) & just_died

    #prep
    prop_on_prep <- length(which(alive_index & dat$pop$prep_list == 1))/total_alive
    prop_eligible_prep <- length(which(alive_index & dat$pop$prep_eligible_list == 1))/total_alive

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
    log10_vl_values  <-  log10(dat$pop$V[which(inf_index)]+dat$param$AbsoluteCut)
    spvl_untreated_values <- (
      dat$pop$LogSetPoint[which(inf_index & not_treated_index)])
    # todo: may be a faster way to calculate degree

    edges_by_agent <- unname(summary(nw ~ sociality(base = 0),at=at)) #use dat$attr$id for index on dat$pop
    edges_untreated <- edges_by_agent[dat$attr$id %in% not_treated_agents ]
    edges_treated <- edges_by_agent[dat$attr$id %in%  treated_agents]
    edges_under30 <- edges_by_agent[dat$attr$id %in% agents_under30]
    edges_30to50 <- edges_by_agent[dat$attr$id %in% agents_30to50]
    edges_over50 <- edges_by_agent[dat$attr$id %in% agents_over50]


    #aim3 mutations
    inf_undetect_ix <- (dat$pop$Status==1 & dat$pop$V> dat$param$vl_undetectable)
    no_inf_undect <- length(which(inf_undetect_ix))
    mutations0 <- length(which(inf_undetect_ix & dat$pop$aim3_no_muts==0))
    mutations1 <- length(which(inf_undetect_ix & dat$pop$aim3_no_muts>=1))
    mutations2 <- length(which(inf_undetect_ix & dat$pop$aim3_no_muts>=2))
    mutations3 <- length(which(inf_undetect_ix & dat$pop$aim3_no_muts>=3))
    mutations4 <- length(which(inf_undetect_ix & dat$pop$aim3_no_muts>=4))
    mutations5 <- length(which(inf_undetect_ix & dat$pop$aim3_no_muts>=5))

    mutations1exact <- length(which(inf_undetect_ix & dat$pop$aim3_no_muts==1))
    mutations2exact <- length(which(inf_undetect_ix & dat$pop$aim3_no_muts==2))
    mutations3exact <- length(which(inf_undetect_ix & dat$pop$aim3_no_muts==3))
    mutations4exact <- length(which(inf_undetect_ix & dat$pop$aim3_no_muts==4))

    mutations3plus_long <- length(which(inf_index & dat$pop$aim3_muations_long>=3))
    mutations4plus_long <- length(which(inf_index & dat$pop$aim3_muations_long>=4))
    mutations5_long <- length(which(inf_index & dat$pop$aim3_muations_long==5))

    mutations0all <- length(which( dat$pop$aim3_no_muts==0))
    mutations1all <- length(which( dat$pop$aim3_no_muts==1))
    mutations2all <- length(which( dat$pop$aim3_no_muts==2))
    mutations3all <- length(which(dat$pop$aim3_no_muts==3))
    mutations4all <- length(which(dat$pop$aim3_no_muts==4))
    mutations5all <- length(which(dat$pop$aim3_no_muts==5))

    mutations1plusall <- length(which( dat$pop$aim3_no_muts>=1))
    mutations2plusall <- length(which( dat$pop$aim3_no_muts>=2))
    mutations3plusall <- length(which(dat$pop$aim3_no_muts>=3))
    mutations4plusall <- length(which(dat$pop$aim3_no_muts>=4))

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

  #---------------------------------------------
  # stats calculated for every run

  dat$popsumm$timestep[popsumm_index]<- at
  dat$popsumm$prevalence[popsumm_index]<-length(which(inf_index))/length(which(alive_index))
  dat$popsumm$new_infections[popsumm_index]<-new_infections_count
  dat$popsumm$susceptibles[popsumm_index]<-length(which(sus_index))
  dat$popsumm$total_infections_alive[popsumm_index]<-length(which(inf_index))
  dat$popsumm$births[popsumm_index]<-length(which(new_births))
  dat$popsumm$aids_deaths[popsumm_index]<-length(which(died_aids))
  dat$popsumm$natural_deaths[popsumm_index]<-length(which(died_non_aids))
  dat$popsumm$aged_out[popsumm_index]<-length(which(aged_out))
  dat$popsumm$natural_deaths_infecteds[popsumm_index]<-length(which(died_non_aids_inf))
  dat$popsumm$natural_deaths_susceptibles[popsumm_index]<-length(which(died_non_aids_sus))
  dat$popsumm$alive[popsumm_index]<-length(which(alive_index))

  dat$popsumm$no_in_aids_gamma[popsumm_index]<-length(which((at > (dat$pop$Time_Inf +
                                                dat$pop$RandomTimeToAIDS))&
                                                inf_index))

  dat$popsumm$no_in_aids_cd4[popsumm_index]<-length(which(cd4_aids & inf_index ))

  dat$popsumm$natural_deaths_infecteds[popsumm_index]<-length(which(died_non_aids_inf))
  dat$popsumm$natural_deaths_susceptibles[popsumm_index]<-length(which(died_non_aids_sus ))
  dat$popsumm$new_diagnoses[popsumm_index]<-length(which(new_diagnoses))
  dat$popsumm$mean_time_donor_infected_incident[popsumm_index] <- mean(dat$pop$Donors_Total_Time_Inf_At_Trans[which(new_infections)])
  dat$popsumm$mean_age_incident[popsumm_index] <- mean(dat$pop$age[which(new_infections)])
  dat$popsumm$mean_age_died_AIDS[popsumm_index]<- mean(dat$pop$age[which(died_aids)])
  dat$popsumm$mean_spvl_pop_all[popsumm_index]<- mean(dat$pop$LogSetPoint[which(inf_index)])
  dat$popsumm$mean_vl_pop_all[popsumm_index]<-mean(log10_vl_values)
  dat$popsumm$mean_spvl_incident[popsumm_index]<-mean(dat$pop$LogSetPoint[which(new_infections)])
  dat$popsumm$mean_age_infecteds[popsumm_index]<- mean(dat$pop$age[which(inf_index)])
  dat$popsumm$mean_age_susceptibles[popsumm_index]<- mean(dat$pop$age[which(sus_index)])
  dat$popsumm$mean_trans_prob[popsumm_index]<- trans_probs_mean
  dat$popsumm$no_edges[popsumm_index]<- number_edges
  dat$popsumm$mean_degree[popsumm_index]<- number_edges*2/network_size
  dat$popsumm$mean_degree_inf_untreated[popsumm_index]<- sum(edges_untreated)/length(not_treated_agents)
  dat$popsumm$mean_spvl_pop_untreated[popsumm_index]<- mean(spvl_untreated_values)
  dat$popsumm$total_infections_not_treated[popsumm_index] <-length(which(inf_index & not_treated_index))

  dat$popsumm$cd4_gt_350[popsumm_index] <- length(which(inf_index & !treated_index & (dat$pop$CD4 == 1 | dat$pop$CD4 == 2)))
  dat$popsumm$cd4_200_350[popsumm_index] <- length(which(inf_index & !treated_index & dat$pop$CD4 == 3))
  dat$popsumm$cd4_0_200[popsumm_index] <- length(which(inf_index & !treated_index & dat$pop$CD4 == 4))

  #--------------------------------------------
  #network
  dat$popsumm$prop_nodes_degree_0[popsumm_index]<- nw_summary[1]/total_nodes
  dat$popsumm$prop_nodes_degree_1[popsumm_index]<- nw_summary[2]/total_nodes
  dat$popsumm$prop_nodes_concurrent[popsumm_index]<- nw_summary[3]/total_nodes
  dat$popsumm$mean_degree_under_30[popsumm_index]<- {
    if (length(agents_under30) > 0) sum(edges_under30)/length(agents_under30)
  else NA}

  dat$popsumm$mean_degree_30_50[popsumm_index]<- {
    if (length(agents_30to50) > 0)
    sum(edges_30to50)/length(agents_30to50)
    else NA}

  dat$popsumm$mean_degree_over_50[popsumm_index]<- {
    if (length(agents_over50) > 0)
    sum(edges_over50)/length(agents_over50)
    else NA}

  #--------------------------------------------
  #hetero model
  if (dat$param$model_sex=="hetero") {
    dat$popsumm$alive_female[popsumm_index]<-no_females_alive
    dat$popsumm$alive_male[popsumm_index]<-no_males_alive
    dat$popsumm$prev_15to24[popsumm_index] <-prev_15to24
    dat$popsumm$prev_15to49[popsumm_index] <-prev_15to49
    dat$popsumm$prev_f_15to24[popsumm_index] <-prev_f_15to24
    dat$popsumm$prev_f_15to49[popsumm_index] <-prev_f_15to49
    dat$popsumm$prev_m_15to24[popsumm_index] <-prev_m_15to24
    dat$popsumm$prev_m_15to49[popsumm_index] <-prev_m_15to49
    dat$popsumm$inf_men[popsumm_index]<-length(which(inf_male_index))/length(which(male_index))
    dat$popsumm$inf_women[popsumm_index]<-length(which(inf_female_index))/length(which(female_index))
    dat$popsumm$inf_under30[popsumm_index]<-length(which(inf_under30_index))/length(which(under30_index))
    dat$popsumm$inf_30to50[popsumm_index]<-length(which(inf_agents30to50_index))/length(which(agents30to50_index))
    dat$popsumm$inf_over50[popsumm_index]<-length(which(inf_over50_index))/length(which(over50_index))
  
    female_edges <-  edges_by_agent[dat$attr$sex == 'f']
    tot_grp <- length(female_edges)
    if(tot_grp>1){
      mean_degree_female <- sum(female_edges)/tot_grp
    }else{mean_degree_female <- NA}
    dat$popsumm$mean_degree_female[popsumm_index]=mean_degree_female
    
    male_edges <-  edges_by_agent[dat$attr$sex == 'm']
    tot_grp <- length(male_edges)
    if(tot_grp>1){
      mean_degree_male <- sum(male_edges)/tot_grp
    }else{mean_degree_male <- NA}
    dat$popsumm$mean_degree_male[popsumm_index]=mean_degree_male
    
    
  }

  #--------------------------------------------
  #treatment (msm and hetero models)
  if (dat$param$start_treatment_campaign[1] < 5e5) {
  dat$popsumm$no_treated[popsumm_index]<-length(which(inf_index & treated_index))
  dat$popsumm$percent_suppressed[popsumm_index]<-((length(which(treated_index &
                                        (log10(dat$pop$V)< dat$param$vl_full_supp ) )))/ length(which(inf_index)) )
  }

  #--------------------------------------------

  #hetero model with treatment
  if (dat$param$model_sex=="hetero" & dat$param$start_treatment_campaign[1] < 5e5) {  #only plot if treatment is available in model

    dat$popsumm$treated_inf_men[popsumm_index]<-length(which(treated_inf_male_index))/length(which(inf_male_index))
    dat$popsumm$treated_inf_women[popsumm_index]<-length(which(treated_inf_female_index))/length(which(inf_female_index))
    dat$popsumm$treated_inf_under30[popsumm_index]<-length(which(treated_inf_under30_index))/length(which(inf_under30_index))
    dat$popsumm$treated_inf_30to50[popsumm_index]<-length(which(treated_inf_agents30to50_index))/length(which(inf_agents30to50_index))
    dat$popsumm$treated_inf_over50[popsumm_index]<-length(which(treated_inf_over50_index))/length(which(inf_over50_index))
    dat$popsumm$no_treated_undetectable[popsumm_index]<-length(which(treated_undetectable))
    dat$popsumm$mean_vl_pop_untreated[popsumm_index]<-mean(log10(dat$pop$V[which(inf_index &  not_treated_index)]))
    dat$popsumm$percent_treated_undetectable[popsumm_index]<- length(which(treated_undetectable))/length(which(treated_index))
    dat$popsumm$total_pills_taken[popsumm_index]<- sum(c(0,dat$popsumm$no_treated[1:popsumm_index]),na.rm=T)
    dat$popsumm$mean_degree_inf_treated[popsumm_index]<- sum(edges_treated)/length(treated_agents)
  }

  #--------------------------------------------
  #prep
  if (dat$param$start_prep_campaign[1] < 5e5) {
    dat$popsumm$prop_on_prep[popsumm_index] <-prop_on_prep
    dat$popsumm$prop_eligible_prep[popsumm_index] <-prop_eligible_prep
  }

  #--------------------------------------------
  #circumcision (?)
  if(dat$param$circum_prob != 0.85) {
    # plot circumcision prevalence if not at default
    dat$popsumm$circum_prev[popsumm_index] <-circum_prev
  }

  #--------------------------------------------
  #vaccine

  if (dat$param$preventative_campaign) {    #only plot if vaccine campaign in model
    dat$popsumm$new_infections_vacc_sens_virus[popsumm_index]<- new_infections_virus_vacc_sens_count
    dat$popsumm$new_infections_vacc_resist_virus[popsumm_index]<-  new_infections_virus_vacc_notsens_count
    dat$popsumm$percent_virus_sensitive_vacc[popsumm_index]<- percent_virus_sensitive
    dat$popsumm$percentAliveVaccinated[popsumm_index]<- percentVaccinated
  }
  #--------------------------------------------
  #disease modifying vaccine
  #Note 2/11/19: intrinsic (formerly called genotypic) = spvl assigned to agents regardless of vaccine status, one that is passed on to infectees, 
  #as opposed to phenotypic spvl, which is the expressed spvl that reflects modification by vaccine. Intrinsic and phenotypic are the same in unvaccinated agents
  if(dat$param$vacc_therapeutic_campaign){
    dat$popsumm$percentAliveVaccinated[popsumm_index]<- percentVaccinated
    dat$popsumm$mean_spvl_genotype[popsumm_index] <- mean(dat$pop$LogSetPoint_genotype[which(dat$pop$Status==1 & dat$pop$vaccinated == 1 )],na.rm=T) #prevalent spvl, maybe should remove, not too useful
    dat$popsumm$mean_spvl_nonvacc[popsumm_index] <- mean(dat$pop$LogSetPoint[which( dat$pop$vaccinated == 0  &
                                                                                            dat$pop$Status==1)],na.rm=T)
    dat$popsumm$mean_spvl_nevervacc[popsumm_index] <- mean(dat$pop$LogSetPoint[which( is.na(dat$pop$vaccinated)  &
                                                                                      dat$pop$Status==1)],na.rm=T)
    
    
    new_infections_nonvacc <- which(dat$pop$Time_Inf %in% time_index &   #all new infections occuring in nonvaccinated agents during the popsumm frequency window (default 30 days)
                                      dat$pop$vaccinated == 0)
    new_infections_nevervacc <- which(dat$pop$Time_Inf %in% time_index &   #all new infections occuring in nonvaccinated agents during the popsumm frequency window (default 30 days)
                                      is.na(dat$pop$vaccinated))
    
    
    ispvl_nonvacc <- dat$pop$LogSetPoint[new_infections_nonvacc]   #spvls of newly infected nonvaccinees (phenotypic, which in nonvaccinated agents is same as intrinsic)
    ispvl_nevervacc <- dat$pop$LogSetPoint[new_infections_nevervacc]   #spvls of newly infected nonvaccinees (phenotypic, which in nonvaccinated agents is same as intrinsic)
    new_infections_vacc<-which(dat$pop$Time_Inf %in% time_index &    #all new infections occuring in vaccinated agents during the popsumm frequency window (default 30 days)
                             dat$pop$vaccinated==1)
    ispvl_vacc <- dat$pop$LogSetPoint_genotype[new_infections_vacc] #spvls of newly infected nonvaccinees (intrinsic)
    ispvl_all <- c(ispvl_nonvacc,ispvl_vacc,ispvl_nevervacc)  #all spvls of agents infected in this time window, in vaccinated and unvaccinated agents
    dat$popsumm$mean_ispvl_intrinsic_all[popsumm_index]<-mean(ispvl_all) #mean of all spvls of agents infected during this time window, provides mean incident SPVL
    dat$popsumm$mean_ispvl_intrinsic_nonvacc[popsumm_index]<-mean(ispvl_nonvacc)  #mean of nonvaccinated spvls of agents infected during this time window, provides mean incident SPVL
    dat$popsumm$mean_ispvl_instrinsic_vacc[popsumm_index]<-mean(ispvl_vacc)  #mean of vaccinated spvls of agents infected during this time window, provides mean incident SPVL
    
    eligible_index1 <- which(dat$pop$Status == 0 & 
                               is.na(dat$pop$vaccinated) &
                               dat$pop$eligible_care == 1) 
    
    #previously vaccinated
    eligible_index2 <- which(dat$pop$Status == 0 & 
                               dat$pop$vaccinated == 0 &
                               (at-dat$pop$vacc_init_time) > dat$param$vacc_eff_duration &
                               dat$pop$eligible_care == 1) 
    
    eligible_index <- c(eligible_index1,eligible_index2)
    dat$popsumm$perc_eligible_vacc[popsumm_index] <- length(eligible_index)/total_alive
  }
    
  
  #--------------------------------------------
  #"aim3"
  if(dat$param$VL_Function=="aim3"){

    dat$popsumm$total_new_infections[popsumm_index]<- sum(c(0,dat$popsumm$new_infections[1:popsumm_index]))

    dat$popsumm$new_infections_drug_sens_virus[popsumm_index]<- new_infections_virus_drug_sens_count

    dat$popsumm$new_infections_drug_part_res_virus[popsumm_index]<- new_infections_virus_drug_part_res_count

    dat$popsumm$new_infections_drug_3_plus_res_virus[popsumm_index]<- new_infections_virus_drug_3_plus_res_count

    dat$popsumm$mean_PPP_incident[popsumm_index]<- mean(dat$pop$PPP[which(new_infections)])

    dat$popsumm$mean_PPP_infected[popsumm_index]<- mean(dat$pop$PPP[which(inf_index)])

    dat$popsumm$"drug_muts_1+"[popsumm_index]<- mutations1

    dat$popsumm$"drug_muts_3+"[popsumm_index]<- mutations1

    dat$popsumm$"total_1+_drug_muts"[popsumm_index]<- {
      sum(c(0,dat$popsumm[["new_infections_virus_1_drug_muts"]][1:popsumm_index]))}

    dat$popsumm$"total_3+_drug_muts"[popsumm_index]<- {
      sum(c(0,dat$popsumm[["new_infections_virus_drug_3_plus_res_count"]][1:popsumm_index]))}

    dat$popsumm$Perc_0_drug_muts[popsumm_index]<- mutations0/no_inf_undect

    dat$popsumm$"Perc_1+_drug_muts"[popsumm_index]<- mutations1/no_inf_undect

    dat$popsumm$"Perc_2+_drug_muts"[popsumm_index]<- mutations2/no_inf_undect

    dat$popsumm$"Perc_3+_drug_muts"[popsumm_index]<- mutations3/no_inf_undect

    dat$popsumm$"Perc_4+_drug_muts"[popsumm_index]<- mutations4/no_inf_undect

    dat$popsumm$Perc_All_5_drug_muts[popsumm_index]<- mutations5/no_inf_undect

    dat$popsumm$Perc_1_drug_muts[popsumm_index]<- mutations1exact/no_inf_undect

    dat$popsumm$Perc_2_drug_muts[popsumm_index]<- mutations2exact/no_inf_undect

    dat$popsumm$Perc_3_drug_muts[popsumm_index]<- mutations3exact/no_inf_undect

    dat$popsumm$Perc_4_drug_muts[popsumm_index]<- mutations4exact/no_inf_undect
    #not graphed/overlay  only
    dat$popsumm$Perc_1_drug_muts_total_pop[popsumm_index]<- mutations1all/total_alive
    #not graphed/overlay  only
    dat$popsumm$Perc_2_drug_muts_total_pop[popsumm_index]<- mutations2all/total_alive
    #not graphed/overlay  only
    dat$popsumm$Perc_3_drug_muts_total_pop[popsumm_index]<- mutations3all/total_alive

    dat$popsumm$Perc_4_drug_muts_total_pop[popsumm_index]<- mutations4all/total_alive

    dat$popsumm$Perc_0_drug_muts_total_pop[popsumm_index]<- mutations0all/total_alive

    dat$popsumm$"Perc_1+_drug_muts_total_pop"[popsumm_index]<- mutations1plusall/total_alive

    dat$popsumm$"Perc_2+_drug_muts_total_pop"[popsumm_index]<- mutations2plusall/total_alive

    dat$popsumm$"Perc_3+_drug_muts_total_pop"[popsumm_index]<- mutations3plusall/total_alive

    dat$popsumm$"Perc_4+_drug_muts_total_pop"[popsumm_index]<- mutations4plusall/total_alive

    dat$popsumm$Perc_All_5_drug_muts_total_pop[popsumm_index]<- mutations5all/total_alive

    dat$popsumm$"Perc_3+_drug_muts_long"[popsumm_index]<- mutations3plus_long/total_inf

    dat$popsumm$"Perc_4+_drug_muts_long"[popsumm_index]<- mutations4plus_long/total_inf

    dat$popsumm$Perc_5_drug_muts_long[popsumm_index]<- mutations5_long/total_inf
  }#end of aim3 summary stats
  
  ####################################
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
    temp_table=table(dat$pop$att1[alive_index])
    #how many alive and infected agents in each category
    temp_table2=table(dat$pop$att1[inf_index])
    # How many vaccinated in each category
    temp_table3 = table(dat$pop$att1[dat$pop$vaccinated == 1])
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
    for(zz in 1:length(temp_match)){
      namevec <- paste("generic_att_mean_degree_cat_",temp_match[zz],sep="")
       risk_group <- which(dat$pop$att1 == temp_match[zz] & dat$pop$Status >=0)
       edges <-  edges_by_agent[dat$attr$id %in% risk_group]
       tot_grp <- length(risk_group)
       if(tot_grp>1){
       mean_degree_group <- sum(edges)/tot_grp
       }else{mean_degree_group <- NA}
       dat$popsumm[[namevec]][popsumm_index]=mean_degree_group
    }
    for(zz in 1:length(temp_match)){
      namevec <- paste("prevalence_attr_",temp_match[zz],sep="")
      risk_group <- which(dat$pop$att1 == temp_match[zz] & dat$pop$Status >=0)
      tot_grp <- length(risk_group)
      inf_grp <- which(dat$pop$Status == 1 & dat$pop$att1 == temp_match[zz])
      if(tot_grp>1){
        prev_group   <- length(inf_grp)/length(risk_group)
      }else{prev_group <- NA}
      dat$popsumm[[namevec]][popsumm_index]=prev_group
    }
    for(zz in 1:length(temp_match)){
      namevec <- paste("new_infections_attr_",temp_match[zz],sep="")
      risk_group <- which(dat$pop$att1 == temp_match[zz] & dat$pop$Status >=0)
      time_inf_group <- ((dat$pop$att1 == temp_match[zz]) * dat$pop$Time_Inf)
      time_inf_group[time_inf_group == 0] <- NA
      tot_grp <- length(risk_group)
      if(tot_grp>1){
        new_inf_group   <- length(which(is.element(time_inf_group, time_index)))
      }else{new_inf_group <- NA}
      dat$popsumm[[namevec]][popsumm_index]=new_inf_group
    }
    for(zz in 1:length(temp_match)){
      namevec <- paste("susceptibles_attr_",temp_match[zz],sep="")
      risk_group <- which(dat$pop$att1 == temp_match[zz] & dat$pop$Status >=0)
      susceptible <- which(dat$pop$Status == 0 & dat$pop$att1 == temp_match[zz])
      tot_grp <- length(risk_group)
      if(tot_grp>1){
        sus_group   <- length(susceptible) 
      }else{sus_group <- NA}
      dat$popsumm[[namevec]][popsumm_index]=sus_group
    }
    for(zz in 1:length(temp_match)){
      namevec <- paste("mean_spvl_incident_attr_",temp_match[zz],sep="")
      risk_group <- which(dat$pop$att1 == temp_match[zz] & dat$pop$Status >=0)
      tot_grp <- length(risk_group)
      time_inf_group <- ((dat$pop$att1 == temp_match[zz]) * dat$pop$Time_Inf)
      time_inf_group[time_inf_group == 0] <- NA
      new_group_infections <- is.element(time_inf_group, time_index)
      if(tot_grp>1){
        mean_spvl_inf_group <- mean(dat$pop$LogSetPoint[new_group_infections])
      }else{mean_spvl_inf_group <- NA}
      dat$popsumm[[namevec]][popsumm_index]=mean_spvl_inf_group
    }
    for(zz in 1:length(temp_match)){
      namevec <- paste("prop_on_prep_attr_",temp_match[zz],sep="")
      risk_group <- which(dat$pop$att1 == temp_match[zz] & dat$pop$Status >=0)
      tot_grp <- length(risk_group)
      on_prep_group <- which(dat$pop$att1 == temp_match[zz] & dat$pop$Status >=0 & dat$pop$prep_list == 1)
      if(tot_grp>1){
        prep_group <- length(on_prep_group)/length(risk_group)
      }else{prep_group <- NA}
      dat$popsumm[[namevec]][popsumm_index]=prep_group
    }
    for(zz in 1:length(temp_match)){
      namevec <- paste("prop_eligible_prep_attr_",temp_match[zz],sep="")
      risk_group <- which(dat$pop$att1 == temp_match[zz] & dat$pop$Status >=0)
      eligible_prep_group <- which(dat$pop$att1 == temp_match[zz] & dat$pop$Status >=0 & dat$pop$prep_eligible_list == 1)
      tot_grp <- length(risk_group)
      if(tot_grp>1){
        prep_eligible_group <- length(eligible_prep_group)/length(risk_group)
      }else{prep_group <- NA}
      dat$popsumm[[namevec]][popsumm_index]=prep_eligible_group
    }
  }
  # end of calculating summary stats for generic attributes    
  #################################################
  return(dat)
}

