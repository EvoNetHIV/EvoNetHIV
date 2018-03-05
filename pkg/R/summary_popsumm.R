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
    
    # Vectors to identify agents engaging in risk compensation in response to vaccination.
    vacc_rc <- (inf_index & dat$pop$vaccinated == 1 & is.na(dat$pop$diag_status)) | (sus_index & dat$pop$vaccinated == 1)

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
    
    # CD4 categories to be used in estimation of disability-adjusted life-years
    cd4_gt_350  <- sum(inf_index & !treated_index & (dat$pop$CD4 == 1 | dat$pop$CD4 == 2))
    cd4_200_350 <- sum(inf_index & !treated_index & dat$pop$CD4 == 3)
    cd4_0_200   <- sum(inf_index & !treated_index & dat$pop$CD4 == 4)

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
    new_infections_count <- length(which(is.element(dat$pop$Time_Inf, time_index)))
    
    new_infections_virus_vacc_sens_count <- length(which(is.element(dat$pop$Time_Inf, time_index) & dat$pop$virus_sens_vacc==1))
    new_infections_virus_vacc_notsens_count <- length(which(is.element(dat$pop$Time_Inf, time_index) & dat$pop$virus_sens_vacc==0))
    new_inf_vaccinated_count   <- length(which(is.element(dat$pop$Time_Inf, time_index) & dat$pop$vaccinated == 1))
    new_inf_unvaccinated_count <- length(which(is.element(dat$pop$Time_Inf, time_index) & dat$pop$vaccinated == 0))
    pt_vaccinated   <- length(which((is.element(dat$pop$Time_Inf, time_index) | dat$pop$Status == 0) & dat$pop$vaccinated == 1)) * dat$param$popsumm_freq
    pt_unvaccinated <- length(which((is.element(dat$pop$Time_Inf, time_index) | dat$pop$Status == 0) & dat$pop$vaccinated == 0)) * dat$param$popsumm_freq
    percent_virus_sensitive <- round(100*(length(which(dat$pop$virus_sens_vacc==1 & inf_index))/length(which(inf_index))))
    percentVaccinated <- round(100*(length(which(dat$pop$vaccinated == 1 & alive_index))/total_alive))
    new_vaccinations  <- sum(dat$pop$vacc_init_time %in% time_index)
    average_vacc_rr   <- mean(dat$pop$vacc_rr[dat$pop$vacc_rr != 1 & dat$pop$vacc_init_time >= (at - dat$param$vacc_eff_duration)])
    
    new_births <- is.element(dat$pop$arrival_time, time_index)
    cd4_aids <- dat$pop$CD4 == 4
    new_diagnoses <- dat$pop$diag_status == 1 &  is.element(dat$pop$diag_time,time_index)
    acute_phase_vec <- (at-dat$pop$Time_Inf)<dat$param$t_acute
    acute_phase <- !is.na(acute_phase_vec) & acute_phase_vec==T
    
    #deaths
    just_died <- is.element(dat$pop$Time_Death,time_index)
    died_aids <- dat$pop$Status == -2 & just_died
    died_aids_mean_age <- mean(dat$pop$age[died_aids])
    died_non_aids <- dat$pop$Status == -1 & just_died
    died_non_aids_inf <- died_non_aids & !is.na(dat$pop$V)
    died_non_aids_sus <- died_non_aids & is.na(dat$pop$V)
    aged_out <- (dat$pop$age>=dat$param$max_age) & just_died

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
    
    if(dat$param$risk_comp_degree) {
      edges_vacc_rc <- edges_by_agent[dat$attr$id %in% which(vacc_rc)]
      edges_no_rc   <- edges_by_agent[dat$attr$id %in% which(!vacc_rc)]
    }

    #aim3 mutations
    inf_undetect_ix <- (dat$pop$Status==1 & dat$pop$V> dat$param$vl_undetectable)
    no_inf_undect <- length(which(inf_undetect_ix))

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
  # dat$popsumm$percent_donor_acute[popsumm_index]<- donor_acute_count/length(which(new_infections))
  # dat$popsumm$mean_time_donor_infected_incident[popsumm_index] <- mean(dat$pop$Donors_Total_Time_Inf_At_Trans[which(new_infections)])
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

  # Treatment (msm and hetero models)
  dat$popsumm$no_treated[popsumm_index]<-length(which(inf_index & treated_index))
  dat$popsumm$percent_suppressed[popsumm_index]<-(length(which(treated_index &
                                                                 ((at-dat$pop$tx_init_time)>100) & (log10(dat$pop$V) < dat$pop$LogSetPoint/10) & length(inf_index))) / length(inf_index) )

  # Hetero model with treatment
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

  # Circumcision
  dat$popsumm$circum_prev[popsumm_index] <-circum_prev

  # Vaccine
  dat$popsumm$new_infections_vacc_sens_virus[popsumm_index]   <- new_infections_virus_vacc_sens_count
  dat$popsumm$new_infections_vacc_resist_virus[popsumm_index] <- new_infections_virus_vacc_notsens_count
  dat$popsumm$percent_virus_sensitive_vacc[popsumm_index]     <- percent_virus_sensitive
  dat$popsumm$percentAliveVaccinated[popsumm_index]           <- percentVaccinated
  dat$popsumm$total_vaccines_administered[popsumm_index]      <- new_vaccinations
  if(at %in% dat$param$start_vacc_campaign) {
    dat$popsumm$new_infections_vaccinated[popsumm_index]      <- new_inf_vaccinated_count
    dat$popsumm$new_infections_unvaccinated[popsumm_index]    <- new_inf_unvaccinated_count
    dat$popsumm$pt_vaccinated[popsumm_index]                  <- pt_vaccinated
    dat$popsumm$pt_unvaccinated[popsumm_index]                <- pt_unvaccinated
    dat$popsumm$average_vacc_rr[popsumm_index]                <- average_vacc_rr
  } else {
    dat$popsumm$new_infections_vaccinated[popsumm_index]      <- 0
    dat$popsumm$new_infections_unvaccinated[popsumm_index]    <- 0
    dat$popsumm$pt_vaccinated[popsumm_index]                  <- 0
    dat$popsumm$pt_unvaccinated[popsumm_index]                <- 0
    dat$popsumm$average_vacc_rr[popsumm_index]                <- NA
  }
  
  
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
    
  }
  # end of calculating summary stats for generic attributes    
  #################################################
  return(dat)
}

