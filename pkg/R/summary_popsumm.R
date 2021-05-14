summary_popsumm<-function(dat,at){

  
  #--------------------------------------------------------------
  #needs to happen each time-step
  #fill in necessary epimodel stats for resim_net and edges_correct fxns
  dat <- set_epi(dat,"s.num", at, length(which(dat$attr$Status==0)))
  dat <-  set_epi(dat,"i.num", at, length(which(dat$attr$Status==1)))
  dat <-  set_epi(dat,"num", at, length(which(dat$attr$Status>=0)))
  
  #----------------------------------------------------------------

  #----------------------------------------------------------------
  #calculation of summary stats: main section
  
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

  #"at" is an index for the "popsumm" vectors
  #based on value of "at" and paramter "popsumm_frequency"
  #if(at==1)
  #  at <- 1
  #else
   # if(dat$param$popsumm_frequency==1)
   #   at <- at
  #  else
   #   at <- (at/dat$param$popsumm_frequency)+1


    #logical vectors and indices helpful to calculate summary stats
    inf_index     <-  dat$attr$Status == 1
    total_inf     <- length(which(inf_index))
    sus_index     <-  dat$attr$Status == 0
    care_index    <-  dat$attr$eligible_care == 1
    alive_index   <-  inf_index | sus_index
    total_alive <- length(which(alive_index))
    not_treated_index <-  dat$attr$treated == 0 & inf_index
    
    #browser()
   
    new_infections <- is.element(dat$attr$Time_Inf, time_index)
    new_infections_count <- length(which(new_infections))
    

 if(new_infections_count>0){
   
  donor_time_inf <- dat$attr$Time_Inf[which(new_infections)]-dat$attr$Donors_Total_Time_Inf_At_Trans[which(new_infections)] 
  donor_time_inf_index <- which(donor_time_inf <= dat$param$t_acute)
  donor_acute_count <- length(donor_time_inf_index)  
  if(donor_acute_count>0){
    dat$epi$percent_donor_acute[at]<- donor_acute_count/new_infections_count
  }else{
    dat$epi$percent_donor_acute[at] <- 0
  }
 }else{
   dat$epi$percent_donor_acute[at] <- -999
 }
        
    cd4_aids <- dat$attr$CD4 == 4
    new_diagnoses <- dat$attr$diag_status == 1 &  is.element(dat$attr$diag_time,time_index)
    percent_virus_sensitive <- round(100*(length(which(dat$attr$virus_sens_vacc==1 & inf_index))/length(which(inf_index))))
    percentVaccinated <- round(100*(length(which(dat$attr$vaccinated >= 1 & alive_index))/total_alive))
    no_vaccinated <- length(which(dat$attr$vaccinated >= 1 & alive_index))
   
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
    
  
    #transmission probability
    if(!is.null(dat$discord_coital_df)){
      trans_probs_mean <- mean(dat$discord_coital_df$trans_probs)
    }else{
      trans_probs_mean <- NA
    }

  #---------------------------------------------
  # stats calculated for every run

  dat$epi$timestep[at]<- at
  dat$epi$prevalence[at]<-length(which(inf_index))/length(which(alive_index))
  dat$epi$new_infections[at]<-new_infections_count
  dat$epi$susceptibles[at]<-length(which(sus_index))
  dat$epi$total_infections_alive[at]<-length(which(inf_index))
  
  
  if(at==1){ #at model initialization, nothing has happened
    dat$epi$births[at]<- 0
    dat$epi$aids_deaths[at]<- 0
    dat$epi$natural_deaths[at]<-0
    dat$epi$aged_out[at]<- 0
  }else{
    dat$epi$births[at]<- dat$no_births
    dat$no_births <- 0 #reset counter
    dat$epi$aids_deaths[at]<- dat$no_deaths_aids
    dat$no_deaths_aids <- 0 #reset counter
    dat$epi$natural_deaths[at]<-dat$no_deaths_nonaids
    dat$no_deaths_nonaids <- 0 #reset counter
    dat$epi$aged_out[at]<- dat$no_aged_out
    dat$no_aged_out <- 0  #reset counter
  }
  
  
  #not really used anymore
  #dat$epi$natural_deaths_infecteds[at]<-length(which(died_non_aids_inf))
  #dat$epi$natural_deaths_susceptibles[at]<-length(which(died_non_aids_sus))
  
  dat$epi$alive[at]<-length(which(alive_index))

  dat$epi$no_in_aids_cd4[at]<-length(which(cd4_aids & inf_index ))

  dat$epi$new_diagnoses[at]<-length(which(new_diagnoses))
  
  out <- mean(dat$attr$Donors_Total_Time_Inf_At_Trans[which(new_infections)])
  dat$epi$mean_time_donor_infected_incident[at] <- ifelse(!is.na(out),out,-999)
  
  out <-  mean(dat$attr$age[which(new_infections)])
  dat$epi$mean_age_incident[at] <- ifelse(!is.na(out),out,-999)
  
  
  out <-mean(dat$attr$LogSetPoint[which(inf_index)])
  dat$epi$mean_spvl_pop_all[at]<- ifelse(!is.na(out),out,-999)
  
  out <- mean(log10_vl_values)
  dat$epi$mean_vl_pop_all[at]<-ifelse(!is.na(out),out,-999)
  
  out <-mean(dat$attr$LogSetPoint[which(new_infections)])
  dat$epi$mean_spvl_incident[at] <- ifelse(!is.na(out),out,-999)
  
  out <- mean(dat$attr$age[which(inf_index)])
  dat$epi$mean_age_infecteds[at]<- ifelse(!is.na(out),out,-999)
  
  out <- mean(dat$attr$age[which(sus_index)])
  dat$epi$mean_age_susceptibles[at]<- ifelse(!is.na(out),out,-999)

    
  dat$epi$mean_trans_prob[at]<- trans_probs_mean
  dat$epi$no_edges[at]<- number_edges
  dat$epi$mean_degree[at]<- number_edges*2/network_size
  
  
  #--------------------------------------------
  #network
  dat$epi$prop_nodes_degree_0[at]<- nw_summary[1]/total_nodes
  dat$epi$prop_nodes_degree_1[at]<- nw_summary[2]/total_nodes
  dat$epi$prop_nodes_concurrent[at]<- nw_summary[3]/total_nodes
  
  
  #not really used anymore
  #dat$epi$mean_degree_under_30[at]<- {
  #  if (length(agents_under30) > 0) sum(edges_under30)/length(agents_under30)
  #else NA}

  #dat$epi$mean_degree_30_50[at]<- {
  #if (length(agents_30to50) > 0)
  #sum(edges_30to50)/length(agents_30to50)
  #else NA}

  #dat$epi$mean_degree_over_50[at]<- {
  #if (length(agents_over50) > 0)
  #sum(edges_over50)/length(agents_over50)
  #else NA}

  #--------------------------------------------
  #hetero model
  if (dat$param$model_sex!="msm") {
    
    male_index    <- dat$attr$sex == 1 & dat$attr$Status >= 0
    female_index  <- dat$attr$sex == 0 & dat$attr$Status >= 0
    inf_male_index <- dat$attr$sex == 1 & inf_index
    inf_female_index <- dat$attr$sex == 0 & inf_index
    
    no_females_alive <- length(which(female_index & alive_index))
    no_males_alive <- length(which(male_index & alive_index))
   
    dat$epi$alive_female[at]<-no_females_alive
    dat$epi$alive_male[at]<-no_males_alive
    
    dat$epi$inf_men[at]<-length(which(inf_male_index))/length(which(male_index))
    dat$epi$inf_women[at]<-length(which(inf_female_index))/length(which(female_index))
    
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
    
    dat$epi$prev_15to24[at] <-prev_15to24
    dat$epi$prev_15to49[at] <-prev_15to49
    dat$epi$prev_f_15to24[at] <-prev_f_15to24
    dat$epi$prev_f_15to49[at] <-prev_f_15to49
    dat$epi$prev_m_15to24[at] <-prev_m_15to24
    dat$epi$prev_m_15to49[at] <-prev_m_15to49
    
    
    # todo: may be a faster way to calculate degree
    #edges_by_agent <- unname(summary(nw ~ sociality(base = 0),at=at))
    
    #female_edges <-  edges_by_agent[dat$attr$sex == 0]
    #tot_grp <- length(female_edges)
    #if(tot_grp>1){
    #  mean_degree_female <- sum(female_edges)/tot_grp
    #}else{mean_degree_female <- NA}
    #dat$epi$mean_degree_female[at]=mean_degree_female
    
    #male_edges <-  edges_by_agent[dat$attr$sex == 1]
    #tot_grp <- length(male_edges)
    #if(tot_grp>1){
    #  mean_degree_male <- sum(male_edges)/tot_grp
    #}else{mean_degree_male <- NA}
    #dat$epi$mean_degree_male[at]=mean_degree_male
    
    
  }

  
  #--------------------------------------------
  
  #treatment (msm and hetero models)
  if (dat$param$start_treatment_campaign[1] < 1e5) {
    
    treated_inf_male_index <- dat$attr$sex == 1 & inf_index  & dat$attr$treated == 1
    treated_inf_female_index <- dat$attr$sex == 0 & inf_index  & dat$attr$treated == 1
    
    
    treated_index <-  dat$attr$treated == 1 & inf_index
    treated_undetectable <- treated_index & dat$attr$V<dat$param$vl_undetectable
    treated_agents <- which(treated_index)
    not_treated_agents <- which(not_treated_index)
    
    
  dat$epi$no_treated[at]<-length(which(inf_index & treated_index))
  dat$epi$percent_suppressed[at]<-((length(which(treated_index &
                                        (log10(dat$attr$V)< dat$param$vl_full_supp ) )))/ length(which(inf_index)) )
  dat$epi$total_infections_not_treated[at] <-length(which(inf_index & not_treated_index))
  
  spvl_untreated_values <- (
    dat$attr$LogSetPoint[which(inf_index & not_treated_index)])
  
  out <- mean(spvl_untreated_values)
  dat$epi$mean_spvl_pop_untreated[at]<- ifelse(!is.na(out),out,-999)
  
  dat$epi$no_treated_undetectable[at]<-length(which(treated_undetectable))
  dat$epi$mean_vl_pop_untreated[at]<-mean(log10(dat$attr$V[which(inf_index &  not_treated_index)]))
  dat$epi$percent_treated_undetectable[at]<- length(which(treated_undetectable))/length(which(treated_index))
  
  
  #edges_untreated <- edges_by_agent[dat$attr$id %in% not_treated_agents ]
  #dat$epi$mean_degree_inf_untreated[at]<- sum(edges_untreated)/length(not_treated_agents)
  
  # Sex- and age-specific treatment coverage
  #cd4_elig   <- dat$attr$CD4 %in% dat$param$cd4_treatment_threshold | dat$attr$CD4_at_trtmnt %in% dat$param$cd4_treatment_threshold
  #dat$epi$cd4_gt_350[at] <- length(which(inf_index & !treated_index & (dat$attr$CD4 == 1 | dat$attr$CD4 == 2)))
  #dat$epi$cd4_200_350[at] <- length(which(inf_index & !treated_index & dat$attr$CD4 == 3))
  #dat$epi$cd4_0_200[at] <- length(which(inf_index & !treated_index & dat$attr$CD4 == 4))
  
  #dat$epi$total_pills_taken[at]<- sum(c(0,dat$epi$no_treated[1:at]),na.rm=T)
  
  }

  #--------------------------------------------

  #hetero model with treatment
  if (dat$param$model_sex!="msm" & dat$param$start_treatment_campaign[1] < 5e5) {  #only plot if treatment is available in model

    
    dat$epi$treated_inf_men[at]<-length(which(treated_inf_male_index))/length(which(inf_male_index))
    dat$epi$treated_inf_women[at]<-length(which(treated_inf_female_index))/length(which(inf_female_index))
    
    ###############################
    #These stats were calculated previously but now seldom used.
    #Can be uncommented if necessary.
    
    #under30_index <- dat$attr$age < 30 & dat$attr$Status >= 0
    #inf_under30_index <- dat$attr$age < 30 & inf_index
    #treated_inf_under30_index <- dat$attr$age < 30 & inf_index & dat$attr$treated == 1
    #agents30to50_index <- dat$attr$age >= 30 & dat$attr$age < 50 & dat$attr$Status >= 0 # Testing need to change 31 back to 50
    #inf_agents30to50_index <- dat$attr$age >= 30 & dat$attr$age < 50 & inf_index
    #treated_inf_agents30to50_index <- dat$attr$age >= 30 & dat$attr$age < 50 & inf_index & dat$attr$treated == 1
    #over50_index <- dat$attr$age >= 50 & dat$attr$Status >= 0
    #inf_over50_index <- dat$attr$age >= 50 & inf_index
    #treated_inf_over50_index <- dat$attr$age >= 50 & inf_index & dat$attr$treated == 1
    
    #agents_under30 <- which(under30_index)
    #agents_30to50 <- which(agents30to50_index)
    #agents_over50 <- which(over50_index)
    
    #dat$epi$inf_under30[at]<-length(which(inf_under30_index))/length(which(under30_index))
    #dat$epi$inf_30to50[at]<-length(which(inf_agents30to50_index))/length(which(agents30to50_index))
    #dat$epi$inf_over50[at]<-length(which(inf_over50_index))/length(which(over50_index))
    
    
    # Proportion coverage among those eligible by CD4 threshold
    #prop_trt_elig          <- length(which(inf_index & treated_index))/length(which(inf_index & cd4_elig))
    #prop_f_trt_elig        <- length(which(inf_female_index & treated_index))/length(which(inf_female_index & cd4_elig))
    #prop_m_trt_elig        <- length(which(male_index & inf_index & treated_index))/length(which(male_index & inf_index & cd4_elig))
    #prop_f_15to24_trt_elig <- length(which(inf_female_index & age_15to24 & treated_index))/
    #  length(which(inf_female_index & age_15to24 & cd4_elig))
    #prop_f_25to34_trt_elig <- length(which(inf_female_index & age_25to34 & treated_index))/
    #  length(which(inf_female_index & age_25to34 & cd4_elig))
    #prop_f_35plus_trt_elig <- length(which(inf_female_index & age_35plus & treated_index))/
    #  length(which(inf_female_index & age_35plus & cd4_elig))
    #prop_m_15to24_trt_elig <- length(which(inf_male_index & age_15to24 & treated_index))/
    #  length(which(inf_male_index & age_15to24 & cd4_elig))
    #prop_m_25to34_trt_elig <- length(which(inf_male_index & age_25to34 & treated_index))/
    #  length(which(inf_male_index & age_25to34 & cd4_elig))
    #prop_m_35plus_trt_elig <- length(which(inf_male_index & age_35plus & treated_index))/
    #  length(which(inf_male_index & age_35plus & cd4_elig))
    
    # Proportion coverage among all HIV+
    #prop_trt               <- length(which(inf_index & treated_index))/length(which(inf_index))
    #prop_f_trt             <- length(which(inf_female_index & treated_index))/length(which(inf_female_index))
    #prop_m_trt             <- length(which(inf_male_index & treated_index))/length(which(inf_male_index))
    #prop_f_15to24_trt      <- length(which(inf_female_index & age_15to24 & treated_index))/
    #  length(which(inf_female_index & age_15to24))
    #prop_f_25to34_trt      <- length(which(inf_female_index & age_25to34 & treated_index))/
    #  length(which(inf_female_index & age_25to34))
    #prop_f_35plus_trt      <- length(which(inf_female_index & age_35plus & treated_index))/
    #  length(which(inf_female_index & age_35plus))
    #prop_m_15to24_trt      <- length(which(inf_male_index & age_15to24 & treated_index))/
    #  length(which(inf_male_index & age_15to24))
    #prop_m_25to34_trt      <- length(which(inf_male_index & age_25to34 & treated_index))/
    #  length(which(inf_male_index & age_25to34))
    #prop_m_35plus_trt      <- length(which(inf_male_index & age_35plus & treated_index))/
    #  length(which(inf_male_index & age_35plus))
    
    
    #dat$epi$treated_inf_under30[at]<-length(which(treated_inf_under30_index))/length(which(inf_under30_index))
    #dat$epi$treated_inf_30to50[at]<-length(which(treated_inf_agents30to50_index))/length(which(inf_agents30to50_index))
    #dat$epi$treated_inf_over50[at]<-length(which(treated_inf_over50_index))/length(which(inf_over50_index))
    
    
 
    #edges_treated <- edges_by_agent[dat$attr$id %in%  treated_agents]
    #dat$epi$mean_degree_inf_treated[at]<- sum(edges_treated)/length(treated_agents)
  }

  #--------------------------------------------
  #prep
  if (dat$param$start_prep_campaign[1] < 5e5) {
    
    #prep
    prop_on_prep <- length(which(alive_index & dat$attr$prep_list == 1))/total_alive
    prop_eligible_prep <- length(which(alive_index & dat$attr$prep_eligible_list == 1))/total_alive
    
    dat$epi$prop_on_prep[at] <-prop_on_prep
    dat$epi$prop_eligible_prep[at] <-prop_eligible_prep
  }

  #--------------------------------------------

  #--------------------------------------------
  #vaccine preventative

  if (dat$param$preventative_campaign) {    #only plot if vaccine campaign in model
    new_infections_virus_vacc_sens_count <- length(which(is.element(dat$attr$Time_Inf, time_index)&
                                                           dat$attr$virus_sens_vacc==1))
    new_infections_virus_vacc_notsens_count <- length(which(is.element(dat$attr$Time_Inf, time_index)&
                                                              dat$attr$virus_sens_vacc==0))
    
    dat$epi$new_infections_vacc_sens_virus[at]<- new_infections_virus_vacc_sens_count
    dat$epi$new_infections_vacc_resist_virus[at]<-  new_infections_virus_vacc_notsens_count
    dat$epi$percent_virus_sensitive_vacc[at]<- percent_virus_sensitive
    dat$epi$no_vaccinated[at]<- no_vaccinated
  }
  
  
  
  #"new" vaccine model
  if(dat$param$vaccine_model){
    
    vacc_status <- unlist(lapply(dat$attr$phi,function(x) x[1]))
    no_vaccinated  <- length(which(vacc_status>0 & vacc_status< 1))
    dat$epi$no_vaccinated[at] <- no_vaccinated 
    #vaccine trial (component of "new vaccine model)
    if(dat$param$vaccine_trial){
      
      no_placebo <- length(which(vacc_status==2))
      dat$epi$no_placebo[at] <- no_placebo 
      
      dat$epi$new_infections_vacc[at] <- length(which( is.element(dat$attr$Time_Inf, time_index) & 
                                                         vacc_status==1))
      not_vacc_index <- (is.na(vacc_status) | vacc_status==0)
      dat$epi$new_infections_notvacc[at] <- length(which( is.element(dat$attr$Time_Inf, time_index) & not_vacc_index))
      dat$epi$new_infections_placebo[at] <- length(which( is.element(dat$attr$Time_Inf, time_index) & 
                                                            vacc_status==2))
    }
  } #end of "new" vaccine model
  #--------------------------------------------
  #disease modifying vaccine
  #Note 2/11/19: intrinsic (formerly called genotypic) = spvl assigned to agents regardless of vaccine status, one that is passed on to infectees, 
  #as opposed to phenotypic spvl, which is the expressed spvl that reflects modification by vaccine. Intrinsic and phenotypic are the same in unvaccinated agents
  if(dat$param$vacc_therapeutic_campaign){
    dat$epi$percentAliveVaccinated[at]<- percentVaccinated
    #prevalent spvl, maybe should remove, not too useful
    out<-mean(dat$attr$LogSetPoint_genotype[which(dat$attr$Status==1 &
                                                    dat$attr$vaccinated == 1 )],na.rm=T) 
    dat$epi$mean_spvl_genotype[at] <- ifelse(!is.na(out),out,-999)
    
    out <- mean(dat$attr$LogSetPoint[which( dat$attr$vaccinated == 0  &
                                              dat$attr$Status==1)],na.rm=T)                                                                                        
    dat$epi$mean_spvl_nonvacc[at] <-ifelse(!is.na(out),out,-999)
    
    out<-mean(dat$attr$LogSetPoint[which( is.na(dat$attr$vaccinated)&
                                            dat$attr$Status==1)],na.rm=T)
     dat$epi$mean_spvl_nevervacc[at] <- ifelse(!is.na(out),out,-999)
    
    
    new_infections_nonvacc <- which(dat$attr$Time_Inf %in% time_index &   #all new infections occuring in nonvaccinated agents during the popsumm frequency window (default 30 days)
                                      dat$attr$vaccinated == 0)
    new_infections_nevervacc <- which(dat$attr$Time_Inf %in% time_index &   #all new infections occuring in nonvaccinated agents during the popsumm frequency window (default 30 days)
                                      is.na(dat$attr$vaccinated))
    
    
    ispvl_nonvacc <- dat$attr$LogSetPoint[new_infections_nonvacc]   #spvls of newly infected nonvaccinees (phenotypic, which in nonvaccinated agents is same as intrinsic)
    ispvl_nevervacc <- dat$attr$LogSetPoint[new_infections_nevervacc]   #spvls of newly infected nonvaccinees (phenotypic, which in nonvaccinated agents is same as intrinsic)
    new_infections_vacc<-which(dat$attr$Time_Inf %in% time_index &    #all new infections occuring in vaccinated agents during the popsumm frequency window (default 30 days)
                             dat$attr$vaccinated==1)
    ispvl_vacc <- dat$attr$LogSetPoint_genotype[new_infections_vacc] #spvls of newly infected nonvaccinees (intrinsic)
    ispvl_all <- c(ispvl_nonvacc,ispvl_vacc,ispvl_nevervacc)  #all spvls of agents infected in this time window, in vaccinated and unvaccinated agents
    dat$epi$mean_ispvl_intrinsic_all[at]<-mean(ispvl_all) #mean of all spvls of agents infected during this time window, provides mean incident SPVL
    out<-mean(ispvl_nonvacc)
    dat$epi$mean_ispvl_intrinsic_nonvacc[at]<-ifelse(!is.na(out),out,-999) #mean of nonvaccinated spvls of agents infected during this time window, provides mean incident SPVL
    #mean of vaccinated spvls of agents infected during this time window, provides mean incident SPVL
    out<- mean(ispvl_vacc)
    dat$epi$mean_ispvl_instrinsic_vacc[at]<ifelse(!is.na(out),out,-999)  
    
    #never vaccinated
    eligible_index1 <- which(dat$attr$Status == 0 & 
                               is.na(dat$attr$vaccinated) &
                               dat$attr$eligible_care == 1) 
    
    #previously vaccinated
    eligible_index2 <- which(dat$attr$Status == 0 & 
                               dat$attr$vaccinated == 0 &
                               (at-dat$attr$vacc_init_time) > dat$param$vacc_eff_duration &
                               dat$attr$eligible_care == 1) 
    
    eligible_index <- c(eligible_index1,eligible_index2)
    dat$epi$perc_eligible_vacc[at] <- length(eligible_index)/total_alive
  }
  
  #--------------------------------------------
  
  #models with mean degree differing between 2 age groups (GF model)
  if(length(dat$param$age_nw_groups)>1){
    edges_by_agent <- suppressWarnings(unname(summary(nw ~ sociality(base = 0),at=at)))
    age_cats <- 1:length(dat$param$age_nw_groups)
    for(ii in 1:length(age_cats)){
      age1 <- dat$param$age_nw_groups[[ii]][1]
      age2 <- dat$param$age_nw_groups[[ii]][2]
      ix <- which(dat$attr$age > age1 & dat$attr$age < age2+1 & dat$attr$entrTime<at)
      edges <-  edges_by_agent[ix]
      mean_degree_group <- sum(edges)/length(ix)
      if(ii==1){dat$epi$mean_degree_high_risk[at] <- mean_degree_group}
      if(ii==2){dat$epi$mean_degree_low_risk[at] <- mean_degree_group}
    }
  }

  #--------------------------------------------
 
  
  #"aim3"
  if(dat$param$VL_Function=="aim3"){
    
    
    new_infections_virus_drug_sens_count <- length(which(is.element(dat$attr$Time_Inf, time_index)&
                                                           dat$attr$virus_sens_drug==1))
    new_infections_virus_drug_part_res_count <- length(which(is.element(dat$attr$Time_Inf, time_index)&
                                                               dat$attr$virus_part_res_drug==1))
    new_infections_virus_drug_3_plus_res_count <- length(which(is.element(dat$attr$Time_Inf, time_index) &
                                                                 dat$attr$virus_3_plus_drug_muts==1))
    new_infections_virus_1_drug_muts <- length(which(is.element(dat$attr$Time_Inf, time_index) &
                                                       dat$attr$virus_3_plus_drug_muts==1))
    
    
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
    

    dat$epi$total_new_infections[at]<- sum(c(0,dat$epi$new_infections[1:at]))

    dat$epi$new_infections_drug_sens_virus[at]<- new_infections_virus_drug_sens_count

    dat$epi$new_infections_drug_part_res_virus[at]<- new_infections_virus_drug_part_res_count

    dat$epi$new_infections_drug_3_plus_res_virus[at]<- new_infections_virus_drug_3_plus_res_count

    dat$epi$mean_PPP_incident[at]<- mean(dat$attr$PPP[which(new_infections)])

    dat$epi$mean_PPP_infected[at]<- mean(dat$attr$PPP[which(inf_index)])

    dat$epi$"drug_muts_1+"[at]<- mutations1

    dat$epi$"drug_muts_3+"[at]<- mutations1

    dat$epi$"total_1+_drug_muts"[at]<- {
      sum(c(0,dat$epi[["new_infections_virus_1_drug_muts"]][1:at]))}

    dat$epi$"total_3+_drug_muts"[at]<- {
      sum(c(0,dat$epi[["new_infections_virus_drug_3_plus_res_count"]][1:at]))}

    dat$epi$Perc_0_drug_muts[at]<- mutations0/no_inf_undect

    dat$epi$"Perc_1+_drug_muts"[at]<- mutations1/no_inf_undect

    dat$epi$"Perc_2+_drug_muts"[at]<- mutations2/no_inf_undect

    dat$epi$"Perc_3+_drug_muts"[at]<- mutations3/no_inf_undect

    dat$epi$"Perc_4+_drug_muts"[at]<- mutations4/no_inf_undect

    dat$epi$Perc_All_5_drug_muts[at]<- mutations5/no_inf_undect

    dat$epi$Perc_1_drug_muts[at]<- mutations1exact/no_inf_undect

    dat$epi$Perc_2_drug_muts[at]<- mutations2exact/no_inf_undect

    dat$epi$Perc_3_drug_muts[at]<- mutations3exact/no_inf_undect

    dat$epi$Perc_4_drug_muts[at]<- mutations4exact/no_inf_undect
    #not graphed/overlay  only
    dat$epi$Perc_1_drug_muts_total_pop[at]<- mutations1all/total_alive
    #not graphed/overlay  only
    dat$epi$Perc_2_drug_muts_total_pop[at]<- mutations2all/total_alive
    #not graphed/overlay  only
    dat$epi$Perc_3_drug_muts_total_pop[at]<- mutations3all/total_alive

    dat$epi$Perc_4_drug_muts_total_pop[at]<- mutations4all/total_alive

    dat$epi$Perc_0_drug_muts_total_pop[at]<- mutations0all/total_alive

    dat$epi$"Perc_1+_drug_muts_total_pop"[at]<- mutations1plusall/total_alive

    dat$epi$"Perc_2+_drug_muts_total_pop"[at]<- mutations2plusall/total_alive

    dat$epi$"Perc_3+_drug_muts_total_pop"[at]<- mutations3plusall/total_alive

    dat$epi$"Perc_4+_drug_muts_total_pop"[at]<- mutations4plusall/total_alive

    dat$epi$Perc_All_5_drug_muts_total_pop[at]<- mutations5all/total_alive

    dat$epi$"Perc_3+_drug_muts_long"[at]<- mutations3plus_long/total_inf

    dat$epi$"Perc_4+_drug_muts_long"[at]<- mutations4plus_long/total_inf

    dat$epi$Perc_5_drug_muts_long[at]<- mutations5_long/total_inf
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
    
    #used below
    edges_by_agent <- suppressWarnings(unname(summary(nw ~ sociality(base = 0),at=at)))
    
    
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
      dat$epi[[namevec]][at]=temp_table[zz]/sum_temp_table
    }
    for(zz in 1:temp_length){
      namevec2 <- paste("generic_att_percent_inf_cat_",zz,sep="")
      
      ix1<- which(names(temp_table)==zz)
      ix2<- which(names(temp_table2)==zz)
      if(length(ix2)>0){
        val<- temp_table2[ix2]/temp_table[ix1]  
      }else{val<-0}
      dat$epi[[namevec2]][at] <- val
    }
    for(zz in 1:temp_length) {
      namevec3 <- paste("generic_att_percent_vacc_cat_", zz, sep = "")
      ix1 <- which(names(temp_table) == zz)
      ix2 <- which(names(temp_table3) == zz)
      if(length(ix2) > 0) {
        val <- temp_table3[ix2]/temp_table[ix1]
      } else { val <- 0 }
      dat$epi[[namevec3]][at] <- val
    }
    for(zz in 1:length(temp_match)){
      namevec <- paste("generic_att_mean_degree_cat_",temp_match[zz],sep="")
       risk_group <- which(dat$attr$att1 == temp_match[zz] & dat$attr$Status >=0)
       edges <-  edges_by_agent[dat$attr$id %in% risk_group]
       tot_grp <- length(risk_group)
       if(tot_grp>1){
       mean_degree_group <- sum(edges)/tot_grp
       }else{mean_degree_group <- NA}
       dat$epi[[namevec]][at]=mean_degree_group
    }
    for(zz in 1:length(temp_match)){
      namevec <- paste("prevalence_attr_",temp_match[zz],sep="")
      risk_group <- which(dat$attr$att1 == temp_match[zz] & dat$attr$Status >=0)
      tot_grp <- length(risk_group)
      inf_grp <- which(dat$attr$Status == 1 & dat$attr$att1 == temp_match[zz])
      if(tot_grp>1){
        prev_group   <- length(inf_grp)/length(risk_group)
      }else{prev_group <- NA}
      dat$epi[[namevec]][at]=prev_group
    }
    for(zz in 1:length(temp_match)){
      namevec <- paste("new_infections_attr_",temp_match[zz],sep="")
      risk_group <- which(dat$attr$att1 == temp_match[zz] & dat$attr$Status >=0)
      time_inf_group <- ((dat$attr$att1 == temp_match[zz]) * dat$attr$Time_Inf)
      time_inf_group[time_inf_group == 0] <- NA
      tot_grp <- length(risk_group)
      if(tot_grp>1){
        new_inf_group   <- length(which(is.element(time_inf_group, time_index)))
      }else{new_inf_group <- NA}
      dat$epi[[namevec]][at]=new_inf_group
    }
    for(zz in 1:length(temp_match)){
      namevec <- paste("susceptibles_attr_",temp_match[zz],sep="")
      risk_group <- which(dat$attr$att1 == temp_match[zz] & dat$attr$Status >=0)
      susceptible <- which(dat$attr$Status == 0 & dat$attr$att1 == temp_match[zz])
      tot_grp <- length(risk_group)
      if(tot_grp>1){
        sus_group   <- length(susceptible) 
      }else{sus_group <- NA}
      dat$epi[[namevec]][at]=sus_group
    }
    for(zz in 1:length(temp_match)){
      namevec <- paste("mean_spvl_incident_attr_",temp_match[zz],sep="")
      risk_group <- which(dat$attr$att1 == temp_match[zz] & dat$attr$Status >=0)
      tot_grp <- length(risk_group)
      time_inf_group <- ((dat$attr$att1 == temp_match[zz]) * dat$attr$Time_Inf)
      time_inf_group[time_inf_group == 0] <- NA
      new_group_infections <- is.element(time_inf_group, time_index)
      if(tot_grp>1){
        mean_spvl_inf_group <- mean(dat$attr$LogSetPoint[new_group_infections])
      }else{mean_spvl_inf_group <- NA}
      dat$epi[[namevec]][at]=mean_spvl_inf_group
    }
    for(zz in 1:length(temp_match)){
      namevec <- paste("prop_on_prep_attr_",temp_match[zz],sep="")
      risk_group <- which(dat$attr$att1 == temp_match[zz] & dat$attr$Status >=0)
      tot_grp <- length(risk_group)
      on_prep_group <- which(dat$attr$att1 == temp_match[zz] & dat$attr$Status >=0 & dat$attr$prep_list == 1)
      if(tot_grp>1){
        prep_group <- length(on_prep_group)/length(risk_group)
      }else{prep_group <- NA}
      dat$epi[[namevec]][at]=prep_group
    }
    for(zz in 1:length(temp_match)){
      namevec <- paste("prop_eligible_prep_attr_",temp_match[zz],sep="")
      risk_group <- which(dat$attr$att1 == temp_match[zz] & dat$attr$Status >=0)
      eligible_prep_group <- which(dat$attr$att1 == temp_match[zz] & dat$attr$Status >=0 & dat$attr$prep_eligible_list == 1)
      tot_grp <- length(risk_group)
      if(tot_grp>1){
        prep_eligible_group <- length(eligible_prep_group)/length(risk_group)
      }else{prep_group <- NA}
      dat$epi[[namevec]][at]=prep_eligible_group
    }
  }
  # end of calculating summary stats for generic attributes    
  #################################################
  return(dat)
}

