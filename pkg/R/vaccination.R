vaccination <- function(dat, at) {
  
  if(at < dat$param$start_vacc_campaign[1]) {return(dat)}
  
  # off/on for preventive vaccine
  if(at > dat$param$start_vacc_campaign[1] &  dat$param$preventative_campaign == T) {
    vacc_ix <- which(dat$pop$vaccinated == 1)
    dat$pop$vaccinated[vacc_ix] <- rbinom(length(vacc_ix), 1, 1 - (1/dat$param$vacc_eff_duration))
  }
  
  # off/on for multi efficacy vaccine model
  if(at > dat$param$start_vacc_campaign[1] &  dat$param$vacc_multi_eff == T) {
    vacc_ix <- which(dat$pop$vaccinated == 1)
    dat$pop$vaccinated[vacc_ix] <- rbinom(length(vacc_ix), 1, 1 - (1/dat$param$vacc_eff_duration))
  }
  
  #off/on for disease-modifying (therapeutic) vaccine
  #differs from others by resetting spvl/vl
  if(at > dat$param$start_vacc_campaign[1] &  dat$param$vacc_therapeutic_campaign==T) {
    vacc_ix <- which(dat$pop$vaccinated == 1)
    vacc_on_off_ix <- rbinom(length(vacc_ix), 1, 1 - (1/dat$param$vacc_eff_duration))
    dat$pop$vaccinated[vacc_ix] <- vacc_on_off_ix
    vacc_off_ix <- which(vacc_on_off_ix==0)
    
    #if any agents go off vaccine, revert to "genotypic" spvl (and for VL)
    if(length(vacc_off_ix)>0){
      #resetting spvl and vl
      vacc_inf_terminate_ix <- vacc_ix[vacc_off_ix]
      dat$pop$LogSetPoint[vacc_inf_terminate_ix] <- dat$pop$LogSetPoint_genotype[vacc_inf_terminate_ix]
      dat$pop$SetPoint[vacc_inf_terminate_ix] <- "^"(10.0,dat$pop$LogSetPoint_genotype[vacc_inf_terminate_ix])
      
      #temp qaqc check
      #if(any(is.na(dat$pop$LogSetPoint_genotype[vacc_terminate_ix]))){browser()}
      #if(any(is.na(dat$pop$LogSetPoint[vacc_terminate_ix]))){browser()}
    }
  }  
  
  
  #NOTE: need this right after off/on steps 
  if(!is.element(at,dat$param$start_vacc_campaign)) {return(dat)}
  
  # If vaccine is targeted to attribute groups
  if(dat$param$target_vacc_att) {
    for(i in 1:dat$param$generic_nodal_att_no_categories) {
      eligible_index <- which(dat$pop$Status == 0 & 
                                (dat$pop$vaccinated == 0 | is.na(dat$pop$vaccinated)) &
                                dat$pop$eligible_care == 1 & dat$pop$att1 == i) 
      
      if(length(eligible_index) == 0) {next}
      
      no_treated <- sum(rbinom(length(eligible_index), 1, dat$param$perc_vaccinated[i]))
      if(no_treated == 0) {next}
      
      treated_index <- sample(eligible_index, no_treated)
      
      dat$pop$vaccinated[treated_index] <- 1
      dat$pop$vacc_init_time[treated_index] <- at
    }
  } else {
    # Eligible_patients: eligible for care, not vaccinated, not infected
    eligible_index <- which(dat$pop$Status == 0 & 
                              (dat$pop$vaccinated == 0 | is.na(dat$pop$vaccinated)) &
                              dat$pop$eligible_care == 1) 
    
    if(length(eligible_index) == 0) {return(dat)}
    
    no_treated <- sum(rbinom(length(eligible_index), 1, dat$param$perc_vaccinated))
    
    if(no_treated == 0) {return(dat)}
    
    treated_index <- sample(eligible_index, no_treated)
    
    dat$pop$vaccinated[treated_index] <- 1
    dat$pop$vacc_init_time[treated_index] <- at
    if(dat$param$vacc_multi_eff){dat$pop$vacc_eff[treated_index] <- runif(length(treated_index))}
  }
  
  return(dat)
}
