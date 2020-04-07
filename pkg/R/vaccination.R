vaccination <- function(dat, at) {
  
  if(at < dat$param$start_vacc_campaign[1]) {return(dat)}
  #temp qaqc
  #if(at==dat$param$start_vacc_campaign[1]){browser()}
  
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
    vacc_off_ix <- which(dat$pop$vaccinated == 1 & 
                        ((at - dat$pop$vacc_init_time)> dat$param$vacc_eff_duration ))
    
    
    #if any agents go off vaccine, revert to "genotypic" spvl (and for VL)
    if(length(vacc_off_ix)>0){
      
      #reset vaccination status
      dat$pop$vaccinated[vacc_off_ix] <- 0
      
      #resetting spvl and vl
      dat$pop$LogSetPoint[vacc_off_ix] <- dat$pop$LogSetPoint_genotype[vacc_off_ix]
      dat$pop$SetPoint[vacc_off_ix] <- "^"(10.0,dat$pop$LogSetPoint_genotype[vacc_off_ix])
      
      #temp qaqc check
      #if(any(is.na(dat$pop$LogSetPoint_genotype[vacc_terminate_ix]))){browser()}
      #if(any(is.na(dat$pop$LogSetPoint[vacc_terminate_ix]))){browser()}
    }
  }  
  
  #------------------------------------------------
  #NOTE: need this right after off/on steps 
  if(!is.element(at,dat$param$start_vacc_campaign)) {return(dat)}
  #------------------------------------------------
  
  # If vaccine is targeted to attribute groups
  if(dat$param$target_vacc_att) {
    for(i in 1:dat$param$generic_nodal_att_no_categories) {
      eligible_index <- which(dat$pop$Status == 0 & 
                                (dat$pop$vaccinated == 0 | is.na(dat$pop$vaccinated)) &
                                dat$pop$eligible_care == 1 & dat$pop$att1 == i) 
      
      if(length(eligible_index) == 0) {next}
      
      no_vaccinated <- sum(rbinom(length(eligible_index), 1, dat$param$perc_vaccinated[i])) #denominator is eligible people
      if(no_vaccinated == 0) {next}
      
      vaccinated_index <- sample(eligible_index, no_vaccinated)
      
      dat$pop$vaccinated[vaccinated_index] <- 1
      dat$pop$vacc_init_time[vaccinated_index] <- at
    }
  } else {
    
    #if designated vacc. level reached (percent of pop vaccianted), don't vacc anymore
    proportion_vacc <- length(which(dat$pop$vaccinated == 1 & dat$pop$Status>=0 ))/length(which(dat$pop$Status>=0))
                                                                       
    if(proportion_vacc > dat$param$max_perc_vaccinated){return(dat)}
    
    #dat$param$vacc_per_day <- dat$param$vacc_per_day+((dat$param$max_perc_vaccinated)*length(which(dat$pop$Status>=0)) )/(5*365)
     vacc_rate <- (dat$param$max_perc_vaccinated*dat$param$initial_pop  )/(5*365)
    
     time_index <- (dat$param$start_vacc_campaign[1]+dat$param$vacc_rollout_dur) - at
     if(time_index>0){
       vacc_rate <- (dat$param$max_perc_vaccinated*dat$param$initial_pop  )/(time_index)
       
     }else{
        target <- dat$param$max_perc_vaccinated- proportion_vacc
        vacc_rate <- target*length(which( dat$pop$Status>=0 ))
     }
     
    dat$param$vacc_per_day <- dat$param$vacc_per_day+vacc_rate
    
    if(dat$param$vacc_per_day>=1){
      no_vaccinated <- round(dat$param$vacc_per_day)
      dat$param$vacc_per_day <- 0
    }else{
      return(dat)
      }
    
    # Eligible_patients: eligible for care, not vaccinated, not infected
    #note:dat$pop$vaccinated == 0 is an agent whose vaccine effect ended (waned)

    
    #never been vaccinated
    eligible_index1 <- which(dat$pop$Status == 0 & 
                              is.na(dat$pop$vaccinated) &
                              dat$pop$eligible_care == 1) 
    
    #previously vaccinated
    eligible_index2 <- which(dat$pop$Status == 0 & 
                             dat$pop$vaccinated == 0 &
                             (at-dat$pop$vacc_init_time) > dat$param$vacc_eff_duration &
                              dat$pop$eligible_care == 1) 
    
    eligible_index <- c(eligible_index1,eligible_index2)
    
    
    
    if(length(eligible_index) == 0) {return(dat)}  #if no agents are eligible
    
    #default  
    #no_vaccinated <- sum(rbinom(length(which(dat$pop$Status>=0)), 1, dat$param$perc_vaccinated)) #denominator is total population alive 
    #test 3/27/20
    #no_vaccinated <- rbinom(1,length(which(dat$pop$Status>=0)), dat$param$perc_vaccinated) #denominator is total population alive 
    #no_vaccinated <- sum(rbinom(length(which(dat$pop$Status==0)), 1, dat$param$perc_vaccinated)) #denominator is total population alive 
    
    #test 3/27/20
    #if(at==1828){browser()}
    
    # if(dat$param$vacc_per_day<1){
    #    vacc_scalar <- round(1/dat$param$vacc_per_day)
    #    if((at%%vacc_scalar)==0){
    #      no_vaccinated <- 1
    #    }else{no_vaccinated <- 0}
    # }else{
    #   if((at%%2)==0){
    #     no_vaccinated <- floor(dat$param$vacc_per_day) 
    #   }else{
    #     no_vaccinated <- ceiling(dat$param$vacc_per_day) 
    #   }
    # }
    
    if(no_vaccinated == 0) {return(dat)}
    
    
    if(no_vaccinated <length(eligible_index)){
      vaccinated_index <- sample(eligible_index, no_vaccinated)
    }else{
      vaccinated_index <- eligible_index
      #if the %coverage in total population alive exceeds #eligible, vaccinate all eligible
    }
    
    dat$pop$vaccinated[vaccinated_index] <- 1
    dat$pop$vacc_init_time[vaccinated_index] <- at
    
  }
  
  return(dat)
}
