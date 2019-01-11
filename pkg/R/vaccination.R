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
 vaccination <- function(dat, at) {
  
  if(at < dat$param$start_vacc_campaign[1]) {return(dat)}

   # off/on for preventive vaccine
  if(at > dat$param$start_vacc_campaign[1] &  dat$param$preventative_campaign == T) {
    vaccinated <- which(dat$pop$vaccinated == 1)
    dat$pop$vaccinated[vaccinated] <- rbinom(length(vaccinated), 1, 1 - (1/dat$param$vacc_eff_duration))
  }
   
   #off/on for disease-modifying (therapeutic) vaccine
   if(at > dat$param$start_vacc_campaign[1] &  dat$param$vacc_therapeutic_campaign==T) {
     vaccinated <- which(dat$pop$vaccinated == 1)
     #0, off vaccine; 1, stay on
     vacc_time = at - dat$pop$vacc_init_time #sets time a person is vaccinated for as current time minus time at which they were vaccinated  
     vacc_terminate_ix <- which(dat$pop$vaccinated == 1 & vacc_time > dat$param$vacc_eff_duration) #vector of people who are vaccinated and whose vacc_time exceeds efficacy duration
     dat$pop$vaccinated[vacc_terminate_ix] <- 0 #people whose vaccinated status reverts to zero because they've exceeded efficacy duration
    
     vacc_inf_terminate_ix <- which(dat$pop$vaccinated == 1 & 
                                    vacc_time > dat$param$vacc_eff_duration &
                                    dat$pop$Status==1) #vector of people who are vaccinated and whose vacc_time exceeds efficacy duration
     
     #if any agents go off vaccine, revert to "genotypic" spvl (and for VL)
     if(length(vacc_inf_terminate_ix)>0){
     #resetting spvl and vl 
     dat$pop$LogSetPoint[vacc_inf_terminate_ix] <- dat$pop$LogSetPoint_genotype[vacc_inf_terminate_ix]
     dat$pop$SetPoint[vacc_inf_terminate_ix] <- "^"(10.0,dat$pop$LogSetPoint_genotype[vacc_inf_terminate_ix])
     
     #temp qaqc check
     if(any(is.na(dat$pop$LogSetPoint_genotype[vacc_terminate_ix]))){browser()}
     if(any(is.na(dat$pop$LogSetPoint[vacc_terminate_ix]))){browser()}
    }
     
     
   }
   
  
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
  }

  return(dat)
}
