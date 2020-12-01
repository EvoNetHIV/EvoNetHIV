#' @export
social_treatment_vaccination_waning <- function(dat, at) {
  
  if(!(at %in% dat$param$start_vacc_campaign)) { return(dat) }
  
  ## Identify individuals eligible for vaccination (eligible for care, not vaccinated, not infected)
  ## In this function, individuals are 
  eligible_index <- which(dat$attr$Status == 0 & 
                            (dat$attr$vaccinated == 0 | is.na(dat$attr$vaccinated)) &
                            dat$attr$eligible_care == 1) 
  
  n_vaccinated <- sum(rbinom(length(eligible_index), 1, dat$param$perc_vaccinated)) # Because coverage is achieved by the prob_care parameter, user specifies perc_vaccinated to be 1/time to achieve coverage.
  
  if(n_vaccinated != 0) {
    vacc_index <- sample(eligible_index, n_vaccinated)
    dat$attr$vaccinated[vacc_index] <- 1
    dat$attr$vacc_init_time[vacc_index] <- at
  }
  
  ## If agent has exited simulation, set vaccination status to 0
  dat$attr$vaccinated[dat$attr$Status < 0] <- 0
  
  vaccinated <- which(dat$attr$vaccinated == 1)
  
  ## If time since previous vaccination is >= vacc_eff_duration and agent is alive and HIV-negative, give booster (i.e., reset vacc_init_time to at). If agent acquired HIV, set vaccination status to 0.
  dat$attr$vacc_init_time[((at - dat$attr$vacc_init_time) >= dat$param$vacc_eff_duration) & dat$attr$Status == 0 & !is.na(dat$attr$vacc_init_time)] <- at
  dat$attr$vaccinated[((at - dat$attr$vacc_init_time) >= dat$param$vacc_eff_duration) & dat$attr$Status == 1 & !is.na(dat$attr$vacc_init_time)] <- 0
  
  ## Update individual relative risk per waning function
  dat$attr$vacc_rr[vaccinated] <- 1 - ((dat$param$ve_24_months + 0.1999) - (1 - exp(-0.0003056761 * (at - dat$attr$vacc_init_time[vaccinated]))))
  
  if(dat$param$risk_comp_degree) {
    ## Set vaccination/risk compensation status on the network
    vacc_rc <- which((dat$attr$vaccinated == 1 & dat$attr$Status == 0) |
                       (dat$attr$vaccinated == 1 & dat$attr$Status == 1 & is.na(dat$attr$diag_status)))
    m <- match(vacc_rc, dat$attr$id)
    dat$attr$vacc_rc[m] <- 1
  }
  
  return(dat)
  
}