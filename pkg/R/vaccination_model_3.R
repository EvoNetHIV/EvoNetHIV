

update_mu.model_3 <- function(dat,at){
  if(at==2){
    #initial infecteds at model start
    inf_index <- which(dat$pop$Status==1)
    mu_values <- dat$pop$virus_sens_vacc[inf_index]
    invisible(lapply(1:length(inf_index),function(x) dat$vacc_model$agents[[inf_index[x]]]$mu <<- mu_values[x]))
    
  }else{
    #secondary infections from previous timestep
    inf_index <- which(dat$pop$Time_Inf ==(at-1) & dat$pop$Status==1)
    if(length(inf_index)>0){
      donor_index <- dat$pop$Donors_Index[inf_index]
      mu_values <- as.numeric(lapply(donor_index,function(x) dat$vacc_model$agents[[x]]$mu))
      invisible(lapply(1:length(inf_index),function(x) dat$vacc_model$agents[[inf_index[x]]]$mu <<- mu_values[x]))
    
    }
  }
  return( dat$vacc_model$agents )
}

#' @export
update_sigma.model_3 <- function(dat,at){
  if(at==2){
    #initial infecteds at model start
    inf_index <- which(dat$pop$Status==1)
    sigma_values <- 0
    invisible(lapply(1:length(inf_index),function(x) dat$vacc_model$agents[[inf_index[x]]]$sigma <<- sigma_values))
    
  }else{
    #secondary infections from previous timestep
    inf_index <- which(dat$pop$Time_Inf ==(at-1) )
    if(length(inf_index)>0){
      sigma_values <- 0
      invisible(lapply(1:length(inf_index),function(x) dat$vacc_model$agents[[inf_index[x]]]$sigma <<- sigma_values))
    }
  }
  return( dat$vacc_model$agents )
}

#' #' @export
#' initialize_phi.model_3 <- function(dat,at){
#'   
#'   #current phi values
#'   index <- 1:length(dat$vacc_model$agents)
#'   phi_values <- as.numeric(lapply(index,function(x) dat$vacc_model$agents[[x]]$phi))
#'   #for future phi_values <- get_values(index,"phi")
#'   
#'   
#'   #if designated vacc. level reached (percent of pop vaccianted), don't vacc anymore
#'   alive_index <-  dat$pop$Status>=0
#'   proportion_vacc <- length(which(phi_values== 1 & alive_index))/length(which(alive_index))
#'   if(proportion_vacc > dat$param$max_perc_vaccinated){return(dat)}
#'   
#'   vacc_rate <- (dat$param$max_perc_vaccinated*dat$param$initial_pop  )/(dat$param$vacc_rollout_dur)
#'   
#'   #temp qaqc
#'   vacc_rate <- (dat$param$max_perc_vaccinated*dat$param$initial_pop  )/(2*dat$param$vacc_rollout_dur)
#'   
#'   
#'   #if(at==(5*365)+20){browser()}
#'   
#'   #time_index <- (dat$param$start_vacc_campaign[1]+dat$param$vacc_rollout_dur) - at
#'   #if(time_index>0){
#'   #  vacc_rate <- (dat$param$max_perc_vaccinated*dat$param$initial_pop  )/(time_index)
#'     
#'   #}else{
#'   #  target <- dat$param$max_perc_vaccinated- proportion_vacc
#'   #  vacc_rate <- target*length(which( dat$pop$Status>=0 ))
#'   #}
#'   
#'   dat$param$vacc_per_day <- dat$param$vacc_per_day+vacc_rate
#'   
#'   #temp swith to 10 from 1 for qaqc 5/21/20
#'   if(dat$param$vacc_per_day>=1){
#'     no_vaccinated <- round(dat$param$vacc_per_day)
#'     dat$param$vacc_per_day <- 0
#'   }else{
#'     return(dat)
#'   }
#'   
#'   
#'   #never been vaccinated
#'   eligible_index1 <- which(dat$pop$Status == 0 & 
#'                              is.na(dat$pop$vaccinated) &
#'                              dat$pop$eligible_care == 1) 
#'   
#'   #previously vaccinated
#'   eligible_index2 <- which(dat$pop$Status == 0 & 
#'                              dat$pop$vaccinated == 0 &
#'                              (at-dat$pop$vacc_init_time) > dat$param$vacc_eff_duration &
#'                              dat$pop$eligible_care == 1) 
#'   
#'   eligible_index <- c(eligible_index1,eligible_index2)
#'   if(length(eligible_index) == 0) {return(dat)}  #if no agents are eligible
#'   
#'   
#'   if(no_vaccinated <length(eligible_index)){
#'     vaccinated_index <- sample(eligible_index, no_vaccinated)
#'   }else{
#'     vaccinated_index <- eligible_index
#'     #if the %coverage in total population alive exceeds #eligible, vaccinate all eligible
#'   }
#'   
#'   
#'   invisible(lapply(1:length(vaccinated_index),function(x) dat$vacc_model$agents[[vaccinated_index[x]]]$phi <<- 1 ))
#'   dat$pop$vacc_init_time[vaccinated_index] <- at
#'   #for graphing routines
#'   dat$pop$vaccinated[vaccinated_index] <- 1
#'   
#'   
#'   return(dat)
#'   
#' }


#' @export
initialize_phi.model_3 <- function(dat,at){
  
  
  #current phi values
  index <- 1:length(dat$vacc_model$agents)
  phi_values <- unlist(lapply(index,function(x) dat$vacc_model$agents[[x]]$phi))
  #for future phi_values <- get_values(index,"phi")
  
  
  #if designated vacc. level reached (percent of pop vaccianted), don't vacc anymore
  alive_index <-  dat$pop$Status>=0
  
  if(length(alive_index) != length(phi_values)){browser()}
  
  proportion_vacc <- length(which(phi_values>0  & alive_index))/length(which(alive_index))
  if(proportion_vacc > dat$param$max_perc_vaccinated){return(dat)}
  
  vacc_rate <- (dat$param$max_perc_vaccinated*dat$param$initial_pop  )/(dat$param$vacc_rollout_dur)
  
  #temp qaqc
  vacc_rate <- (dat$param$max_perc_vaccinated*dat$param$initial_pop  )/(2*dat$param$vacc_rollout_dur)
  
  
  #if(at==(5*365)+20){browser()}
  
  #time_index <- (dat$param$start_vacc_campaign[1]+dat$param$vacc_rollout_dur) - at
  #if(time_index>0){
  #  vacc_rate <- (dat$param$max_perc_vaccinated*dat$param$initial_pop  )/(time_index)
  
  #}else{
  #  target <- dat$param$max_perc_vaccinated- proportion_vacc
  #  vacc_rate <- target*length(which( dat$pop$Status>=0 ))
  #}
  
  dat$param$vacc_per_day <- dat$param$vacc_per_day+vacc_rate
  
  #temp swith to 10 from 1 for qaqc 5/21/20
  if(dat$param$vacc_per_day>=1){
    no_vaccinated <- round(dat$param$vacc_per_day)
    dat$param$vacc_per_day <- 0
  }else{
    return(dat)
  }
  
  
  
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
  
  
  if(no_vaccinated <length(eligible_index)){
    vaccinated_index <- sample(eligible_index, no_vaccinated)
  }else{
    vaccinated_index <- eligible_index
    #if the %coverage in total population alive exceeds #eligible, vaccinate all eligible
  }
  
  
  invisible(lapply(1:length(vaccinated_index),function(x) dat$vacc_model$agents[[vaccinated_index[x]]]$phi <<- dat$param$init_ab_conc ))
  dat$pop$vacc_init_time[vaccinated_index] <- at
  #for graphing routines
  dat$pop$vaccinated[vaccinated_index] <- 1
  dat$pop$no_ab_infusions[vaccinated_index] <- 1 
  
  return(dat)
  
}


#' @export
update_phi.model_3 <- function(dat,at){
  
  
  
  if(at > dat$param$start_vacc_campaign[1] ) {
  
  #if time elapsed is met for another infusion
  infusion_ix <- which((at-dat$pop$vacc_init_time)> dat$param$ab_infusion_interval &
                        dat$pop$no_ab_infusions < dat$param$ab_max_no_infusions)
  
  if(length(infusion_ix)>0){
    dat$pop$vacc_init_time[infusion_ix] <- at
    dat$pop$no_ab_infusions[infusion_ix] <- dat$pop$no_ab_infusions[infusion_ix]+1
    invisible(lapply(1:length(infusion_ix),function(x) dat$vacc_model$agents[[infusion_ix[x]]]$phi <<- dat$param$init_ab_conc ))
  }
  
  #otherwise, update antibody [ ] 
  update_ix <- which((at-dat$pop$vacc_init_time) < dat$param$ab_infusion_interval &
                         dat$pop$no_ab_infusions< dat$param$ab_max_no_infusions)
  if(length(update_ix)>0){
   phi_values <- unlist(lapply(update_ix,function(x) dat$vacc_model$agents[[x]]$phi))
   elapsed_time <- (at-dat$pop$vacc_init_time[update_ix])
   new_phi_values <- dat$param$init_ab_conc*(.5)^(elapsed_time/dat$param$ab_half_life)
   invisible(lapply(1:length(update_ix),function(x) dat$vacc_model$agents[[update_ix[x]]]$phi <<- new_phi_values[x] ))
  }
  
  }  

 return(dat)
}
  

#' @export
draw_m.model_3 <- function(dat,at,...){
  index <- dat$infector_id
  mu_values <- as.numeric(lapply(index,function(x) dat$vacc_model$agents[[x]]$mu))
  return(mu_values)
}


#' @export
calculate_theta.model_3 <- function(dat,m){

  theta <- rep(0,length(dat$susceptible_id))
  #of susceptibles, which are vaccinated
  phi_values <- as.numeric(lapply(dat$susceptible_id,function(x) dat$vacc_model$agents[[x]]$phi))
  vacc_ix <- which(phi_values>0 & m==1)
  
  if(length(vacc_ix)>0){
    trans_prob_decrease <- 1-exp(-(dat$param$ab_trans_rate *phi_values[vacc_ix]))
    theta[vacc_ix]  <- trans_prob_decrease
  }
  
  return(theta)
}

