

update_mu.model_1 <- function(dat,at){
  if(at==2){
    #initial infecteds at model start
    inf_index <- which(dat$pop$Status==1)
    mu_values <- dat$pop$virus_sens_vacc[inf_index]
    invisible(lapply(1:length(inf_index),function(x) dat$vacc_model$agents[[inf_index[x]]]$mu <<- mu_values[x]))
    
  }else{
    #secondary infections from previous timestep
    inf_index <- which(dat$pop$Time_Inf ==(at-1))
    if(length(inf_index)>0){
      donor_index <- dat$pop$Donors_Index[inf_index]
     tmp1=try( mu_values <- as.numeric(lapply(donor_index,function(x) dat$vacc_model$agents[[x]]$mu)))
     if(class(tmp1)=="try-error"){browser()}
      if(max(inf_index)>length( dat$vacc_model$agents)){browser()}
     tmp2= try(        invisible(lapply(1:length(inf_index),function(x) dat$vacc_model$agents[[inf_index[x]]]$mu <<- mu_values[x])))
     if(class(tmp2)=="try-error"){browser()}
    }
  }
  return( dat$vacc_model$agents )
}

#' @export
update_sigma.model_1 <- function(dat,at){
  if(at==2){
    #initial infecteds at model start
    inf_index <- which(dat$pop$Status==1)
    sigma_values <- 0
    invisible(lapply(1:length(inf_index),function(x) dat$vacc_model$agents[[inf_index[x]]]$sigma <<- sigma_values))
    
  }else{
    #secondary infections from previous timestep
    inf_index <- which(dat$pop$Time_Inf ==(at-1))
    if(length(inf_index)>0){
      sigma_values <- 0
      invisible(lapply(1:length(inf_index),function(x) dat$vacc_model$agents[[inf_index[x]]]$sigma <<- sigma_values))
    }
  }
  return( dat$vacc_model$agents )
}

#' @export
initialize_phi.model_1 <- function(dat,at){
  
  #current phi values
   index <- 1:length(dat$vacc_model$agents)
   phi_values <- as.numeric(lapply(index,function(x) dat$vacc_model$agents[[x]]$phi))
   
  #if designated vacc. level already reached (percent of pop vaccianted), don't vacc anymore
  if(length(which(phi_values == 1 & dat$pop$Status>=0))/length(which(dat$pop$Status>=0)) > dat$param$max_perc_vaccinated){return(dat)}
  
  # Identify eligible_patients: eligible for care, not vaccinated, not infected
  # by default, all agents eligible for care, unless specified otw
  #note:dat$pop$phi == 0 is an agent whose vaccine effect ended (waned)
  
  #never been vaccinated
  eligible_index1 <- which(dat$pop$Status == 0 &
                             is.na(phi_values) &
                             dat$pop$eligible_care == 1)
  
  #previously vaccinated
  eligible_index2 <- which(dat$pop$Status == 0 &
                             phi_values == 0 &
                             (at-dat$pop$vacc_init_time) > dat$param$vacc_eff_duration &
                             dat$pop$eligible_care == 1)
  
  eligible_index <- c(eligible_index1,eligible_index2)
  
  #if no agents eligible, end fxn
  if(length(eligible_index)==0){return(dat)}
  
  #calculate how many agents can be vaccinated, based on user-specified vaccination rate
  no_vaccinated <- sum(rbinom(length(which(dat$pop$Status>=0)), 1, dat$param$perc_vaccinated_rate)) #denominator is total population alive 
  if(no_vaccinated == 0) {return(dat)}
  
  #if number of eligible agents exceeds number permissible, randomly choose subset
  if(no_vaccinated <length(eligible_index)){
    vaccinated_index <- sample(eligible_index, no_vaccinated)
  }else{
    vaccinated_index <- eligible_index
    #if the %coverage in total population alive exceeds #eligible, vaccinate all eligible
  }
  
  invisible(lapply(1:length(vaccinated_index),function(x) dat$vacc_model$agents[[vaccinated_index[x]]]$phi <<- 1 ))
  dat$pop$vacc_init_time[vaccinated_index] <- at
  
  return(dat)
  
}

#' @export
update_phi.model_1 <- function(dat,at){
  # off/on for already vaccinated
  if(at > dat$param$start_vacc_campaign[1] ) {
    index <- 1:length(dat$vacc_model$agents)
    phi_values <- as.numeric(lapply(index,function(x) dat$vacc_model$agents[[x]]$phi))
    vacc_index <- which(phi_values == 1)
    if(length(vacc_index)>0){
      new_values <-  rbinom(length(vacc_index), 1, 1 - (1/dat$param$vacc_eff_duration))
      invisible(lapply(1:length(vacc_index),function(x) dat$vacc_model$agents[[vacc_index[x]]]$phi <<- new_values[x]))
    }
  }
  return(dat)
}

#' @export
draw_m.model_1 <- function(dat,at,...){
  index <- dat$infector_id
  mu_values <- as.numeric(lapply(index,function(x) dat$vacc_model$agents[[x]]$mu))
  return(mu_values)
}


#' @export
calculate_theta.model_1 <- function(dat,m){

  theta <- rep(0,length(dat$susceptible_id))
  #of susceptibles, which are vaccinated
  phi_values <- as.numeric(lapply(dat$susceptible_id,function(x) dat$vacc_model$agents[[x]]$phi))
  index <- which(phi_values==1 & m==1)
  if(length(index)>0){
    #theta[index]  <- phi_values[index]*m[index]*dat$param$vacc_trans_prob_decrease
    theta[index]  <- dat$param$vacc_trans_prob_decrease
  }
  return(theta)
}

