

update_mu.model_1b <- function(dat,at){
  if(at==2){
    #initial infecteds at model start
    inf_index <- which(dat$pop$Status==1)
    mu_values <- dat$pop$virus_sens_vacc[inf_index]
    #note 0 value indicates hasn't changed/mutated (2nd mu value)
    invisible(lapply(1:length(inf_index),function(x) dat$vacc_model$agents[[inf_index[x]]]$mu <<- c(mu_values[x],0)))
    
  }else{
    #Check to see if "mu" values change, then update if necessary
    #note: first value of "mu" is "mark" and 2nd value is whether it has already mutated (0=no,1=yes)
    #so for example, if agent 1 is infected but mu hasn't changed, dat$vacc_agents[[1]]$mu == c(1,0)
    inf_index <- which(dat$pop$Status==1)
    mu_values <- as.numeric(lapply(inf_index,function(x) dat$vacc_model$agents[[x]]$mu[1]))
    mutate_values <- as.numeric(lapply(inf_index,function(x) dat$vacc_model$agents[[x]]$mu[2]))
    non_mutate_index1 <- which(mutate_values==0 & mu_values==1)
    non_mutate_index2 <- which(mutate_values==0 & mu_values==0)
    #see if any mu values go from 1 ->0
    if(length(non_mutate_index1)>0){
      new_values <- rbinom(length(non_mutate_index1),1,1-dat$param$mu_daily_mutate_rate1)
      change_index1 <- which(new_values==0)
      if(length(change_index1)>0){
        final_index1 <- inf_index[non_mutate_index1[change_index1]]
        invisible(lapply(1:length(final_index1),function(x) dat$vacc_model$agents[[final_index1[x]]]$mu <<- c(0,1)))
      }
    }
    #see if any mu values go from 0 -> 1
    if(length(non_mutate_index2)>0){
      new_values2 <- rbinom(length(non_mutate_index2),1,dat$param$mu_daily_mutate_rate0)
      change_index2 <- which(new_values2==1)
      if(length(change_index2)>0){
        final_index2 <- inf_index[non_mutate_index2[change_index2]]
        invisible(lapply(1:length(final_index2),function(x) dat$vacc_model$agents[[final_index2[x]]]$mu <<- c(1,1)))
      }
    }
    
    #update mu values from NA for agents just infected in previous timestep
    #secondary infections from previous timestep
    inf_index <- which(dat$pop$Time_Inf ==(at-1) & dat$pop$Status==1)
    if(length(inf_index)>0){
      donor_index <- dat$pop$Donors_Index[inf_index]
      mu_values <- as.numeric(lapply(donor_index,function(x) dat$vacc_model$agents[[x]]$mu[1]))
      invisible(lapply(1:length(inf_index),function(x) dat$vacc_model$agents[[inf_index[x]]]$mu <<- c(mu_values[x],0)))
    }
  }
  return( dat$vacc_model$agents )
}

#' @export
update_sigma.model_1b <- function(dat,at){
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

#' @export
initialize_phi.model_1b <- function(dat,at){
  
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
update_phi.model_1b <- function(dat,at){
  # off/on for already vaccinated
  if(at > dat$param$start_vacc_campaign[1] ) {
    index <- 1:length(dat$vacc_model$agents)
    phi_values <- as.numeric(lapply(index,function(x) dat$vacc_model$agents[[x]]$phi))
    vacc_index <- which(phi_values == 1 & dat$pop$Status == 0)
    if(length(vacc_index)>0){
      new_values <-  rbinom(length(vacc_index), 1, 1 - (1/dat$param$vacc_eff_duration))
      invisible(lapply(1:length(vacc_index),function(x) dat$vacc_model$agents[[vacc_index[x]]]$phi <<- new_values[x]))
    }
  }
  return(dat)
}

#' @export
draw_m.model_1b <- function(dat,at,...){
  index <- dat$infector_id
  mu_values <- as.numeric(lapply(index,function(x) dat$vacc_model$agents[[x]]$mu[1]))
  return(mu_values)
}


#' @export
calculate_theta.model_1b <- function(dat,m){

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

