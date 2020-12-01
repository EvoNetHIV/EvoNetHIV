#' @title Initialize 'popsumm' data object
#'
#'
#' @param dat master data object
#' @param at timestep
#' @return 'dat' object with 'popsumm' data object appended.
#' @details
#' Subfunction in 'initialize_module'. Sets up popsumm list and fills in default value of NA or 0 for all elements; then fills in initial value; popsumm is list of various statistics calculated each timestep to describe network/population/epidemic.
#' @examples
#' dat <- initialize_popsumm_dynamics(dat,at=1)


#' @export
initialize_popsumm_dynamics <- function(dat,at)
{
  #Description:
  # Sets up “popsumm” list and fills in default value of NA or 0 for all elements 
  # Then fills in the initial value
  # Popsumm is list of various statistics calculated each timestep to describe network/population/epidemic
  # Inputs: param$initial_infected, param$initial_pop, pop$Time_Inf, pop$RandomTimeToAIDS,
  # pop$LogSetPoint, pop$V, pop$age, pop$id, pop$CD4
  # Outputs: dat$popsumm
  
  param <-  dat$param 
  pop   <-  dat$pop
  nw    <-  dat$nw
  
  #input_parameters_popsumm_stats() returns a vector of all statistics to be
  #calcuated at each time step
  popsumm_vars <- input_parameters_popsumm_stats()
  #create an empty list based on number of variables, fill in with NAs                                                          
  popsumm        <- vector('list', length(popsumm_vars))
  popsumm        <- lapply(popsumm,function(x){ rep(NA_real_,times=param$n_steps)})
  names(popsumm) <- popsumm_vars
  
  #fill in initial values (if NA not appropriate)
  popsumm$aids_deaths[1] <- 0
  popsumm$natural_deaths[1] <- 0
  popsumm$natural_deaths_infecteds[1] <- 0
  popsumm$natural_deaths_susceptibles[1] <- 0
  popsumm$births[1] <- 0
  popsumm$new_infections[1]   <- param$initial_infected
  popsumm$total_infections[1] <- param$initial_infected
  popsumm$susceptibles[1]     <- param$initial_pop-param$initial_infected
  popsumm$alive[1]            <- param$initial_pop
  popsumm$no_in_aids_gamma[1] <- length(which( (at-dat$attr$Time_Inf) >  dat$attr$RandomTimeToAIDS))
  popsumm$no_in_aids_cd4[1]   <- length(which(dat$attr$CD4 == 4))
  popsumm$aged_out[1] <- 0
  
  popsumm$diagnosed[1]  <- 0
  popsumm$no_treated[1]  <- 0
  
  index <- which(pop$Status == 1) 
  index_untreated <- which(pop$Status == 1 & pop$treated != 1) 
  
  popsumm$mean_spvl_pop_all[1]      <- mean(pop$LogSetPoint[index])
  popsumm$median_spvl_pop_all[1]    <- median(pop$LogSetPoint[index])
  popsumm$variance_spvl_pop_all[1]  <- var(pop$LogSetPoint[index])
  
  popsumm$mean_spvl_pop_untreated[1]      <- mean(pop$LogSetPoint[index_untreated])
  popsumm$median_spvl_pop_untreated[1]    <- median(pop$LogSetPoint[index_untreated])
  popsumm$variance_spvl_pop_untreated[1]  <- var(pop$LogSetPoint[index_untreated])
  
  popsumm$mean_spvl_incident[1]    <- popsumm$mean_spvl_pop_untreated[1]
  popsumm$median_spvl_incident[1]  <- popsumm$median_spvl_pop_untreated[1]
  popsumm$variance_spvl_incident[1] <- popsumm$variance_spvl_pop_untreated[1]
  
  popsumm$mean_vl_pop_untreated[1]     <- mean(log10(pop$V[index_untreated]))
  popsumm$median_vl_pop_untreated[1]   <- median(log10(pop$V[index_untreated]))
  popsumm$variance_vl_pop_untreated[1] <- var(log10(pop$V[index_untreated]))
  
  popsumm$mean_vl_pop_all[1]     <- mean(log10(pop$V[index]))
  popsumm$median_vl_pop_all[1]   <- median(log10(pop$V[index]))
  popsumm$variance_vl_pop_all[1] <- var(log10(pop$V[index]))
  
  popsumm$total_pills_taken[1]     <- 0# length(which(pop$treated == 1))

  popsumm$mean_age_infecteds[1]    <- mean(pop$age[index])
  popsumm$mean_age_incident[1]    <- mean(pop$age[index])
  popsumm$mean_age_susceptibles[1] <- mean(pop$age[pop$Status == 0])
  popsumm$mean_age_died_AIDS[1]    <- NA
  popsumm$mean_age_infected_died_natural[1]  <- NA
  popsumm$mean_age_susceptibles_died_natural[1] <- NA
  
  aa <- summary(nw~degree(0:1) + concurrent, at = at)
  popsumm$no_edges[1]   <- unname(summary(nw~edges,at=at)[1,1])
  popsumm$mean_degree[1]   <- popsumm$no_edges[1]*2/network::network.size(nw)
  
  total_nodes <- sum(aa[1,1]+aa[1,2]+aa[1,3])
  popsumm$no_nodes_degree_0[1]   <- aa[1,1]/total_nodes
  popsumm$no_nodes_degree_1[1]   <- aa[1,2]/total_nodes
  popsumm$no_nodes_concurrent[1] <- aa[1,3]/total_nodes
  
  dat$popsumm <- popsumm
  
  return(dat)
}
#######################################################################
