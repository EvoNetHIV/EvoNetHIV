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
vital_death_non_aids <- function(dat,at)
{
  #########################################  
  #Description: daily probability of dying for HIV- based on input ASMR values scaled to daily timesteps.
  #-- kills off people based on annual ASMR or those that have reached 
  #-- maximum model age
  #-- ASMR is scaled to timestep of model
  # Inputs: dat$attr$active, dat$attr$Status, dat$attr$age, dat$param$mort_per_timestep, 
  # Outputs: dat$popsumm$natural_deaths, dat$attr$Status, dat$attr$status_evo,
  # dat$attr$active, dat$attr$Time_Death
  #######################################
  

  active_evo <- which(dat$attr$Status>=0)

  #get ages
  ages <- trunc(dat$attr$age[active_evo])
  ix_ages <- match(ages,dat$param$min_age:(dat$param$max_age-1))
  ix_female <- which(dat$attr$sex[active_evo]==0)
  ix_male <- which(dat$attr$sex[active_evo]==1)
  death_vec <- numeric(length(active_evo))
  if(dat$param$model_sex=="msm"){
    death_vec<- dat$param$mort_per_timestep_male[ix_ages]
  }else{
    death_vec[ix_male]<- dat$param$mort_per_timestep_male[ix_ages[ix_male]]
    death_vec[ix_female]<- dat$param$mort_per_timestep_female[ix_ages[ix_female]]
  }
  
  #index of agents: total natural deaths: daily death rates plus beyond model age range
  nat_deaths <- which(runif(length(active_evo)) <= death_vec )
  no_dead <- length(nat_deaths)
  
  #  bookkeeping

  if(no_dead>0) {
    
    index <- active_evo[nat_deaths]
    
    dat$attr$Status[index] <- (-1)
    dat$attr$active[index] <- 0
    dat$attr$exitTime[index] <- at
    dat$attr$Time_Death[index] <- at
    #update counter
    dat$no_deaths_nonaids <- dat$no_deaths_nonaids + no_dead 
    
    #end of bookkeeeping if non-aids deaths occur
  }
  
  
  return(dat)
}
