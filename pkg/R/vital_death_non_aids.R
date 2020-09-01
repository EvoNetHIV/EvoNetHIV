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
  # Inputs: dat$attr$active, dat$pop$Status, dat$pop$age, dat$param$mort_per_timestep, 
  # Outputs: dat$popsumm$natural_deaths, dat$pop$Status, dat$attr$status_evo,
  # dat$attr$active, dat$pop$Time_Death
  #######################################
  
  active <- which(dat$attr$active == 1)
  active_evo <- which(dat$pop$Status>=0)
  #qaqc
  if(length(active)!=length(active_evo)){browser()}
  
  #get ages
  ages <- trunc(dat$pop$age[active_evo])
  ix_ages <- match(ages,dat$param$min_age:(dat$param$max_age-1))
  ix_female <- which(dat$pop$sex[active_evo]=="f")
  ix_male <- which(dat$pop$sex[active_evo]=="m")
  death_vec <- numeric(length(active_evo))
  if(dat$param$model_sex=="msm"){
    death_vec<- dat$param$mort_per_timestep_male[ix_ages]
  }else{
    death_vec[ix_male]<- dat$param$mort_per_timestep_male[ix_ages[ix_male]]
    death_vec[ix_female]<- dat$param$mort_per_timestep_female[ix_ages[ix_female]]
  }
  
  #index of agents: total natural deaths: daily death rates plus beyond model age range
  nat_deaths <- which(runif(length(active)) <= death_vec )
  
  # evonet, epimodel/network bookkeeping
  if(length(nat_deaths)>0) {
    
    ix1 <- active_evo[nat_deaths]
    
    dat$pop$Status[active_evo][nat_deaths] <- (-1)
    dat$attr$status_evo[active][nat_deaths] <- (-1)
    dat$attr$active[active][nat_deaths] <- 0
    dat$attr$exitTime[active][nat_deaths] <- at
    dat$pop$Time_Death[active_evo][nat_deaths] <- at
    
    #modify network
    #dat <- terminate_vertices(dat = dat,at = at,
    #                          vids.to.terminate = active[nat_deaths])
    
    #end of bookkeeeping if natural deaths occur
  }
  
  
  return(dat)
}