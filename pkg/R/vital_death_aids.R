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
#' 
#' @export
vital_death_aids <-function(dat,at)
{
  #Description: identifies agents that have died of aids based on arguments to param$aids_death_model: 
 
  # 1) “gamma death”: pop$RandomTimeToAids + +dat$param$time_in_aids where param$time_in_aids 
        # is user specified value (default is 720); 
  
      cond1_gamma_death <- dat$param$aids_death_model=="Gamma_Death"

  # 2) “daily probability”: daily probability of death as function of viral load; 
  
       cond2_daily_prob_dying <- dat$param$aids_death_model=="daily_prob"
  
  # 3) CD4: after agents exit CD4 category 4, they are dead (4 represents aids).
     
       cond3_cd4_determined_death <- dat$param$aids_death_model=="cd4"
#---------------------------------
     
  if (cond1_gamma_death) {  
    
   infected<- dat$attr$Status == 1 & dat$attr$treated !=1
    post_acute <- ( at-dat$attr$Time_Inf ) > dat$param$t_acute
     positive_vl <- dat$attr$V > 0.0
    
    gamma_dead <- ( at - dat$attr$Time_Inf ) > ( dat$attr$RandomTimeToAIDS +dat$param$time_in_aids )
    # Notes: 
    #  1. Under this condition, persons who have been infected for more than "RandomTimeToAIDS" + "t_time_in_aids" will die
    #  2. We need to revisit the last term.  Does this say that with AIDS live for a time that exactly equals the length of the acute phase?
    #  3. Note that RandomTimeToAIDS is set else (at the time the person is first infected--i.e., their fate is predetermined in this model)
    
    dead_agents <- which(List_Infected & gamma_dead & post_acute)
    # Notes:
    #   1. Extra filter to prevent acutely infected from dying of AIDS
    #   2. Also filter on infecteds in the odd circumstance that uninfecteds might have a defined RandomTimeToAIDS value (should be NA in this case)

  } # End condition 1 (Gamma time to death)
  
#---------------------------------------
  
  if (cond2_daily_prob_dying) { 
    
   infected<- dat$attr$Status == 1 & dat$attr$treated !=1
    post_acute <- ( at-dat$attr$Time_Inf ) > dat$param$t_acute
   pos_vl <- dat$attr$V > 0.0
    
    dead_agents <- which(List_Infected &pos_vl) # Starts with those at risk of dying
    
    # Probality of dying is a function of viral load and the timestep
    if (length(dead_agents) > 0) {
      yearly_rate <- ( dat$param$death_rate_constant *
                         "^"(log10(dat$attr$V[dead_agents]),
                             dat$param$death_rate_exponent) )
      
      # timestep_rate <- yearly_rate/365.0
      timestep_rate <- annual_prob_conversion(yearly_rate,dat$param$timesteps_per_year)
    }
    
    # Set rates to zero if there are no potentially dying persons (Is this necessary?)
    if (length(dead_agents) <= 0) { # Corrected by John 3/10/15 [Previous version had >0]
      yearly_rate = 0.
      timestep_rate = yearly_rate/365.0
    }
    
    randVec <- runif(length(which(dat$attr$Status==1))) # Random numbers for determing who dies
    dead_agents <- which(List_Infected  & post_acute & (randVec< timestep_rate)) # Final list of people who died
    
  } # End condition 2 (Daily prob of dying)
  
  if (cond3_cd4_determined_death) {  
    # This model categorizes persons into 5 CD4 levels, with 5 meaning dying of AIDS
    dead_agents <- which(dat$attr$CD4==5 & dat$attr$Status == 1)
  } # End condition 3 (CD4 death)
  
#---------------------------------------
    # Change status of persons who have died
      
    no_dead <- length(dead_agents)
       
    if (no_dead>0) {
      dat$attr$Status[dead_agents] <- (-2)
      dat$attr$active[dead_agents] <- 0
      dat$attr$exitTime [dead_agents] <- at
      dat$attr$Time_Death[dead_agents]<-at
      #update counter
      dat$no_deaths_aids <- dat$no_deaths_aids+no_dead 
    }
  
  return(dat)
}
