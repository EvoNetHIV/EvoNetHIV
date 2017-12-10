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
  #   is user specified value (default is 720); 
  # 2) “daily probability”: daily probability of death as function of viral load; 
  # 3) CD4: after agents exit CD4 category 4, they are dead (4 represents aids).
  
  #from John, 3-10-2015
  #############################################################################################
  #### Different ways that the program can model progression to AIDS ##########################
  #############################################################################################
  
  Cond1_Gamma_Death <- dat$param$aids_death_model=="Gamma_Death"
  Cond2_Daily_Prob_Dying <- dat$param$aids_death_model=="daily_prob"
  Cond3_CD4_Determined_Death <- dat$param$aids_death_model=="cd4"
  #############################################################################################
  ### Create readable synonym for time (helpful for virologists not used to "at" for time) ####
  #############################################################################################
  
  timeIndex <- at
  
  #############################################################################################
  #### Persons with various viral load categories that are used by multiple conditions ########
  #############################################################################################
  
  List_Infected <- dat$pop$Status == 1 & dat$pop$treated !=1
  List_PostAcute <- ( timeIndex-dat$pop$Time_Inf ) > dat$param$t_acute
  List_ViralLoadGreaterThanZero <- dat$pop$V > 0.0
  
  
  
  ##########################################################################################################################
  ##### Condition 1: Time to death determined by setpoint viral load (with time to AIDS set from a gamma distribution) #####
  ##########################################################################################################################
  
  if (Cond1_Gamma_Death) {  
    
    List_ProgressedToAIDS_UnderGammaDeath <- ( timeIndex - dat$pop$Time_Inf ) > ( dat$pop$RandomTimeToAIDS +dat$param$time_in_aids )
    # Notes: 
    #  1. Under this condition, persons who have been infected for more than "RandomTimeToAIDS" + "t_time_in_aids" will die
    #  2. We need to revisit the last term.  Does this say that with AIDS live for a time that exactly equals the length of the acute phase?
    #  3. Note that RandomTimeToAIDS is set else (at the time the person is first infected--i.e., their fate is predetermined in this model)
    
    List_Dying_Persons <- which(List_Infected & List_ProgressedToAIDS_UnderGammaDeath & List_PostAcute)
    # Notes:
    #   1. Extra filter to prevent acutely infected from dying of AIDS
    #   2. Also filter on infecteds in the odd circumstance that uninfecteds might have a defined RandomTimeToAIDS value (should be NA in this case)
    
    # Change status of persons who have died
    if (length(List_Dying_Persons) > 0) {
      dat$pop$Status[List_Dying_Persons] <- (-2)
      dat$attr$status_evo[match(List_Dying_Persons,dat$attr$id)] <- (-2)
      dat$attr$active[match(List_Dying_Persons,dat$attr$id)] <- 0
      dat$attr$exitTime[match(List_Dying_Persons,dat$attr$id)] <- at
      #dat$pop$V[List_Dying_Persons] <- 0.0
      dat$pop$Time_Death[List_Dying_Persons]<-timeIndex
      #dat$popsumm$aids_deaths[at] <-  length(List_Dying_Persons)
      #dat$popsumm$mean_age_died_AIDS[at] <- mean(dat$pop$age[List_Dying_Persons])
      
      dat <- EpiModel:::terminate_vertices(dat = dat,
                                           at = at,
                                           vids.to.terminate = which(is.element(dat$attr$id,List_Dying_Persons)))
      # End changing status of those who died
    }else{
      #dat$popsumm$aids_deaths[at]<- 0
    } 
  } # End condition 1 (Gamma time to death)
  
  
  #############################################################################################
  #####  Condition 2 : Daily prob of dying based on current viral load ########################
  #############################################################################################
  
  if (Cond2_Daily_Prob_Dying) { 
    
    List_Dying_Persons <- which(List_Infected & List_ViralLoadGreaterThanZero) # Starts with those at risk of dying
    
    # Probality of dying is a function of viral load and the timestep
    if (length(List_Dying_Persons) > 0) {
      yearly_rate <- ( dat$param$death_rate_constant *
                         "^"(log10(dat$pop$V[List_Dying_Persons]),
                             dat$param$death_rate_exponent) )
      
      # timestep_rate <- yearly_rate/365.0
      timestep_rate <- annual_prob_conversion(yearly_rate,dat$param$timesteps_per_year)
    }
    
    # Set rates to zero if there are no potentially dying persons (Is this necessary?)
    if (length(List_Dying_Persons) <= 0) { # Corrected by John 3/10/15 [Previous version had >0]
      yearly_rate = 0.
      timestep_rate = yearly_rate/365.0
    }
    
    randVec <- runif(length(which(dat$pop$Status==1))) # Random numbers for determing who dies
    List_Dying_Persons <- which(List_Infected  & List_PostAcute & (randVec< timestep_rate)) # Final list of people who died
    
    # Change status of persons who have died
    if (length(List_Dying_Persons) > 0) {
      dat$pop$Status[List_Dying_Persons] <- (-2)
      dat$attr$status_evo[active][which(is.element(inf,List_Dying_Persons))] <- (-2)
      dat$attr$active[active][which(is.element(inf,List_Dying_Persons))] <- 0
      dat$attr$exitTime[active][which(is.element(inf,List_Dying_Persons))] <- at
      #dat$pop$V[List_Dying_Persons] <- 0.0
      dat$pop$Time_Death[List_Dying_Persons]<-timeIndex
      
      #dat$popsumm$aids_deaths[at] <-  length(List_Dying_Persons)
      #dat$popsumm$mean_age_died_AIDS[at] <- mean(dat$pop$age[List_Dying_Persons])
      
      dat <- EpiModel:::terminate_vertices(dat = dat,
                                           at = at,
                                           vids.to.terminate = which(is.element(dat$attr$id,List_Dying_Persons)))
      # end changing status of those who died
    }else{
      #dat$popsumm$aids_deaths[at]<- 0
    }
  } # End condition 2 (Daily prob of dying)
  
  
  ####################################################################################  
  ##### Condition 3:  Death determined by CD4+ T-cell counts #########################
  ####################################################################################  
  
  if (Cond3_CD4_Determined_Death) {  
    
    # This model categorizes persons into 5 CD4 levels, with 5 meaning dying of AIDS
    List_Dying_Persons <- which(dat$pop$CD4==5 & dat$pop$Status == 1)
    
    # Change status of persons who have died
    if (length(List_Dying_Persons) > 0) {
      dat$pop$Status[List_Dying_Persons] <- (-2)
      dat$attr$status_evo[match(List_Dying_Persons,dat$attr$id)] <- (-2)
      dat$attr$active[match(List_Dying_Persons,dat$attr$id)] <- 0
      dat$attr$exitTime [match(List_Dying_Persons,dat$attr$id)] <- at
      #dat$pop$V[List_Dying_Persons] <- 0.0
      dat$pop$Time_Death[List_Dying_Persons]<-timeIndex
      #dat$popsumm$aids_deaths[at] <-  length(List_Dying_Persons)
      #dat$popsumm$mean_age_died_AIDS[at] <- mean(dat$pop$age[List_Dying_Persons])
      dat <- EpiModel:::terminate_vertices(dat = dat,
                                           at = at,
                                           vids.to.terminate = which(is.element(dat$attr$id,List_Dying_Persons)))
      # End changing status of those who died
    }
    
  } # End condition 3 (CD4 death)
  
  ####################################################
  ####  All done, return updated values of dat ######
  ####################################################
  
  return(dat)
}
