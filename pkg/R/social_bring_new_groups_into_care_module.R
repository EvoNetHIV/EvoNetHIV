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
social_bring_new_groups_into_care_module <- function(dat, at){   
 
  #Description: Makes new agents eligible for car based on a user-specified priority list (e.g., "high_risk", "under35") 
  #Inputs: 
    # dat$param$scale_up_type
    # Long list of agent-specific attributes (dat$pop$___'s vals) that could potentially be used as a basis for prioritizing people for testing and care
    #   Examples: att1 (risk group), CD4, STI, sex, age, total_acts, circum
  #Outputs: 
    # pop$eligible_care
 

  if(at < dat$param$start_scale_up_campaign){return(dat)}
  
  # scale_up_type is a list of people (e.g., "high_risk", "under35") that one might target for testing (where testing is implemented
  # by making additional people eligible for care)
  scale_up_strategy <- dat$param$scale_up_type[[1]]  # First convert into a non-subscripted list
  num_care_strategies <- length(scale_up_strategy)
  
  # Loop through the list of strategies in order
  for (j in 1:num_care_strategies) {
    
    not_curr_eligible <- which(dat$pop$eligible_care == 0 & dat$pop$Status >= 0)
    newly_eligible <- NULL
    
    if (scale_up_strategy[j] == "AIDS")     newly_eligible <- which( dat$pop$eligible_care == 0 & dat$pop$Status >= 0 & dat$pop$CD4 == 4) # assume AIDS is recognizable
    if (scale_up_strategy[j] == "CD4_under200")     newly_eligible <- which( dat$pop$eligible_care == 0 & dat$pop$Status >= 0 & dat$pop$CD4 == 4) # same as AIDS
    if (scale_up_strategy[j] == "CD4_under350")     newly_eligible <- which( dat$pop$eligible_care == 0 & dat$pop$Status >= 0 & dat$pop$CD4 >= 3) # assume rapid CD4 test
    if (scale_up_strategy[j] == "CD4_under500")     newly_eligible <- which( dat$pop$eligible_care == 0 & dat$pop$Status >= 0 & dat$pop$CD4 >= 2) # assume rapid CD4 test
    
    if (scale_up_strategy[j] == "highrisk" ) newly_eligible <- which(dat$pop$eligible_care == 0 & dat$pop$Status >= 0 & dat$pop$att1 >= dat$param$attr_treatment_threshold)
    if (scale_up_strategy[j] == "riskgroup_over2" ) newly_eligible <- which(dat$pop$eligible_care == 0 & dat$pop$Status >= 0 & dat$pop$att1 >= 2)
    
    if (scale_up_strategy[j] == "STIs")     newly_eligible <- which(dat$pop$eligible_care == 0 & dat$pop$Status >= 0 & dat$pop$sti_status == 1)
    
    if (scale_up_strategy[j] == "circum")      newly_eligible <- which(dat$pop$eligible_care == 0 & dat$pop$Status >= 0 & dat$pop$circum == 1)
    if (scale_up_strategy[j] == "not_circum")  newly_eligible <- which(dat$pop$eligible_care == 0 & dat$pop$Status >= 0 & dat$pop$circum == 0)
    
    if (scale_up_strategy[j] == "men")      newly_eligible <- which(dat$pop$eligible_care == 0 & dat$pop$Status >= 0 & dat$pop$sex == "m")
    if (scale_up_strategy[j] == "women")    newly_eligible <- which(dat$pop$eligible_care == 0 & dat$pop$Status >= 0 & dat$pop$sex == "f")
    
    if (scale_up_strategy[j] == "under50")  newly_eligible <- which(dat$pop$eligible_care == 0 & dat$pop$Status >= 0 & dat$pop$age <= 45)
    if (scale_up_strategy[j] == "under45")  newly_eligible <- which(dat$pop$eligible_care == 0 & dat$pop$Status >= 0 & dat$pop$age <= 45)
    if (scale_up_strategy[j] == "under40")  newly_eligible <- which(dat$pop$eligible_care == 0 & dat$pop$Status >= 0 & dat$pop$age <= 40)
    if (scale_up_strategy[j] == "under35")  newly_eligible <- which(dat$pop$eligible_care == 0 & dat$pop$Status >= 0 & dat$pop$age <= 35)
    if (scale_up_strategy[j] == "under30")  newly_eligible <- which(dat$pop$eligible_care == 0 & dat$pop$Status >= 0 & dat$pop$age <= 30)
    if (scale_up_strategy[j] == "under25")  newly_eligible <- which(dat$pop$eligible_care == 0 & dat$pop$Status >= 0 & dat$pop$age <= 25)
    
    if (scale_up_strategy[j] == "under_max_age_over_min_age") {  # Target people of intermediate ages
           newly_eligible <- which(dat$pop$eligible_care == 0 & dat$pop$Status >= 0 & dat$pop$age > dat$param$min_age_recruit_for_care & dat$pop$age <= dat$param$max_age_recruit_for_care)
           # Note: Although this is more general than "under50" etc., the fixed ones like "under50" are more useful for nested tests (so keep them)
    }
    
    if (scale_up_strategy[j] == "10acts")     newly_eligible <- which(dat$pop$eligible_care == 0 & dat$pop$Status >= 0 & dat$pop$total_acts >= 10)
    if (scale_up_strategy[j] == "25acts")     newly_eligible <- which(dat$pop$eligible_care == 0 & dat$pop$Status >= 0 & dat$pop$total_acts >= 25)
    if (scale_up_strategy[j] == "50acts")     newly_eligible <- which(dat$pop$eligible_care == 0 & dat$pop$Status >= 0 & dat$pop$total_acts >= 50)
    if (scale_up_strategy[j] == "100acts")    newly_eligible <- which(dat$pop$eligible_care == 0 & dat$pop$Status >= 0 & dat$pop$total_acts >= 100)
    if (scale_up_strategy[j] == "200acts")    newly_eligible <- which(dat$pop$eligible_care == 0 & dat$pop$Status >= 0 & dat$pop$total_acts >= 200)
    if (scale_up_strategy[j] == "400acts")    newly_eligible <- which(dat$pop$eligible_care == 0 & dat$pop$Status >= 0 & dat$pop$total_acts >= 400)
    if (scale_up_strategy[j] == "800acts")    newly_eligible <- which(dat$pop$eligible_care == 0 & dat$pop$Status >= 0 & dat$pop$total_acts >= 800)
  
    if (scale_up_strategy[j] == "men_under45")  newly_eligible <- which(dat$pop$eligible_care == 0 & dat$pop$Status >= 0 & dat$pop$sex == "m" & dat$pop$age <= 45)
    if (scale_up_strategy[j] == "men_under40")  newly_eligible <- which(dat$pop$eligible_care == 0 & dat$pop$Status >= 0 & dat$pop$sex == "m" & dat$pop$age <= 40)
    if (scale_up_strategy[j] == "men_under35")  newly_eligible <- which(dat$pop$eligible_care == 0 & dat$pop$Status >= 0 & dat$pop$sex == "m" & dat$pop$age <= 35)
    if (scale_up_strategy[j] == "men_under30")  newly_eligible <- which(dat$pop$eligible_care == 0 & dat$pop$Status >= 0 & dat$pop$sex == "m" & dat$pop$age <= 30)
    if (scale_up_strategy[j] == "men_under25")  newly_eligible <- which(dat$pop$eligible_care == 0 & dat$pop$Status >= 0 & dat$pop$sex == "m" & dat$pop$age <= 25)
    
    if (scale_up_strategy[j] == "women_under45")  newly_eligible <- which(dat$pop$eligible_care == 0 & dat$pop$Status >= 0 & dat$pop$sex == "f" & dat$pop$age <= 45)
    if (scale_up_strategy[j] == "women_under40")  newly_eligible <- which(dat$pop$eligible_care == 0 & dat$pop$Status >= 0 & dat$pop$sex == "f" & dat$pop$age <= 40)
    if (scale_up_strategy[j] == "women_under35")  newly_eligible <- which(dat$pop$eligible_care == 0 & dat$pop$Status >= 0 & dat$pop$sex == "f" & dat$pop$age <= 35)
    if (scale_up_strategy[j] == "women_under30")  newly_eligible <- which(dat$pop$eligible_care == 0 & dat$pop$Status >= 0 & dat$pop$sex == "f" & dat$pop$age <= 30)
    if (scale_up_strategy[j] == "women_under25")  newly_eligible <- which(dat$pop$eligible_care == 0 & dat$pop$Status >= 0 & dat$pop$sex == "f" & dat$pop$age <= 25)
    
    if (scale_up_strategy[j] == "random5perc")  {
      newly_eligible_temp <- which(dat$pop$eligible_care == 0 & dat$pop$Status >= 0)
      newly_eligible <- newly_eligible_temp[which(newly_eligible_temp %% 20 == 0)] # use agent id as basis for randomly selecting people for inclusion into care
    }

    if (scale_up_strategy[j] == "random10perc")  {
      newly_eligible_temp <- which(dat$pop$eligible_care == 0 & dat$pop$Status >= 0)
      newly_eligible <- newly_eligible_temp[which(newly_eligible_temp %% 10 == 0)] 
    }

    if (scale_up_strategy[j] == "random20perc")  {
      newly_eligible_temp <- which(dat$pop$eligible_care == 0 & dat$pop$Status >= 0)
      newly_eligible <- newly_eligible_temp[which(newly_eligible_temp %% 5 == 0)] 
    }
 
    #########################################################################################################################################
    ### The following are added to allow for tests of the sensitivity of network-based strategies to imperfections in testing strategies  ###
    ### Taken by themselves these are strange, but I think they are valuable for testing broader hypotheses about network-based stratgies  ###
    #########################################################################################################################################
    
    
    if (scale_up_strategy[j] == "50perc_under45")  {
      newly_eligible_temp <- which(dat$pop$eligible_care == 0 & dat$pop$Status >= 0 & dat$pop$age <= 45)
      newly_eligible <- newly_eligible_temp[which(newly_eligible_temp %% 2 != 0)] # select agents whose index isn't an even multiple of 5 (e.g., excludes 5, 10, 15, 20, 25, 30, 35,..)
      # excludes agents (2, 4, 6, 8, 10, 12, ...) note that 80,90, and 95% are not strict subsets of 50%
      # This approach assumes that agent indices are uncorrelated with important agent attributes
      # We know there will be some correlation; e.g., new entrants (= higher indices ) being younger
      # The assumption here that this correlation will be negligible.   
    } 
    if (scale_up_strategy[j] == "80_perc_under45")  {
      newly_eligible_temp <- which(dat$pop$eligible_care == 0 & dat$pop$Status >= 0 & dat$pop$age <= 45)
      newly_eligible <- newly_eligible_temp[which(newly_eligible_temp %% 4 != 0)] # select agents whose index isn't an even multiple of 5 (e.g., excludes 5, 10, 15, 20, 25, 30, 35,..)
      # Note that 90 and 95% are subsets of 80% and 95% is a subset of 90% but that 80, 90, and 95% are strict subsets of 50%
    }
    if (scale_up_strategy[j] == "90_perc_under45")  {
      newly_eligible_temp <- which(dat$pop$eligible_care == 0 & dat$pop$Status >= 0 & dat$pop$age <= 45)
      newly_eligible <- newly_eligible_temp[which(newly_eligible_temp %% 10 != 0)] #select agents whose index isn't an even multiple of 5 (e.g., excludes 5, 10, 15, 20, 25, 30, 35,..)
    } 
    if (scale_up_strategy[j] == "95_perc_under45")  {
      newly_eligible_temp <- which(dat$pop$eligible_care == 0 & dat$pop$Status >= 0 & dat$pop$age <= 45)
      newly_eligible <- newly_eligible_temp[which(newly_eligible_temp %% 20 != 0)] #select agents whose index isn't an even multiple of 5 (e.g., excludes 5, 10, 15, 20, 25, 30, 35,..)
    } 
    
    if (scale_up_strategy[j] == "50_perc_men")  {
      newly_eligible_temp <- which(dat$pop$eligible_care == 0 & dat$pop$Status >= 0 & dat$pop$sex == "m")
      newly_eligible <- newly_eligible_temp[which(newly_eligible_temp %% 2 != 0)] #select based on the index of the agent.  x%%2!=0 means agents with odd numbered indices 
    } 
    if (scale_up_strategy[j] == "80_perc_men")  {
      newly_eligible_temp <- which(dat$pop$eligible_care == 0 & dat$pop$Status >= 0 & dat$pop$sex == "m")
      newly_eligible <- newly_eligible_temp[which(newly_eligible_temp %% 4 != 0)] #select agents whose index isn't an even multiple of 5 (e.g., excludes 5, 10, 15, 20, 25, 30, 35,..)
     } 
    if (scale_up_strategy[j] == "90_perc_men")  {
      newly_eligible_temp <- which(dat$pop$eligible_care == 0 & dat$pop$Status >= 0 & dat$pop$sex == "m")
      newly_eligible <- newly_eligible_temp[which(newly_eligible_temp %% 10 != 0)] # select agents whose index isn't an even multiple of 10 (e.g., excludes 10, 20, 30, 40, 50, etc. )
    }
    if (scale_up_strategy[j] == "95_perc_men")  {
      newly_eligible_temp <- which(dat$pop$eligible_care == 0 & dat$pop$Status >= 0 & dat$pop$sex == "m")
      newly_eligible <- newly_eligible_temp[which(newly_eligible_temp %% 20 != 0)] #select agents whose index isn't an even multiple of 20 (e.g., excludes agents 20, 40, 60, etc. )
    } 
    if (scale_up_strategy[j] == "50_perc_women")  {
      newly_eligible_temp <- which(dat$pop$eligible_care == 0 & dat$pop$Status >= 0 & dat$pop$sex == "f")
      newly_eligible <- newly_eligible_temp[which(newly_eligible_temp %% 2 != 0)] #select based on the index of the agent.  x%%2!=0 means agents with odd numbered indices 
    } 
    if (scale_up_strategy[j] == "80_perc_women")  {
      newly_eligible_temp <- which(dat$pop$eligible_care == 0 & dat$pop$Status >= 0 & dat$pop$sex == "f")
      newly_eligible <- newly_eligible_temp[which(newly_eligible_temp %% 5 != 0)] #select agents whose index isn't an even multiple of 5
    } 
    if (scale_up_strategy[j] == "90_perc_women")  {
      newly_eligible_temp <- which(dat$pop$eligible_care == 0 & dat$pop$Status >= 0 & dat$pop$sex == "f")
      newly_eligible <- newly_eligible_temp[which(newly_eligible_temp %% 10 != 0)] #select agents whose index isn't an even multiple of 10
    } 
    if (scale_up_strategy[j] == "95_perc_women")  {
      newly_eligible_temp <- which(dat$pop$eligible_care == 0 & dat$pop$Status >= 0 & dat$pop$sex == "f")
      newly_eligible <- newly_eligible_temp[which(newly_eligible_temp %% 20 != 0)] #select agents whose index isn't an even multiple of 20
    } 
    
    
    if (length(newly_eligible) >= 1) {
      # If the number of newly eligible people exceeds the desired number under care, randomly sample from the list of newly_eligible
      current_under_care <- length(which(dat$pop$eligible_care==1 & dat$pop$Status >= 0))
      desired_under_care <- round(length(which(dat$pop$Status >= 0))*dat$param$prob_care_after_campaign)
      new_under_care <- max(0,desired_under_care - current_under_care)
      if (length(newly_eligible) > new_under_care) {
        newly_eligible <- sample(newly_eligible,new_under_care)  
      }
      # Finally make the people eligible for care
      dat$pop$eligible_care[newly_eligible] = 1
    }
  }
  
  # If if the last element in the list is "random" and the desired percentage of people "under care" hasn't been reached, then we
  # select new people at random to get up the desired number under care.  Note that with this algorithm we can model sometimes having
  # fewer than the desired number "under care" by making the last element of the list be something other than "random"
 
  if (scale_up_strategy[num_care_strategies] == "random") {
    if (num_care_strategies > 1) {
      jjjjunk = 1 # Pause point for debugging
    }
    current_under_care <- length(which(dat$pop$eligible_care==1 & dat$pop$Status >= 0))
    desired_under_care <- round(length(which(dat$pop$Status >= 0))*dat$param$prob_care_after_campaign)
    new_under_care <- desired_under_care - current_under_care
    if (new_under_care >= 1) {
      not_curr_eligible <- which(dat$pop$eligible_care == 0 & dat$pop$Status >= 0)
      newly_eligible <- sample(not_curr_eligible,new_under_care)
      dat$pop$eligible_care[newly_eligible] = 1
    }
  }
  
  return(dat)
}