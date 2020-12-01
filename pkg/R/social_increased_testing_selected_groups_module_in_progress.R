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
social_increased_testing_selected_groups_module <- function(dat, at){   
 
  #Description: Identifies agents who should be tested more frequently ("enhanced testing")
  #              based on a user-specified priority list (e.g., "high_risk", "under35", "random") 
  #Inputs: 
    # dat$param$scale_up_type  -- a list of people (e.g., "high_risk", "under35") targeted for enhanced testing 
    # dat$param$start_scale_up_campaign
    # dat$param$prob_enhanced_testing_after_campaign
    # dat$param$testing_limit -- Criteria for upper limit to enhanced testing "percent_agents" or "percent_agents_minus_diagnosed"
    # Long list of agent-specific attributes (dat$pop vals) that could potentially be used as a basis for prioritizing people for testing and care
    #   Examples: att1 (risk group), CD4, STI, sex, age, total_acts, circum
  #Outputs: 
    # pop$enhanced_testing
  #Notes: 1. If included, "baserand", needs to be the first element of scale_up_type 
  #       2. If included, "random" needs to be the last element of the scale_up_type
  
  # scale_up_type is a list of people (e.g., "high_risk", "under35") that one might target for enhanced testing 
  scale_up_strategy <- dat$param$scale_up_type[[1]]  # First convert into a non-subscripted list for ease of programming
  num_care_strategies <- length(scale_up_strategy) # length of the list 

  if ((at < dat$param$start_scale_up_campaign) & (scale_up_strategy[1] != "baserand")) return(dat)
  orig_enhanced <- which(dat$attr$Status >= 0 & dat$attr$enhanced_testing==1)
  current_enhanced_testing <- length(orig_enhanced)
  all_agents <- which(dat$attr$Status >= -10)
  dat$attr$enhanced_testing[all_agents] <- 0 # Zero out the list each day (to allow for evolving lists: e.g., people aging out of the enhanced testing group)

    
  # Only allow a baseline level of (random) testing prior to the scale-up campaign
  if ((at < dat$param$start_scale_up_campaign) & (scale_up_strategy[1] == "baserand")) num_care_strategies = 1
  
  # Loop through the list of strategies in order
  for (j in 1:num_care_strategies) {
    
     # Identify living people who aren't already getting tested at an enhanced rate
     not_curr_enhanced <- dat$attr$Status >= 0 & dat$attr$enhanced_testing %in% c(0,NA)
     eligible_enhancement <- NULL
    
    if (scale_up_strategy[j] == "baserand")  eligible_enhancement <- not_curr_enhanced & (dat$attr$rand_prob_test_init < dat$param$prob_enhanced_testing_before_campaign)
    
    if (scale_up_strategy[j] == "AIDS")            eligible_enhancement <- not_curr_enhanced & dat$attr$CD4 == 4 # assume AIDS is recognizable
    if (scale_up_strategy[j] == "CD4_under200")    eligible_enhancement <- not_curr_enhanced & dat$attr$CD4 == 4 # same as AIDS
    if (scale_up_strategy[j] == "CD4_under350")    eligible_enhancement <- not_curr_enhanced & dat$attr$CD4 >= 3 # assume rapid CD4 test
    if (scale_up_strategy[j] == "CD4_under500")    eligible_enhancement <- not_curr_enhanced & dat$attr$CD4 >= 2 # assume rapid CD4 test
    
    if (scale_up_strategy[j] == "highrisk" ) eligible_enhancement <- not_curr_enhanced & dat$attr$att1 >= dat$param$attr_treatment_threshold
    if (scale_up_strategy[j] == "riskgroup_over2" ) eligible_enhancement <- not_curr_enhanced & dat$attr$att1 >= 2
    
    if (scale_up_strategy[j] == "STIs")        eligible_enhancement <- not_curr_enhanced & dat$attr$sti_status == 1
     
    if (scale_up_strategy[j] == "random")  eligible_enhancement <- not_curr_enhanced
     
    
    if (scale_up_strategy[j] == "recent_sex_known_hiv_pos")   eligible_enhancement <- not_curr_enhanced & (at - dat$attr$time_hiv_sex_act <= 90)
    
    if (scale_up_strategy[j] == "circum")      eligible_enhancement <- not_curr_enhanced & dat$attr$circum == 1
    if (scale_up_strategy[j] == "not_circum")  eligible_enhancement <- not_curr_enhanced & dat$attr$circum == 0
    
    if (scale_up_strategy[j] == "men")         eligible_enhancement <- not_curr_enhanced & dat$attr$sex == "m"
    if (scale_up_strategy[j] == "women")       eligible_enhancement <- not_curr_enhanced & dat$attr$sex == "f"
 
    # The next three are only really applicable to MSM relationships
    if (dat$param$model_sex == "msm") {
      if (scale_up_strategy[j] == "insertive")  eligible_enhancement <- not_curr_enhanced & dat$attr$role == "I"
      if (scale_up_strategy[j] == "receptive")  eligible_enhancement <- not_curr_enhanced & dat$attr$role == "R"
      if (scale_up_strategy[j] == "versatile")  eligible_enhancement <- not_curr_enhanced & dat$attr$role == "V"
    }

    if (scale_up_strategy[j] == "under50")  eligible_enhancement <- not_curr_enhanced & dat$attr$age <= 45
    if (scale_up_strategy[j] == "under45")  eligible_enhancement <- not_curr_enhanced & dat$attr$age <= 45
    if (scale_up_strategy[j] == "under40")  eligible_enhancement <- not_curr_enhanced & dat$attr$age <= 40
    if (scale_up_strategy[j] == "under35")  eligible_enhancement <- not_curr_enhanced & dat$attr$age <= 35
    if (scale_up_strategy[j] == "under30")  eligible_enhancement <- not_curr_enhanced & dat$attr$age <= 30
    if (scale_up_strategy[j] == "under25")  eligible_enhancement <- not_curr_enhanced & dat$attr$age <= 25
    
    if (scale_up_strategy[j] == "under_max_age_over_min_age") {  # Target people of intermediate ages
           eligible_enhancement <- not_curr_enhanced & dat$attr$age > dat$param$min_age_recruit_for_care & dat$attr$age <= dat$param$max_age_recruit_for_care
           # Note: Although this is more general than "under50" etc., the fixed ones like "under50" are more useful for nested tests (so keep them)
    }
    
    if (scale_up_strategy[j] == "10acts")     eligible_enhancement <- not_curr_enhanced & dat$attr$total_acts >= 10
    if (scale_up_strategy[j] == "25acts")     eligible_enhancement <- not_curr_enhanced & dat$attr$total_acts >= 25
    if (scale_up_strategy[j] == "50acts")     eligible_enhancement <- not_curr_enhanced & dat$attr$total_acts >= 50
    if (scale_up_strategy[j] == "100acts")    eligible_enhancement <- not_curr_enhanced & dat$attr$total_acts >= 100
    if (scale_up_strategy[j] == "200acts")    eligible_enhancement <- not_curr_enhanced & dat$attr$total_acts >= 200
    if (scale_up_strategy[j] == "400acts")    eligible_enhancement <- not_curr_enhanced & dat$attr$total_acts >= 400
    if (scale_up_strategy[j] == "800acts")    eligible_enhancement <- not_curr_enhanced & dat$attr$total_acts >= 800
  
    if (scale_up_strategy[j] == "men_under45")  eligible_enhancement <- not_curr_enhanced & dat$attr$sex == "m" & dat$attr$age <= 45
    if (scale_up_strategy[j] == "men_under40")  eligible_enhancement <- not_curr_enhanced & dat$attr$sex == "m" & dat$attr$age <= 40
    if (scale_up_strategy[j] == "men_under35")  eligible_enhancement <- not_curr_enhanced & dat$attr$sex == "m" & dat$attr$age <= 35
    if (scale_up_strategy[j] == "men_under30")  eligible_enhancement <- not_curr_enhanced & dat$attr$sex == "m" & dat$attr$age <= 30
    if (scale_up_strategy[j] == "men_under25")  eligible_enhancement <- not_curr_enhanced & dat$attr$sex == "m" & dat$attr$age <= 25
    
    if (scale_up_strategy[j] == "women_under45")  eligible_enhancement <- not_curr_enhanced & dat$attr$sex == "f" & dat$attr$age <= 45
    if (scale_up_strategy[j] == "women_under40")  eligible_enhancement <- not_curr_enhanced & dat$attr$sex == "f" & dat$attr$age <= 40
    if (scale_up_strategy[j] == "women_under35")  eligible_enhancement <- not_curr_enhanced & dat$attr$sex == "f" & dat$attr$age <= 35
    if (scale_up_strategy[j] == "women_under30")  eligible_enhancement <- not_curr_enhanced & dat$attr$sex == "f" & dat$attr$age <= 30
    if (scale_up_strategy[j] == "women_under25")  eligible_enhancement <- not_curr_enhanced & dat$attr$sex == "f" & dat$attr$age <= 25
 
    if (is.null(eligible_enhancement)) num_eligible_enhancement <- 0 
                                  else num_eligible_enhancement <- length(which(eligible_enhancement==TRUE))
       
    if (num_eligible_enhancement >= 1) {
      if (dat$param$testing_limit == "percent_agents") 
        upper_limit_enhanced_testing <- round(length(which(dat$attr$Status >= 0))*dat$param$prob_enhanced_testing_after_campaign)
      if (dat$param$testing_limit == "percent_agents_minus_diagnosed") 
        upper_limit_enhanced_testing <- round(  length(which(dat$attr$Status >= 0))*dat$param$prob_enhanced_testing_after_campaign 
                                              - length(which(dat$attr$Status >= 0 & dat$attr$diag_status==1)) )
      max_enhanced_testing <- max(0,upper_limit_enhanced_testing - current_enhanced_testing)
     
      if (num_eligible_enhancement <= max_enhanced_testing) {
        selected <- eligible_enhancement   # Enhanced testing of all eligibles
        dat$attr$enhanced_testing[selected] <- 1  
      } else {
        which_eligible_enhancement <- which(eligible_enhancement==TRUE)  # Limit enhanced testing to stay under the maximum
        weight_eligible_enhancement <- dat$attr$rand_prob_test[which_eligible_enhancement]
        selected <- which_eligible_enhancement[which(rank(weight_eligible_enhancement)<=max_enhanced_testing)]
        dat$attr$enhanced_testing[selected] <- 1
      }
      current_enhanced_testing <- current_enhanced_testing + length(which(selected==TRUE))
      dat$attr$ever_enhanced_testing[selected] <- 1
    } # At least one eligible in j-th strategy
  } # j-th strategy
  return(dat)
}
