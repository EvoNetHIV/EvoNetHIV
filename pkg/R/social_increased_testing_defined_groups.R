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
social_increased_testing_defined_groups <- function(dat, at){   
 
# Description: Identifies agents who should be tested more frequently ("enhanced testers")
#              based on a user-specified priority list (e.g., "high_risk", "under35", "random") 
#
#  This version differs from previous versions in that there is no random component (other than testing against
#  some agent-specific probabilities set at birth).  I made this "deterministic" version b/c the random versions
#  prove to be tricker than I expected.  This version is a lot more straight-forward and harder to break.  However,
#  to use it to compare different criteria, one may want to tune random_xx options so that the different strategies
#  are testing the same number of people.
#
# Inputs: 
#   dat$param$scale_up_type  -- a list of people (e.g., "high_risk", "under35") targeted for enhanced testing 
#   dat$param$start_scale_up_campaign
#   dat$param$prob_enhanced_testing_before_campaign
#   dat$param$prob_enhanced_testing_after_campaign
#   dat$param$testing_limit -- Criteria for upper limit to enhanced testing "percent_agents" or "percent_agents_minus_diagnosed"
#   Long list of agent-specific attributes (dat$pop vals) that could potentially be used as a basis for prioritizing people for testing and care
#     Examples: att1 (risk group), CD4, STI, sex, age, total_acts, circum
# Outputs: 
#   pop$enhanced_testing
#   pop$ever_enhanced_testing
  
# Zero out the list each day (to allow for evolving lists: e.g., people aging out of the enhanced testing group)
all_agents <- which(dat$pop$Status >= -10)
dat$pop$enhanced_testing[all_agents] <- 0 
  
# Program allows a certain percent to be self-selected enhanced testers prior to the scale-up campaign (one that continues after the campaign)
pot_enhanced <- dat$pop$Status >= 0 & dat$pop$diag_status %in% c(0,NA)
elig_enhanced <- pot_enhanced & (dat$pop$rand_prob_test_init < dat$param$prob_enhanced_testing_before_campaign)
selected <- which(elig_enhanced)
dat$pop$enhanced_testing[selected] <- 1  
dat$pop$ever_enhanced_testing[selected] <- 1

# Start of scale-up campaign
if (at >= dat$param$start_scale_up_campaign) {
  
  # scale_up_type is a list of people (e.g., "high_risk", "under35") that one might target for enhanced testing 
  scale_up_strategy <- dat$param$scale_up_type[[1]]  # First convert into a non-subscripted list for ease of programming
  num_care_strategies <- length(scale_up_strategy) # length of the list 
  
  # Loop through the list of strategies in order
  for (j in 1:num_care_strategies) {
    
    # Start fresh each round identifying living people who are potential candidates for enhanced testing
    # Note: Although this routine zeros out the list of enhanced testers at the top, additions are purely additive from this point forward.
    pot_enhanced <- dat$pop$Status >= 0 & dat$pop$diag_status %in% c(0,NA)
    elig_enhanced <- NULL
  
    if (scale_up_strategy[j] == "all" ) elig_enhanced <- pot_enhanced  # Positive control

    if (scale_up_strategy[j] == "STIs")        elig_enhanced <- pot_enhanced & dat$pop$sti_status == 1
    
    if (scale_up_strategy[j] == "recent_sex_known_hiv_pos")   elig_enhanced <- pot_enhanced & (at - dat$pop$time_hiv_sex_act <= 90)

    if (scale_up_strategy[j] == "highrisk" ) elig_enhanced <- pot_enhanced & dat$pop$att1 >= dat$param$attr_treatment_threshold
    if (scale_up_strategy[j] == "riskgroup_over2" ) elig_enhanced <- pot_enhanced & dat$pop$att1 >= 2
    
    if (scale_up_strategy[j] == "circum")      elig_enhanced <- pot_enhanced & dat$pop$circum == 1
    if (scale_up_strategy[j] == "not_circum")  elig_enhanced <- pot_enhanced & dat$pop$circum == 0
    
    if (scale_up_strategy[j] == "men")         elig_enhanced <- pot_enhanced & dat$pop$sex == "m"
    if (scale_up_strategy[j] == "women")       elig_enhanced <- pot_enhanced & dat$pop$sex == "f"
    
    if (scale_up_strategy[j] == "AIDS")            elig_enhanced <- pot_enhanced & dat$pop$CD4 == 4 # assume AIDS is recognizable
    if (scale_up_strategy[j] == "CD4_under200")    elig_enhanced <- pot_enhanced & dat$pop$CD4 == 4 # same as AIDS
    if (scale_up_strategy[j] == "CD4_under350")    elig_enhanced <- pot_enhanced & dat$pop$CD4 >= 3 # assume rapid CD4 test (kind of positive control)
    if (scale_up_strategy[j] == "CD4_under500")    elig_enhanced <- pot_enhanced & dat$pop$CD4 >= 2 # assume rapid CD4 test (kind of positive control)
    
    # Pseudo-random strategies based on arbitrary agent-specific random numbers. (note: independent ranges to allow for additive summing)
    if (scale_up_strategy[j] == "r01") elig_enhanced <- pot_enhanced & (dat$pop$rand_prob_test < 0.01)
    if (scale_up_strategy[j] == "r02") elig_enhanced <- pot_enhanced & (dat$pop$rand_prob_test >= 0.01) & (dat$pop$rand_prob_test < 0.03)
    if (scale_up_strategy[j] == "r04") elig_enhanced <- pot_enhanced & (dat$pop$rand_prob_test >= 0.03) & (dat$pop$rand_prob_test < 0.07)
    if (scale_up_strategy[j] == "r08") elig_enhanced <- pot_enhanced & (dat$pop$rand_prob_test >= 0.07) & (dat$pop$rand_prob_test < 0.15)
    if (scale_up_strategy[j] == "r16") elig_enhanced <- pot_enhanced & (dat$pop$rand_prob_test >= 0.15) & (dat$pop$rand_prob_test < 0.31)
    if (scale_up_strategy[j] == "r32") elig_enhanced <- pot_enhanced & (dat$pop$rand_prob_test >= 0.31) & (dat$pop$rand_prob_test < 0.63)
    if (scale_up_strategy[j] == "r37") elig_enhanced <- pot_enhanced & (dat$pop$rand_prob_test >= 0.63)
    
    
    # The next three are only really applicable to MSM relationships
    if (dat$param$model_sex == "msm") {
      if (scale_up_strategy[j] == "insertive")  elig_enhanced <- pot_enhanced & dat$pop$role == "I"
      if (scale_up_strategy[j] == "receptive")  elig_enhanced <- pot_enhanced & dat$pop$role == "R"
      if (scale_up_strategy[j] == "versatile")  elig_enhanced <- pot_enhanced & dat$pop$role == "V"
    }

    if (scale_up_strategy[j] == "under50")  elig_enhanced <- pot_enhanced & dat$pop$age <= 45
    if (scale_up_strategy[j] == "under45")  elig_enhanced <- pot_enhanced & dat$pop$age <= 45
    if (scale_up_strategy[j] == "under40")  elig_enhanced <- pot_enhanced & dat$pop$age <= 40
    if (scale_up_strategy[j] == "under35")  elig_enhanced <- pot_enhanced & dat$pop$age <= 35
    if (scale_up_strategy[j] == "under30")  elig_enhanced <- pot_enhanced & dat$pop$age <= 30
    if (scale_up_strategy[j] == "under25")  elig_enhanced <- pot_enhanced & dat$pop$age <= 25
    
    if (scale_up_strategy[j] == "over50")  elig_enhanced <- pot_enhanced & dat$pop$age > 45
    if (scale_up_strategy[j] == "over45")  elig_enhanced <- pot_enhanced & dat$pop$age > 45
    if (scale_up_strategy[j] == "over40")  elig_enhanced <- pot_enhanced & dat$pop$age > 40
    if (scale_up_strategy[j] == "over35")  elig_enhanced <- pot_enhanced & dat$pop$age > 35
    if (scale_up_strategy[j] == "over30")  elig_enhanced <- pot_enhanced & dat$pop$age > 30
    if (scale_up_strategy[j] == "over25")  elig_enhanced <- pot_enhanced & dat$pop$age > 25
    
    if (scale_up_strategy[j] == "under_max_age_over_min_age") {  # Target people of intermediate ages
           elig_enhanced <- (pot_enhanced & dat$pop$age > dat$param$min_age_recruit_for_care) & (dat$pop$age <= dat$param$max_age_recruit_for_care)
           # Note: Although this is more general than "under50" etc., the fixed ones like "under50" are more useful for nested tests (so keep them)
    }
    
    if (scale_up_strategy[j] == "10acts")     elig_enhanced <- pot_enhanced & dat$pop$total_acts >= 10
    if (scale_up_strategy[j] == "25acts")     elig_enhanced <- pot_enhanced & dat$pop$total_acts >= 25
    if (scale_up_strategy[j] == "50acts")     elig_enhanced <- pot_enhanced & dat$pop$total_acts >= 50
    if (scale_up_strategy[j] == "100acts")    elig_enhanced <- pot_enhanced & dat$pop$total_acts >= 100
    if (scale_up_strategy[j] == "200acts")    elig_enhanced <- pot_enhanced & dat$pop$total_acts >= 200
    if (scale_up_strategy[j] == "400acts")    elig_enhanced <- pot_enhanced & dat$pop$total_acts >= 400
    if (scale_up_strategy[j] == "800acts")    elig_enhanced <- pot_enhanced & dat$pop$total_acts >= 800
  
    if (scale_up_strategy[j] == "men_under45")  elig_enhanced <- pot_enhanced & dat$pop$sex == "m" & dat$pop$age <= 45
    if (scale_up_strategy[j] == "men_under40")  elig_enhanced <- pot_enhanced & dat$pop$sex == "m" & dat$pop$age <= 40
    if (scale_up_strategy[j] == "men_under35")  elig_enhanced <- pot_enhanced & dat$pop$sex == "m" & dat$pop$age <= 35
    if (scale_up_strategy[j] == "men_under30")  elig_enhanced <- pot_enhanced & dat$pop$sex == "m" & dat$pop$age <= 30
    if (scale_up_strategy[j] == "men_under25")  elig_enhanced <- pot_enhanced & dat$pop$sex == "m" & dat$pop$age <= 25
    
    if (scale_up_strategy[j] == "women_under45")  elig_enhanced <- pot_enhanced & dat$pop$sex == "f" & dat$pop$age <= 45
    if (scale_up_strategy[j] == "women_under40")  elig_enhanced <- pot_enhanced & dat$pop$sex == "f" & dat$pop$age <= 40
    if (scale_up_strategy[j] == "women_under35")  elig_enhanced <- pot_enhanced & dat$pop$sex == "f" & dat$pop$age <= 35
    if (scale_up_strategy[j] == "women_under30")  elig_enhanced <- pot_enhanced & dat$pop$sex == "f" & dat$pop$age <= 30
    if (scale_up_strategy[j] == "women_under25")  elig_enhanced <- pot_enhanced & dat$pop$sex == "f" & dat$pop$age <= 25
 
    ### PrEP increased testing
    if (scale_up_strategy[j] == "prep")  elig_enhanced <- pot_enhanced & dat$pop$prep_list == 1
    
    if (is.null(elig_enhanced)) num_elig_enhanced <- 0 
                           else num_elig_enhanced <- length(which(elig_enhanced==TRUE))
       
    if (num_elig_enhanced >= 1) {
      selected <- which(elig_enhanced)
      dat$pop$enhanced_testing[selected] <- 1  
      dat$pop$ever_enhanced_testing[selected] <- 1
    } # At least one eligible in j-th strategy
  } # j-th strategy
} # after scale-up campaign 
if (at == dat$param$start_scale_up_campaign) {
  diag_enh <- which(dat$pop$Status >= 0 & 
                    (dat$pop$diag_status == 1 | dat$pop$enhanced_testing == 1))
  dat$param$num_enhanced_start_campaign <- length(diag_enh) 
}
  
return(dat)
}