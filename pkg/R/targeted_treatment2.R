
#' @export
targeted_treatment2 <- function(dat, at)
{
  # Description:
  # Assuming some limit on how many people can get treated, this module determines which infected, diagnosed, eligible-for-care agents
  # get treatment given that a treatment campaign was in effect.  Choices are based on a user-specified list (e.g., "AIDS", "highrisk", "random")
  # If included, "random" needs to be the last element of the tx_type.,
  # -- This version differs from previous versions in that it can apply mulitiple criteria; e.g., first priority to AIDS, second priority to high risk people
  # This version (v3) differs from v2 in that we assume a linear increase in the number of people being treated between the start of the "spontaneous"
  # treatment campaign and the targeted TasP campaign.

  # Inputs:
    # param$start_treatment_campaign -- Start of targeted TasP campaign
    # param$start_treat_before_big_campaign -- Start of random therapy before TasP campaign
    # param$tx_limit    ("absolute_num" or "percentage")
    # param$max_num_treated  -- Maximum number of treated at the start of the TasP campaign (used with "absolute_num")
    # param$max_num_treated_begin -- Just before targeted TasP campaign
    # param$proportion_treated  -- used with "absolute_num"
    # param$proportion_treated_begin -- treated before the targeted TasP campaign
    # pop$treated
    # pop$Status
    # pop$diag_status
    # param$tx_type  -- a list of people c("high_risk", "under35") prioritized for therapy
 # Outputs:
    # pop$treated
    # pop$tx_init_time
    # proportion_treated_begin  (Note: _begin suffixes mean before the targeted TasP campaign.  Originally we had a notion that this would represent a single
    #                            beginning value.  As versions have evolved, we have allowed this beginning value to ramp-up from the value at start_treat_before_big_campaign )
    # param$max_num_treated  -- used with "absolute_num"
    # param$max_num_treated_begin -- before targeted TasP campaign


  if(length(which(dat$pop$Status==1))==0){return(dat)}

  total_alive <- length(which(dat$pop$Status>=0))
  diag_time_noNAs <- dat$pop$diag_time
  diag_time_noNAs[is.na(diag_time_noNAs)] <- 99999999

  if (at == dat$param$start_treat_before_big_campaign) {
    dat$param$total_infected_begin <- length(which(dat$pop$Status ==1))
      dat$param$proportion_treated_begin <- 0
      dat$param$max_num_treated_begin <- dat$param$proportion_treated_begin*dat$param$total_infected_begin
  }

  # Assume a baseline percent treated before the big treatment campaign
  if (at >= dat$param$start_treat_before_big_campaign &
      at <  dat$param$start_treatment_campaign) {
    new_eligible_tx <- which(dat$pop$Status == 1 & dat$pop$eligible_care == 1 &
                               dat$pop$diag_status == 1 & dat$pop$treated == 0 & (at - diag_time_noNAs > dat$param$mean_trtmnt_delay))
    num_new_eligible_tx <- length(new_eligible_tx)
    if (num_new_eligible_tx >=1){
      max_initial_treated <- round(dat$param$max_num_treated_begin)
      already_treated <- length(which(dat$pop$Status >= 0 & dat$pop$treated ==1)) # later add consistency checks
      max_new_treated <- max(0,max_initial_treated - already_treated)
      if (num_new_eligible_tx <= max_new_treated) {
        initial_treated <- new_eligible_tx
      } else {
        initial_treated <- sample(new_eligible_tx,max_new_treated)
      }
      dat$pop$treated[initial_treated] <- 1
      dat$pop$tx_init_time[initial_treated] <- at
    }
    # Allow for gradual increases in number treated in early spontaneous campaign to levels at the start of the TasP campaign
    time_elapsed <- (at - dat$param$start_treat_before_big_campaign) / (dat$param$start_treatment_campaign - dat$param$start_treat_before_big_campaign)
    dat$param$proportion_treated_begin <-  dat$param$proportion_treated_begin + time_elapsed * (dat$param$proportion_treated -dat$param$proportion_treated_begin)
    if (dat$param$proportion_treated_begin > 1) dat$param$proportion_treated_begin <- 1
    if (dat$param$proportion_treated_begin < 0) dat$param$proportion_treated_begin <- 0
    dat$param$max_num_treated_begin <- dat$param$proportion_treated_begin*dat$param$total_infected_begin

  }

  if(at < dat$param$start_treatment_campaign){return(dat)}


  # tx_type is a list of strategies (e.g., "high_risk", "under35") used to prioritize people for treatment
  tx_strategy <- dat$param$tx_type[[1]]  # First convert into a non-subscripted list for ease of programming
  num_tx_strategies <- length(tx_strategy) # length of the list

  # Loop through the list of strategies in order
  for (j in 1:num_tx_strategies) {

    if (tx_strategy[j] == "all") {
      eligible_tx <- dat$pop$Status == 1      # Positive control (Note this overrides any other options)
      dat$param$tx_limit <- "percentage"       # over-write parameters to ensure that everyone gets treated
      dat$param$proportion_treated <- 1
      selected <- which(eligible_tx)
      dat$pop$treated[selected] <- 1
      dat$pop$tx_init_time[selected] <- at
    }

    if (tx_strategy[j] == "all_diag") {
      eligible_tx <- dat$pop$Status == 1  & dat$pop$diag_status == 1   # Positive control (Note this overrides any other options)
      dat$param$tx_limit <- "percentage"       # over-write parameters to ensure that everyone gets treated
      dat$param$proportion_treated <- 1
      selected <- which(eligible_tx)
      dat$pop$treated[selected] <- 1
      dat$pop$tx_init_time[selected] <- at
    }

    if (tx_strategy[j] == "all_under25") {  # Treat everyone 25 and under without regard to overall limits
      eligible_tx <- dat$pop$Status == 1  & dat$pop$age <= 25
      selected <- which(eligible_tx)
      dat$pop$treated[selected] <- 1
      dat$pop$tx_init_time[selected] <- at
    }
    if (tx_strategy[j] == "all_under30") {  # Treat everyone 30 and under without regard to overall limits
      eligible_tx <- dat$pop$Status == 1  & dat$pop$age <= 30
      selected <- which(eligible_tx)
      dat$pop$treated[selected] <- 1
      dat$pop$tx_init_time[selected] <- at
    }

    # Identify people total eligible for tx and those aren't already being treated
    not_curr_tx <- dat$pop$Status == 1 & dat$pop$eligible_care == 1 & dat$pop$diag_status == 1  & dat$pop$treated == 0 & (at - diag_time_noNAs > dat$param$mean_trtmnt_delay)
    eligible_tx <- NULL

    if (!is.null(not_curr_tx)) {
      if (tx_strategy[j] == "none")            eligible_tx <- NULL       # Negative control
      if (tx_strategy[j] == "AIDS")            eligible_tx <- not_curr_tx & dat$pop$CD4 == 4 # assume AIDS is recognizable
      if (tx_strategy[j] == "CD4_under200")    eligible_tx <- not_curr_tx & dat$pop$CD4 == 4 # same as AIDS
      if (tx_strategy[j] == "CD4_under350")    eligible_tx <- not_curr_tx & dat$pop$CD4 >= 3 # assume rapid CD4 test
      if (tx_strategy[j] == "CD4_under500")    eligible_tx <- not_curr_tx & dat$pop$CD4 >= 2 # assume rapid CD4 test

      if (tx_strategy[j] == "CD4_nadir_under200")    eligible_tx <- not_curr_tx & dat$pop$CD4_nadir == 4 # same as AIDS
      if (tx_strategy[j] == "CD4_nadir_under350")    eligible_tx <- not_curr_tx & dat$pop$CD4_nadir >= 3 # assume rapid CD4 test
      if (tx_strategy[j] == "CD4_nadir_under500")    eligible_tx <- not_curr_tx & dat$pop$CD4_nadir >= 2 # assume rapid CD4 test

      if (tx_strategy[j] == "V2.5")    eligible_tx <- not_curr_tx & log10(dat$pop$V) >= 2.5 # Treat patients with high viral loads
      if (tx_strategy[j] == "V3.0")    eligible_tx <- not_curr_tx & log10(dat$pop$V) >= 3.0
      if (tx_strategy[j] == "V3.5")    eligible_tx <- not_curr_tx & log10(dat$pop$V) >= 3.5
      if (tx_strategy[j] == "V4.0")    eligible_tx <- not_curr_tx & log10(dat$pop$V) >= 4.0
      if (tx_strategy[j] == "V4.5")    eligible_tx <- not_curr_tx & log10(dat$pop$V) >= 4.5
      if (tx_strategy[j] == "V5.0")    eligible_tx <- not_curr_tx & log10(dat$pop$V) >= 5.0
      if (tx_strategy[j] == "V5.5")    eligible_tx <- not_curr_tx & log10(dat$pop$V) >= 5.5
      if (tx_strategy[j] == "V6.0")    eligible_tx <- not_curr_tx & log10(dat$pop$V) >= 6.0

      if (tx_strategy[j] == "S2.5")    eligible_tx <- not_curr_tx & log10(dat$pop$SetPoint) >= 2.5 # Treat patients with high viral loads
      if (tx_strategy[j] == "S3.0")    eligible_tx <- not_curr_tx & log10(dat$pop$SetPoint) >= 3.0
      if (tx_strategy[j] == "S3.5")    eligible_tx <- not_curr_tx & log10(dat$pop$SetPoint) >= 3.5
      if (tx_strategy[j] == "S4.0")    eligible_tx <- not_curr_tx & log10(dat$pop$SetPoint) >= 4.0
      if (tx_strategy[j] == "S4.5")    eligible_tx <- not_curr_tx & log10(dat$pop$SetPoint) >= 4.5
      if (tx_strategy[j] == "S5.0")    eligible_tx <- not_curr_tx & log10(dat$pop$SetPoint) >= 5.0
      if (tx_strategy[j] == "S5.5")    eligible_tx <- not_curr_tx & log10(dat$pop$SetPoint) >= 5.5
      if (tx_strategy[j] == "S6.0")    eligible_tx <- not_curr_tx & log10(dat$pop$SetPoint) >= 6.0

      if (tx_strategy[j] == "random05") eligible_tx <- not_curr_tx & (dat$pop$id %% 20 == 0)
      if (tx_strategy[j] == "random10") eligible_tx <- not_curr_tx & (dat$pop$id %% 10 == 1)

      if (tx_strategy[j] == "diag10yrs")  eligible_tx <- not_curr_tx & (at - diag_time_noNAs > 10*365)
      if (tx_strategy[j] == "diag8rsyr")  eligible_tx <- not_curr_tx & (at - diag_time_noNAs > 8*365)
      if (tx_strategy[j] == "diag6yrs")   eligible_tx <- not_curr_tx & (at - diag_time_noNAs > 6*365)
      if (tx_strategy[j] == "diag5yrs")   eligible_tx <- not_curr_tx & (at - diag_time_noNAs > 5*365)
      if (tx_strategy[j] == "diga4yrs")   eligible_tx <- not_curr_tx & (at - diag_time_noNAs > 4*365)
      if (tx_strategy[j] == "diag3yrs")   eligible_tx <- not_curr_tx & (at - diag_time_noNAs > 3*365)
      if (tx_strategy[j] == "diag2yrs")   eligible_tx <- not_curr_tx & (at - diag_time_noNAs > 2*365)
      if (tx_strategy[j] == "diag1yrs")   eligible_tx <- not_curr_tx & (at - diag_time_noNAs > 1*365)
      if (tx_strategy[j] == "diag0.5yrs") eligible_tx <- not_curr_tx & (at - diag_time_noNAs > 0.5*365)

      if (tx_strategy[j] == "Vlt2.5")    eligible_tx <- not_curr_tx & log10(dat$pop$V) < 2.5 # Treat patients with low viral loads (as a kind of negative control)
      if (tx_strategy[j] == "Vlt3.0")    eligible_tx <- not_curr_tx & log10(dat$pop$V) < 3.0
      if (tx_strategy[j] == "Vlt3.5")    eligible_tx <- not_curr_tx & log10(dat$pop$V) < 3.5
      if (tx_strategy[j] == "Vlt4.0")    eligible_tx <- not_curr_tx & log10(dat$pop$V) < 4.0
      if (tx_strategy[j] == "Vlt4.5")    eligible_tx <- not_curr_tx & log10(dat$pop$V) < 4.5
      if (tx_strategy[j] == "Vlt5.0")    eligible_tx <- not_curr_tx & log10(dat$pop$V) < 5.0
      if (tx_strategy[j] == "Vlt5.5")    eligible_tx <- not_curr_tx & log10(dat$pop$V) < 5.5
      if (tx_strategy[j] == "Vlt6.0")    eligible_tx <- not_curr_tx & log10(dat$pop$V) < 6.0

      if (tx_strategy[j] == "highrisk" ) eligible_tx <- not_curr_tx & dat$pop$att1 >= dat$param$attr_treatment_threshold
      if (tx_strategy[j] == "riskgroup_1" ) eligible_tx <- not_curr_tx & dat$pop$att1 ==1
      if (tx_strategy[j] == "riskgroups_12" ) eligible_tx <- not_curr_tx & dat$pop$att1 <= 2

      if (tx_strategy[j] == "riskgroup_3" ) eligible_tx <- not_curr_tx & dat$pop$att1 == 3
      if (tx_strategy[j] == "riskgroups_23" ) eligible_tx <- not_curr_tx & dat$pop$att1 >= 2

      if (tx_strategy[j] == "STIs")        eligible_tx <- not_curr_tx & dat$pop$sti_status == 1

      if (tx_strategy[j] == "random")  eligible_tx <- not_curr_tx

      if (tx_strategy[j] == "circum")      eligible_tx <- not_curr_tx & dat$pop$circum == 1
      if (tx_strategy[j] == "not_circum")  eligible_tx <- not_curr_tx & dat$pop$circum == 0

      if (tx_strategy[j] == "men")         eligible_tx <- not_curr_tx & dat$pop$sex == "m"
      if (tx_strategy[j] == "women")       eligible_tx <- not_curr_tx & dat$pop$sex == "f"

      # The next three are only really applicable to MSM relationships
      if (dat$param$model_sex == "msm") {
        if (tx_strategy[j] == "insertive")  eligible_tx <- not_curr_tx & dat$pop$role == "I"
        if (tx_strategy[j] == "receptive")  eligible_tx <- not_curr_tx & dat$pop$role == "R"
        if (tx_strategy[j] == "versatile")  eligible_tx <- not_curr_tx & dat$pop$role == "V"
      }

      if (tx_strategy[j] == "under50")  eligible_tx <- not_curr_tx & dat$pop$age <= 45
      if (tx_strategy[j] == "under45")  eligible_tx <- not_curr_tx & dat$pop$age <= 45
      if (tx_strategy[j] == "under40")  eligible_tx <- not_curr_tx & dat$pop$age <= 40
      if (tx_strategy[j] == "under35")  eligible_tx <- not_curr_tx & dat$pop$age <= 35
      if (tx_strategy[j] == "under30")  eligible_tx <- not_curr_tx & dat$pop$age <= 30
      if (tx_strategy[j] == "under25")  eligible_tx <- not_curr_tx & dat$pop$age <= 25

      if (tx_strategy[j] == "under_max_age_over_min_age") {  # Target people of intermediate ages
        eligible_tx <- not_curr_tx & dat$pop$age > dat$param$min_age_recruit_for_care & dat$pop$age <= dat$param$max_age_recruit_for_care
        # Note: Although this is more general than "under50" etc., the fixed ones like "under50" are more useful for nested tests (so keep them)
      }

      if (tx_strategy[j] == "10acts")     eligible_tx <- not_curr_tx & dat$pop$total_acts >= 10
      if (tx_strategy[j] == "25acts")     eligible_tx <- not_curr_tx & dat$pop$total_acts >= 25
      if (tx_strategy[j] == "50acts")     eligible_tx <- not_curr_tx & dat$pop$total_acts >= 50
      if (tx_strategy[j] == "100acts")    eligible_tx <- not_curr_tx & dat$pop$total_acts >= 100
      if (tx_strategy[j] == "200acts")    eligible_tx <- not_curr_tx & dat$pop$total_acts >= 200
      if (tx_strategy[j] == "400acts")    eligible_tx <- not_curr_tx & dat$pop$total_acts >= 400
      if (tx_strategy[j] == "800acts")    eligible_tx <- not_curr_tx & dat$pop$total_acts >= 800

      if (tx_strategy[j] == "men_under45")  eligible_tx <- not_curr_tx & dat$pop$sex == "m" & dat$pop$age <= 45
      if (tx_strategy[j] == "men_under40")  eligible_tx <- not_curr_tx & dat$pop$sex == "m" & dat$pop$age <= 40
      if (tx_strategy[j] == "men_under35")  eligible_tx <- not_curr_tx & dat$pop$sex == "m" & dat$pop$age <= 35
      if (tx_strategy[j] == "men_under30")  eligible_tx <- not_curr_tx & dat$pop$sex == "m" & dat$pop$age <= 30
      if (tx_strategy[j] == "men_under25")  eligible_tx <- not_curr_tx & dat$pop$sex == "m" & dat$pop$age <= 25

      if (tx_strategy[j] == "men_under23_women_under27")  eligible_tx <- not_curr_tx & ((dat$pop$sex == "m" & dat$pop$age <= 23) | (dat$pop$sex == "f" & dat$pop$age <= 27))

      if (tx_strategy[j] == "women_under45")  eligible_tx <- not_curr_tx & dat$pop$sex == "f" & dat$pop$age <= 45
      if (tx_strategy[j] == "women_under40")  eligible_tx <- not_curr_tx & dat$pop$sex == "f" & dat$pop$age <= 40
      if (tx_strategy[j] == "women_under35")  eligible_tx <- not_curr_tx & dat$pop$sex == "f" & dat$pop$age <= 35
      if (tx_strategy[j] == "women_under30")  eligible_tx <- not_curr_tx & dat$pop$sex == "f" & dat$pop$age <= 30
      if (tx_strategy[j] == "women_under25")  eligible_tx <- not_curr_tx & dat$pop$sex == "f" & dat$pop$age <= 25

      if (is.null(eligible_tx)) num_eligible_tx <- 0
                          else  num_eligible_tx <- length(which(eligible_tx==TRUE))

      if (num_eligible_tx >= 1) {
        current_tx <- length(which(dat$pop$treated==1 & dat$pop$Status >= 0))
        if (dat$param$tx_limit == "absolute_num")  {
          if (at == dat$param$start_treatment_campaign) {
            total_infected <- length(which(dat$pop$Status ==1))
            dat$param$max_num_treated <- dat$param$proportion_treated*total_infected
          }
          upper_limit_tx <- round(dat$param$max_num_treated * ((1+dat$param$yearly_incr_tx)^(1/365))^(at-dat$param$start_treatment_campaign))
        }
        if (dat$param$tx_limit == "percentage")  {
          upper_limit_tx <- round(dat$param$proportion_treated*total_alive)
        }
        max_new_tx <- max(0,upper_limit_tx - current_tx)

        # Subset if the number eligible exceeds the maximum
        which_eligible_tx <- which(eligible_tx)
        if (num_eligible_tx <= max_new_tx) {
          selected <- which_eligible_tx    # treat all newly eligibles
        } else {
          selected <- sample(which_eligible_tx,max_new_tx)  # treat a subsample of the newly eligible
        }
        if (at == dat$param$start_treatment_campaign ) {
          dat$param$num_treated_start_campaign <- dat$param$num_treated_start_campaign + length(selected)
          if ( tx_strategy[j] == "random") {
            dat$param$num_randomly_chosen_start_campaign <- length(selected)
          }
        }
        dat$pop$treated[selected] <- 1
        dat$pop$tx_init_time[selected] <- at
        if (tx_strategy[j] != "random") {
          dat$pop$prioritized_tx[selected] <- 1 # Keep track of patients who receive therapy specifically b/c of prioritization strategy
        }

      } # At least one eligible in j-th strategy after applying criteria
    } # At least one eligible in j-th strategy before apply any criteria
  } # j-th strategy

  # Now allow for increases in treatment rates over time. Apply to both percentage and absolute_num
  if (at >= dat$param$start_treatment_campaign) {
    dat$param$proportion_treated <-  dat$param$proportion_treated * ( (1+dat$param$yearly_incr_tx)^(1/365) )
    if (dat$param$proportion_treated > 1) dat$param$proportion_treated <- 1
    #dat$param$max_num_treated <- dat$param$max_num_treated * ( (1+dat$param$yearly_incr_tx)^(1/365) )
  }
  return(dat)
}
