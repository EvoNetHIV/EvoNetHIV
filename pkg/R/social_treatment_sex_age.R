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
social_treatment_sex_age <- function(dat, at) {
  
  if(at < dat$param$start_treatment_campaign[1]) { return(dat) }
  
  # Modify CD4 treatment eligibility threshold if at designated guideline change timestep
  if(is.element(at, dat$param$start_treatment_campaign)) {
    dat$param$cd4_treatment_threshold <- dat$param$cd4_trt_guidelines_chgs[[which(at == dat$param$start_treatment_campaign)]]
  }
  
  if(length(which(dat$pop$Status == 1)) == 0) { return(dat) }
  
  # Select eligible patients: alive, HIV+, not on treatment, and meeting current CD4 threshold
  elig_pat <- which(dat$pop$Status == 1 & dat$pop$treated == 0 & 
                    (dat$pop$CD4 %in% dat$param$cd4_treatment_threshold | dat$pop$CD4_at_trtmnt %in% dat$param$cd4_treatment_threshold))
  
  if(length(elig_pat) == 0) { return(dat) }
  
  # Assign coverage level according to time point in simulation
  cov_prob <- dat$param$cov_prob[findInterval(at/365, dat$param$cov_prob_yrs)]
  
  # If overall treatment coverage is greater than overall target, return
  if(sum(dat$pop$treated[dat$pop$Status == 1])/sum(dat$pop$Status == 1) > cov_prob) { return(dat) }
  
 # if(at > 26*365) { browser() }
  # For each age and sex group, randomly select patients to initiate ART
  elig_sex_age <- list()
  for(i in 1:ncol(dat$param$cov_prob_scal)) {
    for(j in 1:nrow(dat$param$cov_prob_scal)) {
      # Subset eligible patients of the ith sex and jth age group
      elig <- elig_pat[dat$pop$sex[elig_pat] == colnames(dat$param$cov_prob_scal)[i] & findInterval(dat$pop$age[elig_pat], dat$param$cov_prob_ageg[[j]]) == 1]
      
      if(length(elig) == 0) { next }
      
      # Assign sex- and age-specific target coverage
      target_cov <- cov_prob * dat$param$cov_prob_scal[j, i]
      
      nTreated <- length(which(dat$pop$sex == colnames(dat$param$cov_prob_scal)[i] &
                                 findInterval(dat$pop$age, dat$param$cov_prob_ageg[[j]]) == 1 &
                                 dat$pop$Status == 1 & dat$pop$treated == 1))
      nElig    <- length(which(dat$pop$sex == colnames(dat$param$cov_prob_scal)[i] &
                                 findInterval(dat$pop$age, dat$param$cov_prob_ageg[[j]]) == 1 &
                                 dat$pop$Status == 1 &
                                 (is.element(dat$pop$CD4, dat$param$cd4_treatment_threshold) |
                                    is.element(dat$pop$CD4_at_trtmnt, dat$param$cd4_treatment_threshold))))
      current_cov <- nTreated/nElig
        
      #if(current_cov > target_cov) { browser() }
      if(current_cov > target_cov) { next }
      
      elig_sex_age[[length(elig_sex_age) + 1]] <- elig[which(rbinom(length(elig), 1, target_cov/365) == 1)] # Divide by 365 to make daily probability
    }
  }
  
  trt_pat <- unlist(elig_sex_age)
  
  if(length(trt_pat) == 0) { return(dat) }
  
  dat$pop$CD4_at_trtmnt[trt_pat] <- dat$pop$CD4[trt_pat]
  dat$pop$treated[trt_pat] <- 1
  dat$pop$tx_init_time[trt_pat] <- at
  dat$treatment_index <- trt_pat

  return(dat)
}
