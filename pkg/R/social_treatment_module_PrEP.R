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
social_treatment_module_PrEP <- function(dat, at)
{
  
  #Description:
  #Assigns treatment status eligible uninfected patients
  #inputs: tx_type=="VL","CD4","time","vl_and_cd4","vl_and_time","vl_and_cd4_and_time"
          #param$start_treatment_campaign
           #pop$diag_status
           #pop$treated
           #pop$Status
           #pop$CD4
           #pop$Time_Inf
           #param$t_acute
           #param$min_inf_time_for_treat          
  #outputs: pop$treated
            #pop$tx_init_time
  dat$treatment_index <- NULL
  eligible_patients_criteria <- NULL
  
  if(at < dat$param$start_treatment_campaign){return(dat)}
  
   #eligible_patients: diagnosed, eligible for care
  eligible_patients <- 
      which(dat$attr$diag_status %in% c(0,NA) & dat$attr$eligible_care == 1) 
  
  if(length(eligible_patients)==0){return(dat)}
  
  # eligible_patients_criteria: Currently set to all eligible agents.  Later we might subset these
  # by risk factors
  
  eligible_patients_criteria <- eligible_patients
 
  if(length(eligible_patients_criteria)==0){return(dat)}

  #subset of eligible agents (eligible_patients) that meet specified tx criteria
  eligible_patients_treated <- eligible_patients[eligible_patients_criteria]
  
  dat$attr$treated[eligible_patients_treated] <- 1
  dat$attr$tx_init_time[eligible_patients_treated] <- at
  dat$treatment_index <- eligible_patients_treated
  
 return(dat)
}
