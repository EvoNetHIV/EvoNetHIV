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
social_2nd_line_treatment_module <- function(dat, at)
{
  
  # Description:
  # Assigns second line treatment status (0/1) to eligible patients based on being 
  #  infected, diagnosed, eligible for care, eligible for ART, and (most importantly)
  #  being diagnosed as being infected with a drug resistant virus 
  #inputs: 
           #pop$diag_resistance_status
  #outputs: pop$treated_2nd_line
            #pop$tx_2nd_line_init_time
  dat$treatment_index <- NULL
  eligible_patients_criteria <- NULL
  
  if(at < dat$param$start_treatment_campaign[1]){return(dat)}
  
  infected <-  which(dat$pop$Status==1)
  if(length(infected)==0){return(dat)}
  
  #eligible_patients: infected, diagnosed with HIV, eligible for care, eligible for 2nd line ART,
  #                   diagnosed with drug resistant virus, and not already taking 2nd line ART
  if(dat$param$second_line_elig == "vl_and_resist") {
    eligible_patients <- 
      which(dat$pop$Status == 1 & 
              dat$pop$diag_status == 1 & dat$pop$eligible_care == 1 &
              dat$pop$eligible_2nd_line_ART == 1 &
              dat$pop$treated_2nd_line != 1 &
              dat$pop$num_consec_VL_gt1k >= 2 &
              dat$pop$diag_resist_status == 1)
  }
  if(dat$param$second_line_elig == "vl_only") {
    eligible_patients <- 
      which(dat$pop$Status == 1 & 
              dat$pop$diag_status == 1 & dat$pop$eligible_care == 1 &
              dat$pop$eligible_2nd_line_ART == 1 & 
              dat$pop$treated_2nd_line != 1 &
              dat$pop$num_consec_VL_gt1k >= 2)
  }
   
  
  if(length(eligible_patients)==0){return(dat)}
    
  dat$pop$treated_2nd_line[eligible_patients] <- 1
  dat$pop$time_init_second_line[eligible_patients] <- at
  # dat$treatment_2nd_line_index <- eligible_patients
  
 return(dat)
}
