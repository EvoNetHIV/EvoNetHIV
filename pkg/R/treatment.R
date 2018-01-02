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
 treatment <- function(dat, at)
{
  
  #Description:
  #Assigns treatment status (0/1) to eligible patients based on:
  # is there treatment camagin?
  # is vl > vl threshold for treatment
  # is infection duration > minimum duration for start of treatment
  # is cd4 < minimum threshold
  # is there a postivie diagnosis
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
  
  if(at < dat$param$start_treatment_campaign[1]){return(dat)}
  
  if(is.list(dat$param$cd4_trt_guidelines_chgs)) {
    if(is.element(at, dat$param$start_treatment_campaign)) {
      dat$param$cd4_treatment_threshold <- dat$param$cd4_trt_guidelines_chgs[[which(at == dat$param$start_treatment_campaign)]]
    }
  }
  
  infected <-  which(dat$pop$Status==1)
  if(length(infected)==0){return(dat)}
  
  #eligible_patients: infected,diagnosed,eligible for care,not treated
  eligible_patients <- 
      which(dat$pop$Status == 1 & dat$pop$treated == 0 &
            dat$pop$diag_status == 1 & dat$pop$eligible_care == 1 &
            dat$pop$eligible_ART == 1) 
  
  # If tx_in_acute_phase = F (i.e., patients are not treated if they are in acute phase), subset
  # those patients who were infected more than dat$param$t_acute days ago.
  if(!dat$param$tx_in_acute_phase) {
    eligible_patients <- eligible_patients[which((at - dat$pop$Time_Inf[eligible_patients]) > dat$param$t_acute)]
  }
  
  # Eligible patients initiate ART only when they attend a clinic visit (i.e., each patient
  # has some treatment delay following eligibility for ART). Subset eligible patients by 
  # memoryless process according to user-defined mean # of days of treatment delay.
  eligible_patients <- eligible_patients[which(rnorm(length(eligible_patients), dat$param$mean_trtmnt_delay, 365) > dat$param$mean_trtmnt_delay)]
  
  if(length(eligible_patients)==0){return(dat)}
  
  #eligible_patients_criteria: agents in eligible_patients not in acute stage and meet various
  #treatment criteria (time,vl,cd4) set in input_parameters_primary
  #Viral load: if VL higher than treatment threshold
  #CD4 count: if CD4 meets treatment threshold
  #Time: if time agent has been infected is greater than the minimun time for treatment
  
  if(dat$param$tx_type=="VL"){
  
    eligible_patients_criteria <- 
      which(dat$pop$V[eligible_patients] > dat$param$treatment_threshold)
  
  }
   
  if(dat$param$tx_type=="CD4") {
    
    eligible_patients_criteria <- 
      which(is.element(dat$pop$CD4[eligible_patients], dat$param$cd4_treatment_threshold))
  }
  if(dat$param$tx_type=="time"){
    
    eligible_patients_criteria <- 
      which((at-dat$pop$Time_Inf[eligible_patients]) > dat$param$min_inf_time_for_treat)
  }
  if(dat$param$tx_type == "time_dist") {
    eligible_patients_criteria <-
      which((at-dat$pop$Time_Inf[eligible_patients]) > dat$pop$min_time_tx[eligible_patients])
  }
  if(dat$param$tx_type=="vl_and_cd4"){
    
    eligible_patients_criteria <- 
        which(dat$pop$V[eligible_patients] > dat$param$treatment_threshold &
        is.element(dat$pop$CD4[eligible_patients], dat$param$cd4_treatment_threshold) )
  }
  if(dat$param$tx_type=="vl_and_time"){
    
    eligible_patients_criteria <- 
        which(dat$pop$V[eligible_patients] > dat$param$treatment_threshold &
        (at-dat$pop$Time_Inf[eligible_patients]) > dat$param$min_inf_time_for_treat )
  }
  if(dat$param$tx_type=="cd4_and_time"){
    
    eligible_patients_criteria <- 
         which( is.element(dat$pop$CD4[eligible_patients], dat$param$cd4_treatment_threshold) &
         (at-dat$pop$Time_Inf[eligible_patients]) > dat$param$min_inf_time_for_treat)
  }
  if(dat$param$tx_type=="cd4_and_time_dist") {
    eligible_patients_criteria <- 
      which(is.element(dat$pop$CD4[eligible_patients], dat$param$cd4_treatment_threshold) &
            (at-dat$pop$Time_Inf[eligible_patients]) > dat$pop$min_time_tx[eligible_patients])  
  }
  
  if(dat$param$tx_type=="vl_and_cd4_and_time"){
    
    eligible_patients_criteria <- 
         which(dat$pop$V[eligible_patients] > dat$param$treatment_threshold &
         is.element(dat$pop$CD4[eligible_patients], dat$param$cd4_treatment_threshold) &
         (at-dat$pop$Time_Inf[eligible_patients]) > dat$param$min_inf_time_for_treat )
  }
  
  if(length(eligible_patients_criteria)==0){return(dat)}

  #subset of eligible agents (eligible_patients) that meet specified tx criteria
  eligible_patients_treated <- eligible_patients[eligible_patients_criteria]
  
  dat$pop$treated[eligible_patients_treated] <- 1
  dat$pop$tx_init_time[eligible_patients_treated] <- at
  dat$treatment_index <- eligible_patients_treated
  dat$pop$vl_expected[eligible_patients_treated] <- dat$pop$V[eligible_patients_treated]
  
 return(dat)
}
