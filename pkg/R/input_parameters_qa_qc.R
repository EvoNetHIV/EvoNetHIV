#' @title Check parameter values for plausible values.
#'
#' @description Evaluates parameter values for plausible values, returns warning/error when warranted (e.g., initial population size is non-positive). Mostly placeholder currently.
#'
#' @param evonet_params A list of EvoNet parameters.
#' @return return value here.
#' @details
#' Additional details here
#' @examples
#' primary_parameters  <- input_parameters_primary()
#' cd4_data            <- input_parameters_cd4_data()
#' evoparams <- c(primary_parameters, cd4_data)
#' input_parameters_qaqc(evoparams)
#' 
#' @export
input_parameters_qaqc <- function(evonet_params)
{
  #Description:
  # placeholder function used to review input parameter values before model run
  #in progress, many more checks possible 
  
  cat("\n----------------------------------")
  cat("\n Checking input parameters!\n")
  
  #---------------------------------------------
   if(!dir.exists(evonet_params$output_path)){
     cat("\n output path invalid \n")
     return(invisible(NULL))
   }
  #---------------------------------------------
  if(length(evonet_params$target_stats)==1){
    mean_degree <- ((2*evonet_params$target_stats)/evonet_params$initial_pop)
   cat("\n Mean degree of initial network:\n")
   cat( mean_degree, "\n")
   cat("\n")
  }
Sys.sleep(evonet_params$QA_QC_pause_time)
  #---------------------------------------------
cat("\n----(Pause time ",evonet_params$QA_QC_pause_time,")--------------")
if(evonet_params$birth_model=="poisson_birth_numbers"){
   cat("\n Birth rate parameter scaled for population size:\n")
   cat(evonet_params$poisson_birth_lambda/0.01370 *100,"\n")
   Sys.sleep(evonet_params$QA_QC_pause_time)
 }
  #---------------------------------------------
cat("\n----------------------------------")

if(length(evonet_params$target_stats)==1){
  cat("\n output files placed in:\n")
  cat( evonet_params$output_path, "\n")
  Sys.sleep(evonet_params$QA_QC_pause_time)
 }
cat("\n----------------------------------")

#---------------------------------------------
  
    #loci number diagnostics for aim3
  if(evonet_params$VL_Function == "aim3")
  {
    if(evonet_params$no_loci>evonet_params$Max_Allowable_Loci)
    {
      stop(paste("the number of specified loci is greater than number allowed; maximum
                   number of loci is ",evonet_params$Max_Allowable_Loci))
    }
  }
  #-------------------------------------------------
  
# Warn user and stop simulation if using both sti_prob and sti_prob_att parameters
  if(evonet_params$sti_prob != 0 && !is.na(evonet_params$sti_prob_att)) {
    stop(paste("If using att1-specific STI probabilities, sti_prob must be 0 (which is the
               default). Using both parameters will set STI status twice, leading to
               unexpected STI prevalence."))
  }

# Warn user and stop simulation if cd4_trt_guidelines_chgs is not in list format.
  if(length(evonet_params$start_treatment_campaign) > 1 && !is.list(evonet_params$cd4_trt_guidelines_chgs)) {
    stop(paste("cd4_trt_guidelines_chgs will not work as expected unless it is in list format."))
  }

# Warn user and stop simulation if the length of start_treatment_campaign and length of cd4_trt_guidelines_chgs differ
  if(length(evonet_params$start_treatment_campaign) != length(evonet_params$cd4_trt_guidelines_chgs)) {
    stop(paste("Length of start_treatment_campaign (length: ", length(evonet_params$start_treatment_campaign),
               ") must be the same as length of cd4_trt_guidelines_chgs (length: ", length(evonet_params$cd4_trt_guidelines_chgs)),
         ").")
  }

# Warn user and stop simulation if the number of vaccine target coverage values does not equal the 
# number of generic attribute groups
  if(evonet_params$target_vacc_att) {
    if(length(evonet_params$perc_vaccinated) != evonet_params$generic_nodal_att_no_categories) {
      stop(paste("Number of target vaccination coverage values (", length(evonet_params$perc_vaccinated),
                 ") must equal number of att1 groups (", evonet_params$generic_nodal_att_no_categories,
                 ").", sep = ""))
    }
  }

  #-------------------------------------------------
  #save_vl_list diagnostics
   if(evonet_params$save_vl_list==TRUE){
     if(evonet_params$initial_pop>200){
       cat("\n input parameter 'save_vl_list' set to TRUE but large initial population (>200) will slow model performance
             considerably; initial population <50 recommended when 'save_vl_list==TRUE' \n")
       
       Sys.sleep(evonet_params$QA_QC_pause_time)
       }
     if(evonet_params$n_steps> 5*365){
       cat("\n input parameter 'save_vl_list' set to TRUE but large number of timesteps will slow model performance
             considerably; timesetps < 1825 recommended when 'save_vl_list==TRUE' \n")
       
       Sys.sleep(evonet_params$QA_QC_pause_time)
     }
   }
  
  
}