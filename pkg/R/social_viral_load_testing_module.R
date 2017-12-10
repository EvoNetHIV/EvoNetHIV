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
social_viral_load_testing_module <- function(dat, at) {
  # Description: 
  # After user-specified time on treatment, test VL. Test at user-specified interval thereafter. 
  # Record whether the patient has VL > 1000 copies/mL. Call social_resistance_testing_module
  # or social_2nd_line_treatment_module, as appropriate.
  
  if(dat$param$VL_Function != "aim3") {return(dat)}
  
  # Identify those agents due for a VL test.
  # To have VL measured, must be HIV+ and alive (Status=1), have initiated ART at a user-specified period of time  
  # prior to eligibility for VL testing, and be eligible for VL testing (also must be diagnosed and being treated 
  # with ART, but those reqs are covered in ART initiation, which is documented by tx_init_time. So only need code
  # below). If at-dat$pop$tx_init-time == round(dat$param$time_on_tx_for_vl_testing, then the dividend is 0,
  # and the remainder will be 0. Thus, the code below works for identifying when an agent is due for both the 
  # first VL test and subsequent VL tests. 
  testing_ix <- which(dat$pop$Status == 1 & 
                      dat$pop$eligible_vl_test == 1 &
                      ((at - dat$pop$tx_init_time - round(dat$param$time_on_tx_for_vl_testing)) 
                       %% round(dat$param$vl_testing_interval) == 0))
  
  if(length(testing_ix) > 0) {
    # Of those tested, Identify those with VL > 1000 copies/mL and those <= 1000 copies/mL
    vl_gt_1k <- testing_ix[which(dat$pop$V[testing_ix] > 1000)]
    vl_le_1k <- testing_ix[which(dat$pop$V[testing_ix] <= 1000)]
    
    # If VL > 1000 copies/mL, increment number of consecutive VL measures. Else, set to 0.
    dat$pop$num_consec_VL_gt1k[vl_gt_1k] <- dat$pop$num_consec_VL_gt1k[vl_gt_1k] + 1
    dat$pop$num_consec_VL_gt1k[vl_le_1k] <- 0
    
    if(dat$param$second_line_elig == "vl_and_resist") {
      dat <- social_resistance_testing_module(dat, at)
    }
    if(dat$param$second_line_elig == "vl_only") {
      dat <- social_2nd_line_treatment_module(dat, at)
    }
  }
  
  return(dat)
}