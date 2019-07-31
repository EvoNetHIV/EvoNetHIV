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
transmission_hill_fxn<-function(dat, acute_phase_status, sti_status_sus, condom_use, 
                                msm_sus_receptive_status, msm_sus_insert_status,
                                msm_circum_status_insert_sus, age_vec_sus,
                                hetero_sus_female_vi, hetero_sus_female_ai, 
                                hetero_sus_male_ai, hetero_sus_male_circum_status,
                                V_inf, vacc_sens_sus, susceptibility){
# Effect of viral load on infection 
# MaxInfRate            = 0.002,  	    	  # Asymptotic function from Fraser 2007, Assuming P_inf(1 year) = 1 - (1 _ P_inf(1 day))^365
# VHalfMaxInfRate       = 13938,				    # Fraser 2007
# HillCoeffInfRate      = 1.02, 				    # Fraser 2007

#all couples
hill_function1 <-  dat$param$MaxInfRate*"^"(V_inf, dat$param$HillCoeffInfRate) 
hill_function2 <-  "^"(dat$param$VHalfMaxInfRate, dat$param$HillCoeffInfRate) +
  "^"(V_inf, dat$param$HillCoeffInfRate)   
hill_function <- hill_function1/hill_function2
result <- hill_function
result <- result * susceptibility     # New for AgeAndSPVL work
result[which(acute_phase_status==1)] <- result[which(acute_phase_status==1)] * dat$param$trans_RR_acute_phase
result[which(sti_status_sus==1)] <- result[which(sti_status_sus==1)] * dat$param$trans_RR_STI
result[which(condom_use==1)] <- result[which(condom_use==1)] * dat$param$trans_RR_uses_condoms
result[which(vacc_sens_sus==1)] <-  result[which(vacc_sens_sus==1)] *dat$param$trans_RR_vaccine

# Age effect
temp_age_xb <-  dat$param$trans_RR_age^((age_vec_sus-dat$param$trans_base_age)/10)
result  <-  result  * temp_age_xb

if(dat$param$model_sex=="msm") {
  result[which(msm_sus_insert_status==1)]  <-   result[which(msm_sus_insert_status==1)]   * dat$param$trans_RR_insertive_anal_msm 
  # RR sus male receptive anal (relative to baseline of insertive vaginal)
  result[which(msm_sus_receptive_status==1)] <-  result[which(msm_sus_receptive_status==1)] * dat$param$trans_RR_receptive_anal_msm
  # RR sus male msm insertive circumcised
  result[which(msm_circum_status_insert_sus==1)] <-  result[which(msm_circum_status_insert_sus==1)] * dat$param$trans_RR_circumcised
} else {
  # sus female, act of vi (relative to baseline of sus hetero male, act of vi)
  result[which(hetero_sus_female_vi == 1)]  <-  result[which(hetero_sus_female_vi == 1)] * dat$param$trans_RR_receptive_vaginal
  # sus female, act of ai (relative to baseline of sus hetero male, act of vi)
  result[which(hetero_sus_female_ai == 1)] <- result[which(hetero_sus_female_ai == 1)] * dat$param$trans_RR_rec_ai
  # sus male, act of ai (relative to baseline of sus hetero male, act of vi)
  result[which(hetero_sus_male_ai == 1)] <- result[which(hetero_sus_male_ai == 1)] * dat$param$trans_RR_ins_ai
  # RR sus hetero male insertive circumcised
  result[which(hetero_sus_male_circum_status==1)]   <-  result[which(hetero_sus_male_circum_status==1)] * dat$param$trans_RR_circumcised
}
return(result)
}