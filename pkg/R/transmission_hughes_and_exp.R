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
transmission_hughes_and_exp<-function(dat, acute_phase_status, sti_status_sus, condom_use,
                                      msm_sus_receptive_status, msm_sus_insert_status,
                                      msm_circum_status_insert_sus, age_vec_sus,
                                      hetero_sus_female_vi, hetero_sus_female_ai,
                                      hetero_sus_male_ai, hetero_sus_male_circum_status,
                                      logV_inf, vacc_sens_sus){
  # Hughes or exponetial
  # Effect of viral load on infection 
  if(dat$param$transmission_model=="hughes"){
    xB_vl <- (logV_inf - dat$param$trans_VLbase) * log(dat$param$trans_RR_LogV)
  }
  
  if(dat$param$transmission_model=="exponential"){
    generic_exp = dat$param$BaselineRisk_SimpleExp*logV_inf *log(dat$param$RR_LogV_Simple_Exp)
    xB_vl = log(generic_exp)  # To accommodate Hughes et al’.s ln-transformed risk factors
  }
  
  # If Hughes or exponential apply Hughes' double exponential strategy for RR (adding the xB terms and exponentiating)
  if(dat$param$model_sex=="msm") {
    xB_msm  <-  rep(0, nrow(dat$discord_coital_df))
    # RR sus male insertive anal (relative to baseline of insertive vaginal)
    xB_msm <-  xB_msm + msm_sus_insert_status * log(dat$param$trans_RR_insertive_anal_msm)    
    # RR sus male receptive anal (relative to baseline of insertive vaginal)
    xB_msm <-  xB_msm + msm_sus_receptive_status * log(dat$param$trans_RR_receptive_anal_msm)
    # RR sus male msm insertive circumcised
    xB_msm <-  xB_msm + msm_circum_status_insert_sus * log(dat$param$trans_RR_circumcised)
    xB <- xB_msm + xB_vl
  } else {
    xB_hetero   <-  rep(0, nrow(dat$discord_coital_df))
    # sus female, act of vi (relative to baseline of sus hetero male, act of vi)
    xB_hetero   <-  xB_hetero + hetero_sus_female_vi * log(dat$param$trans_RR_receptive_vaginal)
    # sus female, act of ai (relative to baseline of sus hetero male, act of vi)
    xB_hetero <- xB_hetero + hetero_sus_female_ai * log(dat$param$trans_RR_rec_ai)
    # sus male, act of ai (relative to baseline of sus hetero male, act of vi)
    xB_hetero <- xB_hetero + hetero_sus_male_ai * log(dat$param$trans_RR_ins_ai)
    # RR sus hetero male insertive circumcised
    xB_hetero   <-  xB_hetero + hetero_sus_male_circum_status * log(dat$param$trans_RR_circumcised)
    xB <- xB_hetero + xB_vl
  }
  #risk factors relevant to all couples
  xB <- xB + acute_phase_status * log(dat$param$trans_RR_acute_phase)
  xB <- xB + sti_status_sus * log(dat$param$trans_RR_STI)
  xB <- xB + condom_use * log(dat$param$trans_RR_uses_condoms)
  # Age effect
  temp_age_xb <-  (age_vec_sus - dat$param$trans_base_age) * log(dat$param$trans_RR_age)/10
  xB <- xB + temp_age_xb
  xB <- xB + vacc_sens_sus * log(dat$param$trans_RR_vaccine)
  exp_xB = exp(xB)
  result <- 1 - ((1-dat$param$trans_lambda)^(1*exp_xB))
  return(result)
}