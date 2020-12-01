#' @title PrEP risk compensation module 
#'
#' @description PrEP risk compensation module parameterized with MSM values
#'
#' @param x A number.
#' @param y A number.
#' @return return value here.
#' @details
#' Additional details here
#' @examples
#' example function call here

#' @export
prep_risk_compensation<-function(dat,at){
  
  ## must be called AFTER social_treatment_prep_SES 
  ## not currently equipped to handle linked generic attribute group and prep adherance 
  
  if(at < dat$param$start_prep_campaign[1]){return(dat)}
  
  pop_on_prep <- which(dat$attr$on_prep==1)
  pop_not_on_prep <- which(is.na(dat$attr$on_prep) | (dat$attr$on_prep==-1))
  
  prep_init <- which(dat$attr$prep_init_time[pop_on_prep]==at)
  if(length(prep_init)>0){
    if(dat$param$individual_condom_prob_rc!=dat$param$individual_condom_prob_var){
    dat$attr$individual_condom_prob[pop_on_prep[prep_init]] <- rnorm(n=length(prep_init),mean=dat$param$individual_condom_prob_rc,sd=dat$param$condom_prob_sd)}
    
    if(dat$param$nonadherant_rc==T){
      group1 <- which(dat$attr$prep_decrease[pop_on_prep[prep_init]]==dat$param$prep_risk_decrease[1])
      group2 <- which(dat$attr$prep_decrease[pop_on_prep[prep_init]]==dat$param$prep_risk_decrease[2])
      group3 <- which(dat$attr$prep_decrease[pop_on_prep[prep_init]]==dat$param$prep_risk_decrease[3])
      group4 <- which(dat$attr$prep_decrease[pop_on_prep[prep_init]]==dat$param$prep_risk_decrease[4])
      dat$attr$individual_condom_prob[pop_on_prep[prep_init[group1]]] <- rnorm(n=length(prep_init),mean=dat$param$nonadherant_rc_condom_prob[1],sd=dat$param$condom_prob_sd)
      dat$attr$individual_condom_prob[pop_on_prep[prep_init[group2]]] <- rnorm(n=length(prep_init),mean=dat$param$nonadherant_rc_condom_prob[2],sd=dat$param$condom_prob_sd)
      dat$attr$individual_condom_prob[pop_on_prep[prep_init[group3]]] <- rnorm(n=length(prep_init),mean=dat$param$nonadherant_rc_condom_prob[3],sd=dat$param$condom_prob_sd)
      dat$attr$individual_condom_prob[pop_on_prep[prep_init[group4]]] <- rnorm(n=length(prep_init),mean=dat$param$nonadherant_rc_condom_prob[4],sd=dat$param$condom_prob_sd)}
  }
  
  prep_discont <- which(dat$attr$prep_discontinue_time[pop_not_on_prep]==at)
  if(length(prep_discont)>0){
    if(dat$param$individual_condom_prob_rc!=dat$param$individual_condom_prob_var){
    dat$attr$individual_condom_prob[pop_not_on_prep[prep_discont]] <- rnorm(n=length(prep_discont),mean=dat$param$individual_condom_prob_var,sd=dat$param$condom_prob_sd)}
    }
  return(dat)
}  
