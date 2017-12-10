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
###########################################################
#' @export
viral_update_aim3_rcpp_output <- function(dat,out,ind)
{
  #description:
  #organizes output from Rcpp fxn, mod_logistic_v5, called in
  #viral_update_modified_logistic fxn
  #input: "out", output object from rcpp fxn mod_logistic_v5, that contains
  #variable info to be put into 'pop' list
  #output: values for 13 dat$pop variables for infected agents
  dat$pop$V_vec[ind,]     = out$V_vec
  dat$pop$V[ind]          = sum(dat$pop$V_vec[ind,])
  dat$pop$I_vec[ind,]     = out$I_vec
  dat$pop$M_vec[ind,]     = out$M_vec
  dat$pop$L_vec[ind,]     = out$L_vec
  dat$pop$K[ind]          = out$K
  dat$pop$Imm_Trig[ind]   = out$Imm_Trig
  dat$pop$ChronPhase[ind] = out$ChronPhase  
  dat$pop$Drug1[ind]      = out$Drug1  
  dat$pop$Drug2[ind]      = out$Drug2 
  dat$pop$Drug3[ind]      = out$Drug3  
  dat$pop$Drug4[ind]      = out$Drug4  
  dat$pop$Aim3RoundingErrors[ind] = out$Aim3RoundingErrors
  
  return(dat$pop)
  
}
