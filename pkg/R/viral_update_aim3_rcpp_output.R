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
  dat$attr$V_vec[ind,]     = out$V_vec
  dat$attr$V[ind]          = sum(dat$attr$V_vec[ind,])
  dat$attr$I_vec[ind,]     = out$I_vec
  dat$attr$M_vec[ind,]     = out$M_vec
  dat$attr$L_vec[ind,]     = out$L_vec
  dat$attr$K[ind]          = out$K
  dat$attr$Imm_Trig[ind]   = out$Imm_Trig
  dat$attr$ChronPhase[ind] = out$ChronPhase  
  dat$attr$Drug1[ind]      = out$Drug1  
  dat$attr$Drug2[ind]      = out$Drug2 
  dat$attr$Drug3[ind]      = out$Drug3  
  dat$attr$Drug4[ind]      = out$Drug4  
  dat$attr$Aim3RoundingErrors[ind] = out$Aim3RoundingErrors
  
  return(dat$attr)
  
}
