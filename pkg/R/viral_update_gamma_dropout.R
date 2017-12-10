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
#' 
#' @export
viral_update_gamma_dropout <- function(dat, at) {

  timestep <- at
  infected <-  which( dat$pop$Status==1 & dat$pop$tx_dropout==1 & dat$pop$V< dat$pop$vl_expected)
  
  if(length(infected)>1){
    rebound_duration <- (timestep-dat$pop$tx_stop_time[infected])
    dat$pop$V[acute_exp_ix] <- dat$param$V0 * exp(dat$pop$r0[infected]*rebound_duration)
  }  
  
  return(dat)
}
