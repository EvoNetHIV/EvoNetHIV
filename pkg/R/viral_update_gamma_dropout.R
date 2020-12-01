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
  infected <-  which( dat$attr$Status==1 & dat$attr$tx_dropout==1 & dat$attr$V< dat$attr$vl_expected)
  
  if(length(infected)>1){
    rebound_duration <- (timestep-dat$attr$tx_stop_time[infected])
    dat$attr$V[acute_exp_ix] <- dat$param$V0 * exp(dat$attr$r0[infected]*rebound_duration)
  }  
  
  return(dat)
}
