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
transmission <- function(dat,at){

  if(!is.null(  dat$discord_coital_df)){  #if no sex acts, skip transmission functions
    dat <- transmission_main_module(dat,at) #caclulate initial transmission probs
    dat <- transmission_vaccine(dat,at) #if vaccine model in effect, modify trans probs, does nothing otw 
    dat <- transmission_inf_calc(dat,at) #calculate actual infections
    dat <- transmission_bookkeeping_module(dat,at) #fill in data structures
    dat <- transmission_cd4_module(dat,at) #calculate cd4 values for newly infected
  }
  return(dat)
}