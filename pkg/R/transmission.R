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
  dat <- transmission_main_module(dat,at)
  dat <- transmission_bookkeeping_module(dat,at)
  dat <- transmission_cd4_module(dat,at)
  return(dat)
}