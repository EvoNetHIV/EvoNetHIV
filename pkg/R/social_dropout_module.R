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
social_dropout_module <- function(dat, at) {
  
  #identify dropouts
  dat <- social_treatment_droput(dat,at)  
  #calculate expected values of vl for those on tx
  dat <- viral_update_gamma_treated(dat,at)  
  #revert vl to expected for droputs
  dat <- viral_update_gamma_dropout(dat,at)
  
  return(dat)
}
