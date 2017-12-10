#' @title Load default parameters
#'
#' @description Returns default parameter values
#'
#' @return list of parameter values.
#' @details
#' Additional details here
#' @examples
#' evoparams <- input_parameters()


#' @export
input_parameters <- function(){
  #returns default parameter values
  params1 <- input_parameters_primary()
  params2 <- input_parameters_cd4_data()
  c(params1,params2)
}
