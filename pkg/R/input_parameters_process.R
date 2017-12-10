#' @title Process raw input parameter list for model run
#'
#' @description Calculates derived parameters, qa/qc check, and converts input parameter list 
#' into EpiModel object
#'
#' @return list of parameter values.
#' @details
#' Additional details here
#' @examples
#' evoparams <- input_parameters()


#' @export
input_parameters_process <- function(params){
  pp1  <- input_parameters_derived(params)
  pp2 <- do.call(EpiModel::param.net,pp1)
  input_parameters_qaqc(evoparams)
  return(pp2)
}
