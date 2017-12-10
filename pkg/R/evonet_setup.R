
#' @export
evonet_setup<-function(...){
  params <- input_params(...)
  params <- input_parameters_derived(params)
  return(params)
}

