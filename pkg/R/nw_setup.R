#' @export

nw_setup <- function(params){

nw <- setup_initialize_network(params)

#--------------------------------------------------------------

#estimate initial network (create argument list, then call fxn)
netest_arg_list <- list(
  nw            =  nw,
  formation     =  as.formula(params$nw_form_terms),
  target.stats  =  params$target_stats,
  coef.form     =  params$nw_coef_form,
  constraints   =  as.formula(params$nw_constraints),
  verbose       =  FALSE,
  coef.diss     =  dissolution_coefs( dissolution =  as.formula(params$dissolution),
                                      duration    =  params$relation_dur,
                                      d.rate      =  3e-05) )

estimated_nw <- do.call(EpiModel::netest, netest_arg_list)
return(estimated_nw)
}
