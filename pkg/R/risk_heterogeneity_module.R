#' @export

risk_heterogeneity_module <- function(dat, at) {
  
  if(at %% dat$param$popsumm_frequency == 0) {
    dat$param$trans_lambda <- dat$param$trans_lambda_init * exp(-dat$param$trans_lambda_alpha * dat$popsumm$prevalence[at/dat$param$popsumm_frequency + 1])
  }
  
  return(dat)
}