#' @export
#' 
transmission_vaccine<- function(dat,at){
  #if vaccine model not running, skip this function
  if(!dat$param$vaccine_model){return(dat)}

  # draw m, then calculate theta.
  dat <- draw_m( dat, at );
  dat <- calculate_theta( dat, at );
  theta <- unlist(getTheta(dat,dat$discord_coital_df$sus_id))
  # adjust raw transmission probabilities
  dat$discord_coital_df$trans_probs <- dat$discord_coital_df$trans_probs * ( 1 - theta );
  
  return(dat)
}