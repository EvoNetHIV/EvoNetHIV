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
treatment_dropout <- function(dat, at)
{
  # Description:
  # People on treatment have a small per day probability of halting their therapy
  
  # Inputs: 
  # pop$treated
  # pop$Status
  # pop$tx_schedule
  # param$prob_tx_dropout   *** new parameter ***
  # Outputs:
  # pop$treated
  # pop$tx_stop_time  *** new attribute ***
  
  treated_30days <- which(dat$pop$Status >= 0  & dat$pop$treated == 1 & dat$pop$tx_schedule == "V" & (dat$pop$tx_init_time - at > 30))
  # Current code doesn't allow for gradual increases in VL when patients go off of therapy.
  # Instead VL jumps immediately to SPVL (or whatever it should be)
  # To get around this, I build in a 30-day time delay.  This way the probabilities can be 
  # interpreted as the probability that someone stopped therapy 30 days previously.
  
  if(length(treated_30days)==0){return(dat)}
  
  num_treated_30days <- length(treated_30days)
  prob_dropout <- runif(num_treated_30days)
  daily_dropout_prob = 1 - (1 - dat$param$prob_tx_droput)^(1/365)
  dropouts <- which(prob_dropout < daily_dropout_prob)
  if(length(dropouts)>0){
    dropout_ix <- treated_30days[dropouts]
    dat$pop$treated[dropout_ix] <- 0
    dat$pop$prioritized_tx[dropout_ix] <- 0
    dat$pop$tx_stop_time[dropout_ix] <- at
    dat$pop$Time_Inf_Adj[dropout_ix] <- dat$pop$Time_Inf_Adj[dropout_ix] + at - dat$pop$tx_init_time
  }
  
  return(dat)
}    
