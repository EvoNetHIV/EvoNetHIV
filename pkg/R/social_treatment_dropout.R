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
social_treatment_dropout <- function(dat, at)
{
  # Description:
  # People on treatment have a small per day probability of halting their therapy
  
  # Inputs: 
  # pop$treated
  # param$prob_tx_dropout   *** new parameter ***
  # Outputs:
  # pop$treated
  # pop$tx_stop_time  *** new attribute ***
  
  curr_treated <- which(dat$pop$Status >= 0  & dat$pop$treated == 1)
  
  if(length(curr_treated)==0){return(dat)}
  
  num_curr_treated <- length(curr_treated)
  prob_dropout <- runif(num_curr_treated)
  dropouts <- which(prob_dropout < dat$param$prob_tx_droput)
  if(length(dropouts)>0){
    dropout_ix <- curr_treated[dropouts]
    dat$pop$treated[dropout_ix] <- 0
    dat$pop$tx_stop_time[dropout_ix] <- at
    dat$pop$tx_dropout[dropout_ix] <- 1
  }
  
  return(dat)
}    
