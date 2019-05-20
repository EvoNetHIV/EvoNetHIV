#' @title PrEP risk factor module 
#'
#' @description module to determine if agent has risk factors for PrEP
#'
#' @param x A number.
#' @param y A number.
#' @return return value here.
#' @details
#' Additional details here
#' @examples
#' example function call here

#' @export
prep_risk_factors<-function(dat,at){
  
  #partner list code doesn't work on first timestep
  if(at<3){return(dat)}

  #convert fast edgelist indices to evonet indices
  aa<- dat$el[[1]]
  col1=dat$attr$id[aa[,1]]
  col2=dat$attr$id[aa[,2]]
  all.el <- c(col1,col2)
 
  #ts risk factor relevant until
  risk_time = at + dat$param$no_past_partners_time_prep #i.e. 6 months from present
  
  #risk factor = in a relationship
  dat$pop$last_ts_relationship[all.el] <- risk_time
  
  #risk factor = in 1+ relationships
  n_occur <- data.frame(table(all.el)) #data frame with a list of ids and the number of times they occurred.
  multiples <- n_occur[n_occur$Freq > 1,] #which ids occurred more than once.
  mult_rel <- as.numeric(as.character(multiples$all.el))
  
  dat$pop$last_ts_multiple_relationships[mult_rel] <- risk_time
  
  return(dat)
}