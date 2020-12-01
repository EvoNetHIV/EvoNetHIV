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
#' 
#' @export
vital_death_aged_out <- function(dat,at)
{
  #Description: 
  # identifies agents that have reached maximum age (para$max_age) and 
  # does necessary “pop” list and network bookkeeping
  # Inputs: dat$attr$active, dat$attr$Status, dat$param$max_age
  # Outputs: dat$popsumm$aged_out, dat$attr$treated, dat$attr$Status, dat$attr$status_evo,
  # dat$attr$active, dat$attr$Time_Death, dat$nw
  
  active_evo <- which(dat$attr$Status>=0)
  
  ages <- dat$attr$age[active_evo]
  aged_out_index <- which(ages>=dat$param$max_age)
  no_aged_out <- length(aged_out_index)
  
  if(no_aged_out){
    
    index <- active_evo[aged_out_index]
    #bookkeeping
    dat$attr$treated[index] <- 0
    dat$attr$treated_2nd_line[index] <- 0
    dat$attr$Status[index] <- (-1.5)
    dat$attr$active[index] <- 0
    dat$attr$exitTime[index] <- at
    dat$attr$Time_Death[index] <- at
    #update counter
    dat$no_aged_out <- dat$no_aged_out + no_aged_out
  }
  
  return(dat)
  
}  

  
