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
  # Inputs: dat$attr$active, dat$pop$Status, dat$param$max_age
  # Outputs: dat$popsumm$aged_out, dat$pop$treated, dat$pop$Status, dat$attr$status_evo,
  # dat$attr$active, dat$pop$Time_Death, dat$nw
  
  active <- which(dat$attr$active == 1)
  active_evo <- which(dat$pop$Status>=0)
  
  ages <- dat$pop$age[active_evo]
  aged_out_index <- which(ages>=dat$param$max_age)
  if(length(aged_out_index) > 0){
    #bookkeeping
    dat$pop$treated[active_evo][aged_out_index] <- 0
    dat$pop$treated_2nd_line[active_evo][aged_out_index] <- 0
    dat$pop$Status[active_evo][aged_out_index] <- (-1.5)
    dat$attr$status_evo[active][aged_out_index] <- (-1)
    dat$attr$active[active][aged_out_index] <- 0
    dat$pop$Time_Death[active_evo][aged_out_index] <- at

    # modify either network or edgelist version
    # in edgelist mode, corresponding attributes on dat$attr will be deleted as well
    dat <- EpiModel:::terminate_vertices(dat = dat,
                                  at = at,
                                  vids.to.terminate = active[aged_out_index])
  }
    
  return(dat)
  
}  
  