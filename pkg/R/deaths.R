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
evo_departures <- function(dat,at){
  
  #Description:
  #Calls vital_death_aged_out, vital_death_aids, vital_death_non_aids
  dat <- vital_death_aids(dat,at)
  dat<- vital_death_aged_out(dat,at)
  dat <- vital_death_non_aids(dat,at)
  
  #This is necessary step for epimodel's edges_correct(), a subfxn
  #in resim_nets(); dat$epi$num is a vector that stores how many agents alive each 
  #timestep. This fxn call normall occurs in epimodel's "prevalence.net", which evonet
  #doesn't use
  dat <- set_epi(dat, "num", at, sum(dat$attr$active))
  
  return(dat)

}

