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
deaths_tx_time_adjusted <- function(dat,at){
  
  #Description:
  #Calls vital_death_aged_out, vital_death_aids, vital_death_non_aids
  dat <- vital_death_aids_john(dat,at)
  dat<- vital_death_aged_out(dat,at)
  dat <- vital_death_non_aids(dat,at)
  
  return(dat)

}

