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
evo_arrivals <- function(dat, at) {
  
  #Description:
  #Calculates number of births (additions) to population for timestep
  #Expands network and population data structures based on number of births
  #Calls: vital_births_calculate_new(), vital_births_bookkeeping_pop(), vital_births_bookkeeping_misc() 
  #Inputs: param$birth_model==
              #"annual_growth_rate"
              #"births=deaths"
              #"poisson_birth_numbers"
              #"constant_rate"
              #"constant_number"
  #Outputs: dat$nw, dat$pop, dat$popsumm, dat$attr$status
  
  if(is.element(at, dat$param$circum_prob_yr_chg)) {
    dat$param$circum_prob <- dat$param$circum_prob_chg[which(at == dat$param$circum_prob_yr_chg)]
  }
    
  nBirths <- vital_births_calculate_new(dat,at)
  if(nBirths==0){return(dat)}
  
  dat$no_births <- dat$no_births + nBirths 
  
  #expand "attr" lists and fill in default values as needed
  dat$attr <- vital_births_bookkeeping_pop(no_births = nBirths, 
                                          dat=dat, timestep = at)
  
  
  return(dat)
}
