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
vital_births_calculate_new<-function(dat,at){
  #Description: calculate number of new births/additions for timestep based on birth_model parameter.
  #Inputs: param$birth_model==
              #"annual_growth_rate"
              #"poisson_birth_numbers"
              #"constant_rate"
              #"constant_number"
              #"constant_size"
  #Main output is number of births, stored in dat$popsumm$births[at], at=current timestep
  #Outputs:   dat$pop, dat$popsumm

nBirths <- 0
#-----------------------------------------------
#Sam's epimodel birth fxn
if (dat$param$birth_model=="annual_growth_rate")
{
  exptPopSize <- dat$param$initial_pop * ((1 + dat$param$pop_growth_rate_timestep)^at)
  numNeeded   <- exptPopSize - sum(dat$attr$active == 1)

  if (numNeeded > 0) {
    nBirths <- rpois(1, numNeeded)
  }
}
#-----------------------------------------------
# exponential growth
#  Tries to implement: dN1/dt = b*N1(0)*exp(r*t) - a*N1 - d1*N1, where N1 = youngest age class
#  This is like poisson_birth_lambda, except that the per-day birth rate can increase over time.
if (dat$param$birth_model=="exponential_growth")
{
  per_day_birth_rate <- dat$param$baseline_input_exp_growth*exp(dat$param$pop_growth_rate_timestep*at)
  nBirths <- rpois(1,per_day_birth_rate)
}
#-----------------------------------------------
#constant births, based on total deaths
if (dat$param$birth_model=="constant_size")
{
  
  nBirths <- length(which(dat$pop$Time_Death==at))
}
#-----------------------------------------------
#constant number of births
if (dat$param$birth_model=="poisson_birth_numbers")
{
  #note, this assumes natural_deaths before, births
  nBirths <- rpois(1,dat$param$poisson_birth_lambda )
}
#-----------------------------------------------
#constant birth rate
if (dat$param$birth_model == "constant_rate")
{
  no_alive <- length(which(is.element(dat$pop$Status,c(0:1))))
  nBirths  <-  rpois(1,no_alive*dat$param$constant_birth_rate)
}
#-----------------------------------------------
#constant birth numbers
if (dat$param$birth_model == "constant_number")
{
  nBirths <- dat$param$constant_birth_number
}

#-----------------------------------------------
#constant birth numbers
if (dat$param$birth_model == "constant_rate_spread_out")
{
  if(at==2 | !exists("dat$constant_rate_spread_out_new_adds")){
    daily_rate <- utilities_annual_prob_conversion(dat$param$constant_rate_spread_out,365)
    newvec <- dat$param$initial_pop*(1+ daily_rate)^(1:dat$param$n_steps)-dat$param$initial_pop
    unique_vector <- unique(round(newvec))
    unique_vector_adds <- unique_vector[2:(length(unique_vector))]-unique_vector[1:(length(unique_vector)-1)]
    newvec_integer <- round(newvec)
    match_vec <- match(unique_vector[-1],newvec_integer)
    new_adds <- rep(0,dat$param$n_steps)
    new_adds[match_vec] <- unique_vector_adds
    dat$constant_rate_spread_out_new_adds <- new_adds
  }
  
  nBirths <- dat$constant_rate_spread_out_new_adds[at-1]
  
}

  if (dat$param$birth_model == "constant_number_spread_out")
    {
      if(at==2 | !exists("dat$constant_number_spread_out_new_adds")){
        if(dat$param$births_per_year<=365){
          total_adds <- round((dat$param$n_steps/365)*dat$param$births_per_year)
          add_per_day <- total_adds/dat$param$n_steps
          cumulative_adds_by_day <- cumsum(rep(add_per_day,dat$param$n_steps))
          unique_add_vector <- (round(cumulative_adds_by_day))
          temp_match <- match(1:total_adds,unique_add_vector)
          new_adds <- rep(0,dat$param$n_steps)
          new_adds[temp_match] <- 1
          dat$constant_number_spread_out_new_adds <- new_adds
        }
        if(dat$param$births_per_year>365){
          total_adds <- round((dat$param$n_steps/365)*dat$param$births_per_year)
          add_per_event <- ceiling(total_adds/dat$param$n_steps)
          temp_diff <- (dat$param$n_steps*add_per_event)-total_adds
          add_index <- (dat$param$n_steps-(temp_diff/add_per_event))
          new_adds <- rep(0,length(dat$param$n_steps))
          new_adds[round(seq(1,dat$param$n_steps,length=add_index))] <- add_per_event
          new_adds[is.na(new_adds)] <- 0
          dat$constant_number_spread_out_new_adds <- new_adds
        }
      }
    nBirths <- dat$constant_number_spread_out_new_adds[at-1]
   }
#-----------------------------------------------
return(nBirths)

}
