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
vital_initial_age_dist <- function(age.range,popsize,age_dist)
{  
  #################################################3
  #Description:
  #called in new_additions_fxn() for initial model setup
  
  #determines distribution of ages across initial population,
  #based on “initial_age_dist_method” parameter value:
  #input variables: min_age max_age,initial_pop, male_age_dist   
  
  #output variables: age_vec
  
  ##################################################
  
  #if(sum(age_dist)!=1){stop("age distribution does not sum to 1.0")}
  cum_prob_ages <- c(0,cumsum(age_dist)/max(cumsum(age_dist))) 
  age_vec       <- (as.numeric(as.character( cut(runif(popsize),
                      breaks= cum_prob_ages ,labels=age.range) )) )

  #add fractional component to age distribution
  age_vec <- age_vec + (sample(0:364,length(age_vec),replace=T)/365)
  
  #qaqc
  if(any(is.na(age_vec))){stop("something funny with ages of initial population")}
  if(any(age_vec<min(age.range))){stop("some inital ages are < min(age.range)?")}
  
  return(age_vec)
  
}
#---------------------------------------------------------------------
