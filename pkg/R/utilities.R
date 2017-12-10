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
#functions:
#3) apportion.lr
#4) annual_prob_conversion
#5) annual_mortality_conversion

#---------------------------------------------------------------------

### apportion.lr: apportion a vector with the least-remainder method
#from steve g's mardham code, is based on "Hare quota" framework
#for apportioning parliamentary seats based on vote totals

#note: it doesn't actually work well, as it distributes remainders identically
# each iteration: eg, 3 categories, 5 objects will always be distributed:
# cat1=2 objects, cat2=2 objects, cat3 = 1 object
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
utilities_apportion.lr <- function(vector.length,values,proportions,shuffled=F) {
  # lr = "largest remainder" method
  # COMMENT
  
  if (vector.length != round(vector.length)) 
    stop ("argument vector.length must be a positive integer")
  
  if (vector.length <= 0) 
    stop ("argument vector.length must be a positive integer")
  
  if (is.vector(values) == FALSE) 
    stop ("argument values must be a vector")
  
  if (!(length(proportions)==length(values) && sum(proportions)==1)  &&
        (!(length(proportions)==length(values)-1 && sum(proportions)<=1 && sum(proportions)>=0)))
    stop ("error in proportions length or proportions sum")
  
  if (length(proportions)==length(values)-1) proportions <- c(proportions, 1-sum(proportions))
  
  result <- rep(NA,vector.length)
  exp.nums <- proportions * vector.length
  counts <- floor(exp.nums)
  remainders <- exp.nums - counts
  leftovers <- vector.length - sum(counts)
  
  if (leftovers > 0) {
    additions <- order(remainders,decreasing=T)[1:leftovers]
    counts[additions]   <- counts[additions]+1
  }
  
  result <- c(rep(values,counts))
  
  if (shuffled==T) result <- sample(result,length(result))
  
  return(result)
}
#apportion.lr(3,1:2,c(.5,.5),shuffled=F) 
#apportion.lr(10000,1:10,rep(.1,10))

#---------------------------------------------------------------------
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
utilities_annual_prob_conversion <- function(annual_probs,timesteps_per_year) {
  1-(1-annual_probs)^(1/timesteps_per_year)
}  

#---------------------------------------------------------------------

#this is called in "parameter_list.R" to convert user inputted ASMRs to 
#mortality per timestep
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
utilities_annual_mortality_conversion <- function(.annual_probs,age_range,.timesteps_per_year)
{  

    if( length(.annual_probs)!=length(age_range) )
  {stop("user-inputted mortality rates not same length as age range (extra or missing values")}
  
  utilities_annual_prob_conversion(.annual_probs, .timesteps_per_year)
}

