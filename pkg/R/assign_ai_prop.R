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
assign_ai_prop <- function(dat, ai.vec) {
  
  if(any(ai.vec == 1)) {
    ai.vec[ai.vec == 1] <- sapply(ai.vec[ai.vec == 1], function(x) {
      temp.prob <- rnorm(1, mean = dat$param$mean_prop_acts_AI, sd = dat$param$sd_prop_acts_AI)
      if(temp.prob < 1/(dat$param$mean_sex_acts_day * 90)) {
        ai.vec[x] <- 1/(dat$param$mean_sex_acts_day * 90)
      } else {
        if(temp.prob > 1) {
          ai.vec[x] <- 1
        } else {
          ai.vec[x] <- temp.prob
        }
      }
    })
  }
  
  return(ai.vec)
  
}