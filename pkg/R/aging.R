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

###################################################################
# 
#' @export
 aging <- function(dat,at){
  
  ix <- which(is.element(dat$attr$Status, c(0:1)))
  dat$attr$age[ix] <- round( dat$attr$age[ix] + (1/365) ,5 )
  dat$attr$sqrt_age[ix] <- sqrt(dat$attr$age[ix])
  

  if(!is.null(dat[['nw']])){
    network::set.vertex.attribute(x = dat$nw, attr = "age",
                                value = dat$attr$age)
    network::set.vertex.attribute(x = dat$nw, attr = "sqrt_age",
                                  value = sqrt(dat$attr$age))
    
  }
  return(dat)  
}
