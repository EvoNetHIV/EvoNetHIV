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
  
  ix <- which(is.element(dat$pop$Status, c(0:1)))
  dat$pop$age[ix] <- round( dat$pop$age[ix] + (1/365) ,5 )
  dat$pop$sqrt_age[ix] <- sqrt(dat$pop$age[ix])
  
  mm <- match(ix,dat$attr$id)
  
  #temp qaqc
  if(any(is.na(mm))){browser()}
  
  dat$attr$age[mm]=dat$pop$age[ix]
  dat$attr$sqrt_age[mm]=sqrt(dat$pop$age[ix])
  
  if(!is.null(dat[['nw']])){
    network::set.vertex.attribute(x = dat$nw, attr = "age",
                                value = dat$attr$age)
    network::set.vertex.attribute(x = dat$nw, attr = "sqrt_age",
                                  value = sqrt(dat$attr$age))
    
  }
  return(dat)  
}
