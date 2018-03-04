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

  mm <- match(ix,dat$attr$id)
  
  #temp qaqc
  if(any(is.na(mm))){browser()}
  
  dat$attr$age[mm]=dat$pop$age[ix]
  dat$attr$age_cat[mm] = c(1, 2)[findInterval(x = dat$pop$age[ix], vec = c(14, 25))]
  
  if(!is.null(dat[['nw']])){
    network::set.vertex.attribute(x = dat$nw, attr = "age",
                                value = dat$attr$age)
  }
  
  return(dat)  
}
