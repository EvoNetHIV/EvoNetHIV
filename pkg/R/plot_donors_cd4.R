
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

#called in plot popsumm
plot_donors_cd4<-function(model)
{
pop <- model$pop[[1]]
ix <- which(!is.na(pop$Donors_CD4))
if(length(ix) == 0) { return() }
donors_cd4 <- pop$Donors_CD4[ix]
donors_cd4_table <- table(donors_cd4) 
donors_cd4_vec  <- round(donors_cd4_table/length(ix),2)
barplot(donors_cd4_vec,names.arg=names(donors_cd4_table),main=NULL)
mtext("Percent infection by donors CD4",side=3,line=2,col="blue")
}
