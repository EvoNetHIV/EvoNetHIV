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
infection_history_vl_cd4_plot <- function(ind,vl_df)
{

#plot vl/cd4 progression

    vl_ix  <- which(vl_df[,1]==ind)
    if(length(vl_ix>0))
    {
      plot(vl_df[vl_ix,4],log10(vl_df[vl_ix,2]),type='l',xlab="timestep",ylab="viral load",lwd=2,col=1)
      par(new=T)
      plot(vl_df[vl_ix,4],vl_df[vl_ix,3],type='l',xlab="",ylab="",lwd=2,col=2,axes=F,ylim=c(0,5))
      axis(4)
      mtext("CD4 category",side=4,line=1)
      title(paste("viral load / CD4 data for agent",ind))
    }
    
}