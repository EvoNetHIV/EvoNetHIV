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
plot_vl_trajectories_aim2<- function(model,sim,outpath,name)
{
  
  if(is.null(model$vl_list[[sim]])){
    cat("\n viral load list not saved; set evoparams$save_vl_list=TRUE to plot VL trajectories")
    return(invisible(NULL))
  }
  
  vl_df <- do.call(rbind,model$vl_list[[sim]])
  
  agents <- unique(vl_df[,1]) 
  if(is.null(name)){
    name="vl_trajectories.pdf"
  }else{
    name=paste(name,"_vl_traj.pdf",sep="")    
  }
  pdf(file.path(outpath,name),width=12,height=10)
  par(mfrow=c(2,2))
  
  for(ind in agents)
  {  
    vl_ix  <- which(vl_df[,1]==ind)
    if(length(vl_ix>0)){
      seq1<- seq(0,model$param[[sim]]$n_steps/365,length=10)
      seq2<- seq(-5,8,length=10)
      
      plot(seq1,seq2,type='n', xlab="years",ylab="viral load")
      lines(vl_df[vl_ix,5]/365,log10(vl_df[vl_ix,2]),lwd=2,col="blue")
      axis(2,col="blue")
      par(new=T)
      plot(seq1,seq(0,5,length=length(seq1)),type='n', xlab=NA,ylab=NA,axes=F)
      lines(vl_df[vl_ix,5]/365,(vl_df[vl_ix,3]),lwd=2,col="green")
      axis(4,col="green")
      
      mtext(paste("viral load and cd4  data for agent",ind),side=3)
    }
  }
  dev.off()
}
