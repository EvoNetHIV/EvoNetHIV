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
plot_time_to_removal_by_spvl<- function(model,type="aids"){
  #plots histogram of times to death for infecteds either
  #from aids deaths or non-aids deaths (all sims combined)
  
  nsims <- model$param[[1]]$nsims
  nsteps <- model$param[[1]]$n_steps
  histvec <- NULL
  spvlvec <- NULL
  for(ii in 1:nsims){
    mod <-model$pop[[ii]]
    if(type=="aids"){
      ix1 <- which(mod$Status==-2 & mod$Time_Inf>0 &
                     mod$Time_Inf < nsteps/2)
      titlevec <- "time to aids death from infection"
    }
    if(type!="aids"){
      ix1 <- which((mod$Status==-1 |mod$Status==-1.5) & 
                     mod$Time_Inf>0 &
                     mod$Time_Inf < nsteps/2)
      titlevec <- "time to non-aids death from infection"
    }
    
    if(length(ix1)>0){
      time_to_death <- mod$Time_Death[ix1]-mod$Time_Inf[ix1]
      histvec<- c(histvec,time_to_death)
      spvlvec <- c(spvlvec,mod$LogSetPoint[ix1])
    }  
  }
  if(is.null(histvec)){
    text(5,5,"no aids deaths")
    return(invisible(NULL))
  }
  
  plot(spvlvec,histvec/365,pch=16,ylab="years",
       xlab="log10 spvl",col="blue",cex=.7)
  title(titlevec)
  mtext("when time of infection: 0 > t > nsteps/2",side=3,line=0,
        cex=.75)
  return(invisible(NULL))
}

