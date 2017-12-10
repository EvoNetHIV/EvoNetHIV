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
plot_time_to_removal<- function(model,type="aids"){
  #plots histogram of times to death for infecteds either
  #from aids deaths or non-aids deaths (all sims combined)
  
  nsims <- model$param[[1]]$nsims
  nsteps <- model$param[[1]]$n_steps
  histvec <- NULL
  for(ii in 1:nsims){
    mod <-model$pop[[ii]]
    still_alive <-length(which(mod$Status==1 & 
                        !is.na(mod$Time_Inf) &
                        mod$Time_Inf>0 &
                        mod$Time_Inf < nsteps/2))
    
        if(type=="aids"){
      ix1 <- which(mod$Status==-2 & !is.na(mod$Time_Inf) &
                     mod$Time_Inf>0 &
                     mod$Time_Inf < nsteps/2)
      titlevec <- "time to aids death from infection"
 
    }
    if(type!="aids"){
      ix1 <- which((mod$Status==-1 |mod$Status==-1.5) &
                     !is.na(mod$Time_Inf) &
                     mod$Time_Inf>0 &
                     mod$Time_Inf < nsteps/2)
      titlevec <- "time to non-aids death from infection"
    }
    
    if(length(ix1)>0){
      time_to_death <- mod$Time_Death[ix1]-mod$Time_Inf[ix1]
      histvec<- c(histvec,time_to_death)
    }  
  }
  if(is.null(histvec)){
    text(5,5,"no aids deaths")
    return(invisible(NULL))
  }
  
  hist(histvec/365,col="blue",xlab="years to death from infection",
       main=paste(titlevec,"; n = ",length(histvec),sep=""))
  mtext(paste("mean = ",round(mean(histvec/365),1),
              "; still alive =",still_alive),side=3,line=-.2,cex=.6)
  mtext(paste("when time of infection: 0 > t & t <", round(nsteps/2/365),"years"),
        side=3,line=.75,
        cex=.6)
  return(histvec)
}


