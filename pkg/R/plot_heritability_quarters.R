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
plot_heritability_quarters<-function(model){
  #--------------------------------
  #plot quartery heritability
  titlevec=c("1st quarter","2nd quarter","3rd quarter","4th quarter")
  nsteps <- model$param[[1]]$n_steps
  cuts <- seq(1,nsteps,length=5)
  nsims=model$param[[1]]$nsims
  
  for(ii in 1:(length(cuts)-1)){
    inf_spvl <- vector('list',length=nsims)
    donor_spvl <- vector('list',length=nsims)
    for(jj in 1:nsims){
    ix <- which(model$pop[[jj]]$Time_Inf > cuts[ii] &
                  model$pop[[jj]]$Time_Inf<=cuts[ii+1])
    inf_spvl[[jj]] <- model$pop[[jj]]$LogSetPoint[ix]
    donor_spvl[[jj]] <- model$pop[[jj]]$Donors_LogSetPoint[ix]
  
  }#end nsims loop
    ivec <- unlist(inf_spvl)
    dvec <- unlist(donor_spvl)
    if(length(ivec)>=5){
      out <- try({hh <- (lm(ivec ~ dvec))},silent=TRUE)
    
        if(class(out)!="try-error"){
          plot(dvec,ivec,pch=16,col="blue",cex=.5,xlab="donor spvl",
               ylab="infectee spvl" )
          abline(hh,col="red")
          mtext(paste(titlevec[ii],"heritability (n=",length(ivec),"):",
                      format(hh$coefficients[2], digits=2)),side=3,line=2.7,col="blue")
        }
        if(class(out)=="try-error"){
          plot(1:10,1:10,type='n',axes=F)
          box()
          mtext(paste(titlevec[ii],"heritability: NA"), side=3,line=2.7,col="blue")
          text(3,5,"Insufficient data")
        }
    }
    
    if(length(ivec)<5){
      plot(1:10,1:10,type='n',axes=F)
      box()
      mtext(paste(titlevec[ii],"heritability: NA"), side=3,line=2.7,col="blue")
      text(3,5,"Insufficient data: n<5")
    }

  }#end cuts loop  
}#end fxn  