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
plot_end_epidemic_values<- function(model_names,var,nyears){
  #plots output of fxn summary_end_epidemic_values
  #for each model in vector "model_names" (vector of character string), 
  #will plot the mean
  #values of specified popsumm variable "var" for the last 
  #number of specified years of model run ("nyears)
  #ex,
  # plot_end_epidemic_values(model_names=c("evomodel_1","evomodel_2","evomodel_3"),
  #        var="prevalence",nyears=5)
  
  out_list <- vector('list',length=length(model_names))
  for(ii in 1:length(model_names)){
    out_list[[ii]] <- summary_end_epidemic_values(model=model_names[ii],
                                                  var=var,
                                                  nyears=nyears)
  }  
  ymax=max(unlist(out_list),na.rm=T)
  ymin=min(unlist(out_list),na.rm=T)
  if(length(model_names)==2){
    ix=c(1.2,1.8)
    }else{ix=1:length(model_names)}
  
  plot(1:length(model_names),seq(ymin,ymax,length=length(ix)),
       type='n',axes=F,ylab=var,xlab="simulation")
  axis(1,at=ix,labels=model_names)
  axis(2)
  box()
  for(ii in 1:length(ix)){
    points(rep(ix[ii],length(out_list[[ii]])),out_list[[ii]],pch=1,col="blue")
    points(ix[ii],mean(out_list[[ii]]),pch=16,col="red",cex=1.3)
  }
}
