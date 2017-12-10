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
summary_end_epidemic_values<-function(model,var,nyears){
  
  #for each simulation in model (as charater string), 
  #returns the mean value of
  #specified popsumm variable ("var") for the last number of specified years ("nyears")
  #note: only processes one model at a time
  #ex, summary_end_epidemic_values("evomodel","prevalence",5)
  
  
  model <- get(model)
  params <- model$param[[1]]
  nsims <- params$nsims
  nsteps <- params$n_steps
  popsumm_names <- names(model$popsumm[[1]])
  if(!is.element(var,popsumm_names)){
    return(print("variable not model output..typo?"))
  }
  if(nyears >= (nsteps/365)){return(NA)}
  popfr <- params$popsumm_frequency
  if(popfr>1){
    index_end <- floor(nsteps/popfr)
    index_start <-  floor(index_end - round((nyears*365)/popfr))
  }
  else{
    index_end <- nsteps
    index_start <- (nsteps-(nyears*365))
  }
  if(index_start<1){return(print("bad start_index value"))}
  if(index_end>length(model$popsumm[[1]][[var]])){return(print("bad end_index value"))}
  
  index <- index_start:index_end
  out_vector <- rep(NA_real_,length=nsims)
  for(ii in 1:length(out_vector)){
    data_vec <- model$popsumm[[ii]][[var]][index]
    if(all(is.na(data_vec))){out_vector[ii]=NA_real_}
    else{out_vector[ii] <- mean(data_vec,na.rm=T)}
  }
  return(out_vector)
}


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
  plot(1:length(model_names),seq(ymin,ymax,length=length(model_names)),
       type='n',axes=F,ylab=var,xlab="simulation")
  axis(1,at=1:length(model_names),labels=1:length(model_names))
  axis(2)
  box()
  for(ii in 1:length(model_names)){
    points(rep(ii,length(out_list[[ii]])),out_list[[ii]],pch=1,col="blue")
    points(ii,mean(out_list[[ii]]),pch=16,col="red",cex=1.3)
  }
}

