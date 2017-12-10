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
plot_age_hist<-function(model)
{
  #plots age distribution from dat$age_list object which is 
  #output data structure e.g, evomodel$age_list; dat$age_list is
  #initialized in "initialize_evonet_misc" and filled in 
  #"summary_misc")
  
  yearvec=c(0,
            round((model$param[[1]]$n_steps*c(.25,.5,.75,1))/365,1))
  
  age_list <- model$age_list
  if(is.null(age_list[[1]])){return(invisible(NULL))}
  for(jj in 1:5){
    out<-NULL
    for(ii in 1:length(age_list)){
      out<- c(out,unlist(age_list[[ii]][[jj]]))
    }
  hist(out,
       breaks=length(model$param[[1]]$min_age:model$param[[1]]$max_age),
       col="blue",main=NA,xlab="age")
  mtext(paste("age distribution at year",yearvec[jj]),side=3,line=2.7,col="blue")
  }
}