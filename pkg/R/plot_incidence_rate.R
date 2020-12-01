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
incidence_rate_plot<-function(model_obj){
  #plots incidence rate
  #input netsim object
  #output: plot of incidence rate by year
  
  
  model <- model_obj
  #model <- model_obj 
  total_sims <- model$param[[1]]$nsims
  result_list <- vector('list',length=total_sims)
  
  for(nsim in 1:total_sims)
  {
    popsumm_freq <- model$param[[nsim]]$popsumm_freq
    nsteps <- model$param[[nsim]]$n_steps
    
    if(nsteps>=365)
      steps_per_year <- floor(365/model$param[[nsim]]$popsumm_freq)
    else
      steps_per_year <- floor(nsteps/model$param[[nsim]]$popsumm_freq)
    
    sus <- model$epi$susceptibles[,nsim]
    inf <- model$epi$new_infections[,nsim]
    nsteps <- model$param[[nsim]]$n_steps
    nyears <- (nsteps/365)
    
    if(nyears<1)
    tli <- floor(seq(1,length(sus),length=2))
    if(nyears>=1)
    tli <- floor(seq(1,length(sus),length=floor(nyears)+1))   
    
    number_sus <- rep(NA_real_,length(tli)-1)
    total_new_inf <- rep(NA_real_,length(tli)-1)
    
    #browser()
    for(ii in 1:(length(tli)-1))
    {
      total_new_inf[ii] <- sum(inf[(tli[ii]+1):(tli[ii+1])])
      
      if(popsumm_freq>1){
        number_sus[ii] <- mean(sus[(tli[ii]+1):(tli[ii+1])])
      }
      #note: this gives exact same results as above
      if(popsumm_freq==1){
        number_sus[ii] <- sum(sus[(tli[ii]+1):(tli[ii+1])])/365
      }
    }
    
    if(sum(number_sus)==0){next}
    
    # scalar accounts for round-off error
    scalar<-365/(diff(tli)*popsumm_freq)
    inc <- scalar*100*total_new_inf/number_sus
    inc[number_sus==0]<-0
    result_list[[nsim]] <- inc
    
  }
  result_mat <- do.call(rbind,result_list)
  ymax=max(unlist(result_mat),na.rm=T)
  for(jj in 1:total_sims)
  {
    #browser()
    if(jj==1){
      if(nsteps>=365){
        plot(1:nyears,result_list[[jj]],type='o',ylim=c(0,ymax),ylab=NA,xlab="years",
             pch=1,lty=2,col="darkblue",axes=F)
        axis(1,at=1:nyears,labels=1:nyears)
      }
      
      if(nsteps<365){
        plot(nyears,result_list[[jj]],type='o',ylim=c(0,ymax),ylab=NA,xlab="years",
             pch=16,lty=2,col="darkblue",axes=F)
        axis(1,at=nyears,labels=round(nyears,1))
      }
      axis(2);box()
    }else{
      lines(1:nyears,result_list[[jj]],type='o',col="darkblue",ylim=c(0,max(inc)),pch=1,lty=2)
    }
  }
  lines(1:nyears,colMeans(result_mat),type='o',pch=16,lwd=2,col="darkblue")
  mtext("incidence rate per 100 person years",side=3,line=2.7,col="blue")
}
