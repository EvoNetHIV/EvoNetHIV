summary_popsumm_mat <- function(model){
  popsumm=model$popsumm
  nsims=length(popsumm)
  vars=names(popsumm[[1]])
  outlist=vector('list',length=length(vars))
  outlist[[1]]$timestep$days = popsumm[[1]]$timestep
  outlist[[1]]$timestep$years = popsumm[[1]]$timestep/365
  
  for(jj in 1:length(vars)){
    if(vars[jj]=="timestep"){next}
    tempmat=NULL
    for(ii in 1:nsims){
      tempmat=rbind(tempmat,popsumm[[ii]][[vars[jj]]])
    }
    outlist[[jj]]$values=tempmat
    if(all(is.na(as.numeric(tempmat)))){
      outlist[[jj]]$min_max=NA
    }else{
      mn = 0.95 * min(as.numeric(tempmat),na.rm=T)
      mx = 1.05 * max(as.numeric(tempmat),na.rm=T)
      outlist[[jj]]$min_max=c(mn,mx)
    }
    
    outlist[[jj]]$nsims=nsims
    
    if(all(is.na(as.numeric(tempmat)))){    
      outlist[[jj]]$mean_values = NA
    }else{
      outlist[[jj]]$mean_values = colMeans(tempmat,na.rm=T)
    }  
  }  
  names(outlist)=vars
  model$popsumm_mats=outlist
  return(model)
}

