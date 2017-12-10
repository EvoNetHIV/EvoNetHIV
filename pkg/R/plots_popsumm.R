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

#----------------------------------
#' @export
plots_popsumm_ymin_ymax <- function(variable,plot_type,popsumm)
{
#  
  
if(plot_type=="line_cumul"){    
  ymax <- max(unlist(lapply(popsumm,function(x) cumsum(x[[variable]]))),na.rm=T)
  ymin <- min(unlist(lapply(popsumm,function(x) cumsum(x[[variable]]))),na.rm=T)
}else if(variable=="susceptibles"){
  ymax1 <- max(unlist(lapply(popsumm,function(x) (x[[variable]]))),na.rm=T)
  ymin1 <- min(unlist(lapply(popsumm,function(x) (x[[variable]]))),na.rm=T)
  ymax2 <- max(unlist(lapply(popsumm,function(x) (x[["total_infections_alive"]]))),na.rm=T)
  ymin2 <- min(unlist(lapply(popsumm,function(x) (x[["total_infections_alive"]]))),na.rm=T)
  ymax= max(c(ymax1,ymax2))
  ymin= max(c(ymin1,ymin2))
}else{
  ymax <- max(unlist(lapply(popsumm,function(x) x[[variable]])),na.rm=T)
  ymin <- min(unlist(lapply(popsumm,function(x) x[[variable]])),na.rm=T)    
}
return(c("ymin"=ymin,"ymax"=ymax))
}

#--------------------------------
#' @export
plots_popsumm_all_NA_flag <- function(variable,popsumm){
  na_vec <- lapply(1:length(popsumm),function(x) all(is.na(popsumm[[x]][[variable]]))) 
  na_vec <- unlist(na_vec)
  if(all(na_vec==TRUE)){return(TRUE)}else{return(FALSE)}
}

#--------------------------------
#' @export
plots_popsumm_all_NA_plot<-function(variable){
  plot(1:10,1:10,ylab=NA,xlab=NA,type='n',axes=F)
  box()
  mtext("All values NA or zero",side=3,line=-4)
  mtext(variable,side=3,line=1)
}

#--------------------------------
#' @export
plots_popsumm_overlay_lines<-function(nsims,over_var,params,popsumm,popsumm_freq,
                                      cumul=F,color1="firebrick1",color2="red3"){
  #browser()
  temp_list <- vector('list',length=nsims) 
  for(kk in 1:nsims){
    
    if(popsumm_freq==1)
      xvector <- (1:params$n_steps)/365
    else
      xvector <- c(1,seq(popsumm_freq,params$n_steps,by=popsumm_freq))/365
    if(cumul==T){yvector=cumsum(popsumm[[kk]][[over_var]])
    }else{yvector=popsumm[[kk]][[over_var]]}
    
    if(nsims==1){lty_type=1}else{lty_type=2}
     lines(xvector,yvector,
           col=color1,lty=lty_type)
    temp_list[[kk]]<-yvector
    
  }
   if(nsims>1){
   temp_mat=do.call(rbind,temp_list)
   if(!is.null(temp_mat)){
   temp_means=colMeans(temp_mat,na.rm=T)
   lines((xvector),temp_means,lwd=2,col=color2)
   }
   }
  
  if(color2=="red3"){
    mtext(paste(over_var),side=3,line=1.5,cex=.9,col=color1)
  }else{mtext(paste(over_var),side=3,line=.15,cex=.9,col=color1)}
}

#--------------------------------
#' @export
plots_popsumm_mean_value<-function(datlist,xvector){
  
  data_matrix<-do.call(rbind,datlist)
  mean_values <- colMeans(data_matrix,na.rm=T)
  lines(xvector,mean_values,col="darkblue",lwd=2)
  
}
#--------------------------------
#' @export
plots_popsumm_mean_value_loess<-function(datlist,xvector,variable){
  data_matrix<-do.call(rbind,datlist)
  mean_values <- colMeans(data_matrix,na.rm=T)
  
  navec<- is.na(mean_values)
  
  if(any(navec)){
    if(length(which(navec==FALSE))<5){return(invisible(NULL))}
    lines(lowess(xvector[!navec],mean_values[!navec], f=0.4),col="darkblue",lwd=2,lty=1)        
  }else{lines(lowess(xvector,mean_values, f=0.4),col="darkblue",lwd=2,lty=1)}
  
  
}

#--------------------------------
#' @export
plots_popsumm_plotting<-function(variable,plot_type,nsim,popsumm,xvector,min_max,
                                 loessvec,max_pts_rep,descript=NULL){
  #if(variable=="natural_deaths"){browser()}
 if(plot_type=="line_raw"){
     yy=popsumm[[nsim]][[variable]]
     type="l"
 } 
 if(plot_type=="line_cumul"){
     yy=cumsum(popsumm[[nsim]][[variable]])
     type="l"
  }
 if(plot_type=="points"){
   yy=popsumm[[nsim]][[variable]]
   type="p"
 }


 if(plot_type!="points")
 {
   parlist=list(x=xvector,y=yy,type=type,xlab="years",ylab="",col="blue",
                main="",ylim=c(min_max["ymin"],min_max["ymax"]),lty=2)
 }
  
  if(plot_type=="points")
  {
    #browser()
    data_pts_index=which(!is.na(yy))
    
    if(length(data_pts_index)>max_pts_rep)
      sampvec=sample(data_pts_index,max_pts_rep)
    else
      sampvec=data_pts_index
    
    if(all(is.na(yy))){return(invisible(NULL))}
    
    parlist=list(x=xvector[sampvec],y=yy[sampvec],type=type,xlab="years",ylab="",col=nsim,
                 main="",ylim=c(min_max["ymin"],min_max["ymax"]),
                 xlim=c(1/365,xvector[length(xvector)]),pch=1)
  }
  
 
 if(nsim==1)do.call(plot,parlist)
 if(nsim>1)do.call(lines,parlist)
  
  if(nsim==1){  
  mtext(paste(variable),line=2.7,side=3,cex=.9,col="blue")
  mtext(paste(descript),line=1.5,side=3,cex=.7,col="blue")
  }
  
 if(variable %in% loessvec){
   navec<- is.na(yy)
   if(any(navec)){
     lines(lowess(xvector[!navec],yy[!navec], f=0.4),col=nsim,lwd=1,lty=2)        
   }else{lines(lowess(xvector,yy, f=0.4),col=nsim,lwd=1,lty=2)}
 }
 return(yy)
}

#--------------------------------------------------
#' @export
plots_popsumm_internal<-function(model,outpath,name,
                           nw_stats,max_points_rep,
                           popsumm_frequency,pdf)
{
  
  if(model$param[[1]]$n_steps<100){
    cat("\n plotting function 'plots_popsumm' requires at least 100 timesteps \n")
    return(invisible(NULL))
  }
  
    
  if(is.null(name)){name<-"popsumm_figures"}
  final_name <- paste(name,".pdf",sep="")
  
  if(pdf){
  pdf(file.path(outpath,final_name ),
      8.5,11)
  }
  
  par(mfrow=c(3,2),mgp=c(2.5,1,0))
  if(model$param[[1]]$VL_Function=="aim3"){aim3=T}else{aim3=F}
  if(model$param[[1]]$fast_edgelist==TRUE){fast_el=T}else{fast_el=F}
  plot_fxns <- summary_popsumm_fxns(model$param[[1]]$generic_nodal_att_values,aim3,
                                    fast_el,model$param[[1]])

  data <- model$popsumm
  vars <- names(model$popsumm[[1]])
  plot_type_vec <- lapply(plot_fxns,function(x) x$plot_type)
  plot_type_vec <- unlist(plot_type_vec)
  loess_vec  <- names(unlist(lapply(plot_fxns,function(x) x$loess)))
  overlay_vec <- unlist(lapply(plot_fxns,function(x) x$overlay))
  overlay_vec2 <- unlist(lapply(plot_fxns,function(x) x$overlay2))
  ymin_vec<- names(unlist(lapply(plot_fxns,function(x) x$ymin)))
  description<- unname((unlist(lapply(plot_fxns,function(x) x$description))))
  
  nsims<-length(data)
  
  if(popsumm_frequency==1)
    xx <- (1:model$param[[1]]$n_steps)/365
  else
    xx <- c(1,seq(popsumm_frequency,model$param[[1]]$n_steps,by=popsumm_frequency))/365
  
   #xx<-1:model$param[[1]]$n_steps/365
  
  #loop over variables
  for(ii in 1:length(vars))
  {
   print(vars[ii])
   print(ii)
   if(ii==3){
     incidence_rate_plot(model)
     plot_heritability_quarters(model)
    }
   
   if(vars[ii]=="timestep"){next}
   if(vars[ii]=="Perc_0_drug_muts"){plot_aim3_drug_muts(model)}
    if(vars[ii]=="Perc_0_drug_muts"){next}
    if(vars[ii]=="Perc_1_drug_muts"){next}
    if(vars[ii]=="Perc_2_drug_muts"){next}
   if(vars[ii]=="Perc_3_drug_muts"){next}
   if(vars[ii]=="Perc_4_drug_muts"){next}
    if(vars[ii]=="Perc_1+_drug_muts"){next}
    if(vars[ii]=="Perc_2+_drug_muts"){next}
    if(vars[ii]=="Perc_3+_drug_muts"){next}
    if(vars[ii]=="Perc_4+_drug_muts"){next}
    if(vars[ii]=="Perc_All_5_drug_muts"){next}
    
    if(vars[ii]=="Perc_0_drug_muts_total_pop"){next}
    if(vars[ii]=="Perc_1_drug_muts_total_pop"){next}
    if(vars[ii]=="Perc_2_drug_muts_total_pop"){next}
    if(vars[ii]=="Perc_3_drug_muts_total_pop"){next}
    if(vars[ii]=="Perc_4_drug_muts_total_pop"){next}
    if(vars[ii]=="Perc_All_5_drug_muts_total_pop"){next}
    if(vars[ii]=="Perc_1+_drug_muts_total_pop"){next}
    if(vars[ii]=="Perc_2+_drug_muts_total_pop"){next}
    if(vars[ii]=="Perc_3+_drug_muts_total_pop"){next}
    if(vars[ii]=="Perc_4+_drug_muts_total_pop"){next}
    
    
   if(vars[ii]=="no_treated_undetectable"){next}    
   if(vars[ii]== "total_1+_drug_muts"){next}
   if(vars[ii]== "total_3+_drug_muts"){next}
    if(vars[ii]== "drug_muts_1+"){next}
    if(vars[ii]== "drug_muts_3+"){next}
    
    if(ii==5){plot_donors_cd4(model)}
      
   if(ii==12){
     plot_time_to_removal(model,"aids")
     plot_time_to_removal(model,"nonaids")
     plot_time_to_removal_by_spvl(model,"aids")
     plot_time_to_removal_by_spvl(model,"nonaids")
    }
   if(ii==13){plot_age_hist(model)}  
   if(ii==13){plot_relationship_duration(model)}
    

   if(!nw_stats){
       if(vars[ii] %in% c("no_edges","mean_degree","mean_degree_infected",
          "no_nodes_degree_0","no_nodes_degree_1","no_nodes_concurrent" )){
        next
      }}
    
    all_na_flag <- plots_popsumm_all_NA_flag(vars[ii],model$popsumm)
    if(all_na_flag){
      plots_popsumm_all_NA_plot(vars[ii])
      next
    }
    ymin_ymax <- plots_popsumm_ymin_ymax(vars[ii],plot_type_vec[ii],model$popsumm)
    #aim3 correction
    if(ymin_ymax[["ymin"]]=="-Inf"){ymin_ymax[["ymin"]] <- 0}
    if(!(vars[ii] %in% ymin_vec)){ymin_ymax["ymin"] <- 0}
    
    #loop over replicates
    data_list <- vector('list',length=length(data))
    for(jj in 1:length(data)){
        yy<- plots_popsumm_plotting(vars[ii],plot_type_vec[ii],nsim=jj,
                               model$popsumm,xx,ymin_ymax,loess_vec,
                               max_points_rep,descript=description[ii])
      data_list[[jj]]<- yy
      
        
    }
    
    if(vars[ii] %in% names(overlay_vec)){
      index <- which(names(overlay_vec)==vars[ii])
      plots_popsumm_overlay_lines(length(data),overlay_vec[index],model$param[[1]],
                                  model$popsumm,popsumm_frequency)
    }
    
    if(vars[ii] %in% names(overlay_vec2)){
      index <- which(names(overlay_vec2)==vars[ii])
      plots_popsumm_overlay_lines(length(data),overlay_vec2[index],model$param[[1]],
                                  model$popsumm,popsumm_frequency,color1="green",color2="darkgreen")
    }
    
    if(!(vars[ii] %in% loess_vec)){
    plots_popsumm_mean_value(data_list,xx)
    }
    
    
    if(vars[ii] %in% loess_vec){
      plots_popsumm_mean_value_loess(data_list,xx,vars[ii])
    }
    
    
  }
  if(pdf){
  dev.off()
  }  
}

#--------------------------------------------------
#' @export
plots_popsumm_deprecated <- function(model,outpath,name=NULL,
                          nw_stats=TRUE,max_points_rep=50,
                          popsumm_frequency){
  
  plots_popsumm_internal(model=model,outpath=outpath,name=name,
                                   nw_stats=nw_stats,max_points_rep=max_points_rep,
                                   popsumm_frequency,pdf=FALSE)
  
  plots_popsumm_internal(model=model,outpath=outpath,name=name,
                         nw_stats=nw_stats,max_points_rep=max_points_rep,
                         popsumm_frequency,pdf=TRUE)
}
