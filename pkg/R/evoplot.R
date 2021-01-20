


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



summary_epi_mat <- function(model){
  epi <- model$epi
  vars <- names(epi)
  timestep_yrs <- epi$timestep[,1]/365
  tvar <- c("prevalence","new_infections", "susceptibles", "total_infections_alive", 
            "births", "aids_deaths", "natural_deaths", "aged_out")
  ix <- which(vars %in% c(c("s.num","i.num", "num"),tvar))
  vars <- vars[-ix]
  vars <- c(tvar,vars)
  bad_vars <- NULL
  for(ii in 1:length(vars)){
     #print(c(ii,vars[ii]))
    if(all(is.na( epi[[vars[ii]]][,1]))){
      bad_vars <- c(bad_vars,vars[ii])
      next
    }
    
    for(jj in 1:ncol(epi[[vars[ii]]])){
      ix <- which(epi[[vars[ii]]][,jj]== -999 | is.infinite(epi[[vars[ii]]][,jj]) )
      if(length(ix)>0){
        epi[[vars[ii]]][ix,jj]=NA
      }#end if block
    }#end jj loop
  }#end ii loop
  
  ix <- which(vars %in% bad_vars)
  
  if(length(ix)>0){
  vars <- vars[-ix]
  }
  
  epi_list <- vector("list",length=length(vars))
  names(epi_list) <- vars
  for(ii in 1:length(vars)){
    #if(vars[ii]=="percent_donor_acute"){browser()}
    epi_list[[ii]]$values <- epi[[ vars[ii] ]]
    if(all(is.na(epi_list[[ii]]$values))){
      epi_list[[ii]]$min_max <- c(NA,NA)
    }else{
    epi_list[[ii]]$mean_values <- rowMeans(epi_list[[ii]]$values,na.rm=T)
    mn <- 0.95 * min(unlist(epi_list[[ii]]$values),na.rm=T)
    mx <- 1.05 * max(unlist(epi_list[[ii]]$values),na.rm=T)
    epi_list[[ii]]$min_max <- c(mn,mx)
    }
  }
  
  model$epi_list <- epi_list
  return(model)
}#end fxn



#' @export
#' 
#' 
evoplot<-function(model,outpath=getwd(),name=NULL,nw_stats=TRUE,max_points_rep=NULL,
                  epi_frequency=NULL,variables=NULL,main=NULL,...){
  
  
  for(ii in 1:2){
    evoplot_internal(model=model,save=c(T,F)[ii],name=name,outpath=outpath,
                     variables=variables,main=main)
  }
   
}



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
evoplot_internal <- function(model,save=TRUE,name=NULL,outpath=getwd(),
                             variables=NULL,main=NULL){
  
  not_plotted=c(
    "percent_suppressed",
    "Perc_1_drug_muts_total_pop",
    "Perc_2_drug_muts_total_pop",
    "Perc_3_drug_muts_total_pop",
    "Perc_4_drug_muts_total_pop")
  
  
  #
  
  overlay_vars1=c(
    "susceptibles"="total_infections_alive")
  
  overlay_vars2=NULL
  if(model$param[[1]]$start_treatment_campaign[1]<5e5){
    overlay_vars2=c(
      "inf_under30"="inf_30to50")
  }
  
  overlay_vars3=NULL
  if(model$param[[1]]$model_sex=="hetero"){
    overlay_vars3=c("treated_inf_men"="treated_inf_women","inf_men"="inf_women")
    
  }
  #aim3
  overlay_vars4=NULL
  if(model$param[[1]]$VL_Function=="aim3"){
    overlay_vars4=c("Perc_1+_drug_muts"="Perc_1_drug_muts",
                    "Perc_2+_drug_muts"="Perc_2_drug_muts",
                    "Perc_3+_drug_muts"="Perc_3_drug_muts",
                    "Perc_4+_drug_muts"="Perc_4_drug_muts",
                    "Perc_1+_drug_muts_total_pop"="Perc_1_drug_muts_total_pop",
                    "Perc_2+_drug_muts_total_pop"="Perc_2_drug_muts_total_pop",
                    "Perc_3+_drug_muts_total_pop"="Perc_3_drug_muts_total_pop",
                    "Perc_4+_drug_muts_total_pop"="Perc_4_drug_muts_total_pop" )
  }
  
  overlay_vars5=NULL
  if(model$param[[1]]$model_sex=="hetero" & model$param[[1]]$start_treatment_campaign[1]<5e5){
    overlay_vars5=c("no_treated"="no_treated_undetectable")
  }
  overlay_vars=c(overlay_vars1,overlay_vars2,overlay_vars3,overlay_vars4,overlay_vars5)
  
  
  loess_vars <- c("percent_donor_acute","mean_time_donor_infected_incident",
                  "mean_age_incident","mean_age_died_AIDS","mean_spvl_incident",
                  "mean_PPP_incident","mean_spvl_incident_vacc")
  
  cumul_vars <-c("births" ,"aids_deaths","natural_deaths","new_infections",
                 "aged_out", "natural_deaths_infecteds","natural_deaths_susceptibles",  
                 "natural_deaths_infecteds",
                 "natural_deaths_susceptibles",
                 "new_diagnoses","new_infections_vacc_resist_virus",
                 "new_infections_drug_sens_virus",
                 "new_infections_drug_part_res_virus",
                 "new_infections_drug_3_plus_res_virus")
  
  
  model=summary_epi_mat(model)
  if(is.null(variables)){
    vars=names(model$epi_list)
  }else{
    vars=variables
  }
  epi_mats=model$epi_list
  #timesteps=model$epi$timestep[,1]/365
  timesteps=(1:model$param[[1]]$n_steps)/365
  
  
  if(T){
    if(save){
      if(is.null(name)){name<-"evoplots"}
      final_name <- paste(name,".pdf",sep="")
      pdf(file.path(outpath,final_name ),
          8.5,11)
    }
  }
  if(is.null(variables)){
    par(mfrow=c(3,2),mgp=c(2.5,1,0))
  }
  
  for(ii in 1:length(vars)){
    print(vars[ii])
    #if(vars[ii]=="prevalence"){browser()}
    #if(vars[ii]=="mean_vl_pop_untreated"){browser()}
    
    #plot incidence after prevalence
    if(ii==2){incidence_rate_plot(model)}
    
    if(vars[ii]=="timestep"){next()}
    if(vars[ii] %in% not_plotted){next()}
    
    #all NAs
    if(is.logical(epi_mats[[vars[ii]]]$min_max[1])){
      plot(1:10,1:10,type='n',ylab="",xlab ="")
      text(4,5,"All values NA")
      title(vars[ii])
      next()
    }
    #variables plotted cumulatively
    if(vars[ii] %in% cumul_vars){
      values=as.data.frame(epi_mats[[vars[ii]]]$values)
      for(kk in 1:ncol(values)){
        i1=which(is.na(values[,kk]))
        if(length(i1)>0){
           values[i1,kk]=0  
        }
      }
      remove(kk,i1)
      apply(values,2,function(xx) values[which(is.na(xx))]<<-0)
      values=(apply(values,2,cumsum))
      mean_values=rowMeans(values,na.rm=T)
      ylim=c(min(mean_values,na.rm=T),max(mean_values,na.rm=T))
    }else{
      values=epi_mats[[vars[ii]]]$values
      mean_values =epi_mats[[vars[ii]]]$mean_values
    }
    
    if(!is.element(vars[ii],c(cumul_vars,names(overlay_vars)))){
      ylim=epi_mats[[vars[ii]]]$min_max
    }
    
    if(is.element(vars[ii],names(overlay_vars))){
      ylim1=epi_mats[[vars[ii]]]$min_max
      ix=which(names(overlay_vars)==vars[ii])
      ylim2=epi_mats[[overlay_vars[ix]]]$min_max
      ylim=c(min(c(ylim1[1],ylim2[1]),na.rm=T),max(c(ylim1[2],ylim2[2]),na.rm=T))
    }
    
    
    plot(timesteps,seq(ylim[1],ylim[2],length=length(timesteps)),type='n',
         ylab="",xlab="years")
    if(is.null(variables)){ title(vars[ii])}
    #useer supplied title for plot unliess vector of names supplied in main argument
    #not as long as specified variables to graph
    if(!is.null(variables) & !is.null(main) ){
      if(ii <= length(main)){title(main[ii])}
    }else{
      title(vars[ii])
    }
    
    
 
    
    if(!is.element(vars[ii],loess_vars)){
      ix=which(!is.na(values))
      apply(values,2,function(xx){do.call(lines,list(x=timesteps[ix],y=xx[ix],col="blue"))})
      lines(timesteps[ix],mean_values[ix],col="blue",lwd=2)
    }
    if(is.element(vars[ii],loess_vars)){
      apply(values,2,function(xx){do.call(points,list(x=timesteps[ix],y=xx[ix],col="black"))})
      navec<- is.na(mean_values)
      if(any(navec)){
        if(length(which(navec==FALSE))>=5)
          lines(lowess(timesteps[!navec],mean_values[!navec], f=0.4),col="darkblue",lwd=2,lty=1)        
      }else{lines(lowess(timesteps,mean_values, f=0.4),col="darkblue",lwd=2,lty=1)}
    }
    if(is.element(vars[ii],names(overlay_vars))){
      ix=which(names(overlay_vars)==vars[ii])
      var2=unname(overlay_vars[ix])
      values=epi_mats[[var2]]$values
      mean_values =epi_mats[[var2]]$mean_values
      ix2=which(!is.na(values))
      apply(values,2,function(xx){do.call(lines,list(x=timesteps[ix2],y=xx[ix2],col="red"))})
      lines(timesteps[ix2],mean_values[ix2],col="red",lwd=2)
      mtext(var2,side=3,line=.25,col="red",cex=.75)
    }
    
  }#end of variable loop (epi timeseries)
  
  if(F){
    if(is.null(variables)){
      #non- epi / timeseries plots
      #incidence_rate_plot(model)
      #plot_heritability_quarters(model)
      #plot_donors_cd4(model)
      #plot_time_to_removal(model,"aids")
      #plot_time_to_removal(model,"nonaids")
      #plot_time_to_removal_by_spvl(model,"aids")
      #plot_time_to_removal_by_spvl(model,"nonaids")
      #plot_age_hist(model)
      #plot_relationship_duration(model)
    }
  }
  if(save){temp=dev.off()}
  
}#end of function  

