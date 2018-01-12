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
#' 
#' 
evoplot<-function(model,outpath=getwd(),name=NULL,nw_stats=TRUE,max_points_rep=NULL,
                  popsumm_frequency=NULL,variables=NULL,...){

#evoplot<-function(model,name=NULL,outpath=getwd(),
#                  variables=NULL,nw_stats=NULL,max_points_rep=NULL,
#                  popsumm_frequency=NULL,...){
  
  evoplot_internal(model=model,save=F,name=name,outpath=outpath,
                               variables=variables)
  if(is.null(variables)){
  evoplot_internal(model=model,save=T,name=name,outpath=outpath,
                   variables=variables)
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
                             variables=NULL){

not_plotted=c(
"Perc_1_drug_muts_total_pop",
"Perc_2_drug_muts_total_pop",
"Perc_3_drug_muts_total_pop",
"Perc_4_drug_muts_total_pop")


#

overlay_vars1=c(
"susceptibles"="total_infections_alive")

overlay_vars2=NULL
if(model$param[[1]]$start_treatment_campaign<5e5){
overlay_vars2=c(
"inf_men"="inf_women",
"treated_inf_men"="treated_inf_women",
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
if(model$param[[1]]$model_sex=="hetero" & model$param[[1]]$start_treatment_campaign<5e5){
 overlay_vars5=c("no_treated"="no_treated_undetectable")
}
overlay_vars=c(overlay_vars1,overlay_vars2,overlay_vars3,overlay_vars4,overlay_vars5)



loess_vars <- c("percent_donor_acute","mean_time_donor_infected_incident",
                "mean_age_incident","mean_age_died_AIDS","mean_spvl_incident",
                "mean_PPP_incident")

cumul_vars <-c("births" ,"aids_deaths","natural_deaths",
"aged_out", "natural_deaths_infecteds","natural_deaths_susceptibles",
"no_treated",  "natural_deaths_infecteds",
  "natural_deaths_susceptibles",
  "new_diagnoses","new_infections_vacc_resist_virus",
    "new_infections_drug_sens_virus",
  "new_infections_drug_part_res_virus",
   "new_infections_drug_3_plus_res_virus")


model=summary_popsumm_mat(model)
if(is.null(variables)){
vars=names(model$popsumm_mats)
}else{
  vars=variables
}
popsumm_mats=model$popsumm_mats
timesteps=popsumm_mats$timestep$timestep$years

  

if(save){
  if(is.null(name)){name<-"evoplots"}
  final_name <- paste(name,".pdf",sep="")
  pdf(file.path(outpath,final_name ),
        8.5,11)
}

if(is.null(variables)){
par(mfrow=c(3,2),mgp=c(2.5,1,0))
}

for(ii in 1:length(vars)){
#temporary qaqc
#  print(ii)
#  if(ii==36){browser()}
###    
    if(vars[ii]=="timestep"){next()}
  if(vars[ii] %in% not_plotted){next()}
    
  #all NAs
  if(is.logical(popsumm_mats[[vars[ii]]]$min_max)){
    plot(1:10,1:10,type='n',ylab="",xlab ="")
    text(4,5,"All values NA")
    title(vars[ii])
    next()
  }
  #variables plotted cumulatively
  if(vars[ii] %in% cumul_vars){
    values=as.data.frame(popsumm_mats[[vars[ii]]]$values)
    apply(values,1,function(xx) values[which(is.na(xx))]<<-0)
    values=t(apply(popsumm_mats[[vars[ii]]]$values,1,cumsum))
    mean_values=colMeans(values)
    ylim=c(min(mean_values,na.rm=T),max(mean_values,na.rm=T))
  }else{
    values=popsumm_mats[[vars[ii]]]$values
    mean_values =popsumm_mats[[vars[ii]]]$mean_values
  }
  
  if(!is.element(vars[ii],c(cumul_vars,names(overlay_vars)))){
  ylim=popsumm_mats[[vars[ii]]]$min_max
  }
  
  if(is.element(vars[ii],names(overlay_vars))){
    ylim1=popsumm_mats[[vars[ii]]]$min_max
    ix=which(names(overlay_vars)==vars[ii])
    ylim2=popsumm_mats[[overlay_vars[ix]]]$min_max
    ylim=c(min(c(ylim1[1],ylim2[1]),na.rm=T),max(c(ylim1[2],ylim2[2]),na.rm=T))
  }

  
   plot(timesteps,seq(ylim[1],ylim[2],length=length(timesteps)),type='n',
       ylab="",xlab="years")
   title(vars[ii])
  
  
  if(!is.element(vars[ii],loess_vars)){
    apply(values,1,function(xx){do.call(lines,list(x=timesteps,y=xx,col="blue"))})
    lines(timesteps,mean_values,col="blue",lwd=2)
  }
  if(is.element(vars[ii],loess_vars)){
    apply(values,1,function(xx){do.call(points,list(x=timesteps,y=xx,col="black"))})
    navec<- is.na(mean_values)
    if(any(navec)){
      if(length(which(navec==FALSE))>=5)
        lines(lowess(timesteps[!navec],mean_values[!navec], f=0.4),col="darkblue",lwd=2,lty=1)        
    }else{lines(lowess(timesteps,mean_values, f=0.4),col="darkblue",lwd=2,lty=1)}
  }
  if(is.element(vars[ii],names(overlay_vars))){
    ix=which(names(overlay_vars)==vars[ii])
    var2=unname(overlay_vars[ix])
    values=popsumm_mats[[var2]]$values
    mean_values =popsumm_mats[[var2[ii]]]$mean_values
    apply(values,1,function(xx){do.call(lines,list(x=timesteps,y=xx,col="red"))})
    lines(timesteps,mean_values,col="red",lwd=2)
    mtext(var2,side=3,line=.25,col="red",cex=.75)
  }

}#end of variable loop (popsumm timeseries)
 
if(is.null(variables)){
 #non- popsumm / timeseries plots
  incidence_rate_plot(model)
  plot_heritability_quarters(model)
  plot_donors_cd4(model)
  plot_time_to_removal(model,"aids")
  plot_time_to_removal(model,"nonaids")
  plot_time_to_removal_by_spvl(model,"aids")
  plot_time_to_removal_by_spvl(model,"nonaids")
  plot_age_hist(model)
  plot_relationship_duration(model)
}

if(save){temp=dev.off()}
  
}#end of function  

