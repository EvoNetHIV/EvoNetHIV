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
plot_summary_timeseries<-function(model,outpath,name=NULL,nw_stats=TRUE)
{
  
  if(is.null(name)){name<-"population_summary_figures"}
  final_name <- paste(name,".pdf",sep="")
  pdf(file.path(outpath,final_name ),
      8.5,11)
    
  #setting up graphical parameters
  par(mfrow=c(3,2),mgp=c(2.5,1,0))
  tcl_value <- (-.05)

  
  
  
  xvec=1:model$param[[1]]$n_steps/365
  plot(xvec,seq(0,1,length=length(xvec)),type='n',ylab="prevalence",xlab="years",
       axes=F,tcl= -.05,cex.lab=1.5)
  for(ff in 1:length(model$pop))
  {
    yvec=model$popsumm[[ff]]$total_infections/model$popsumm[[ff]]$alive
    lines(xvec,yvec,col=ff)
  }
  axis(2,cex.axis=1.3)
  axis(1,cex.axis=1.3)
  box(,col="brown")
  mtext("prevalence",side=3,line=1)
  
ylab_vec=c(
  "aids_deaths"="cumulative deaths",
   "natural_deaths"="cumulative deaths",
   "natural_deaths_infecteds"="cumulative deaths",
   "natural_deaths_susceptibles"="cumulative deaths",
  "aged_out"= "cumulative agents",
   "births"= "cumulative 'births' ",
   "new_infections" = "cumulative new infections",
   "total_infections" = "total infected",
   "total_infections_not_treated" = "total infected",
   "susceptibles" = "total susceptibles",
   "alive" = "number alive",
  "no_in_aids_gamma" = "no. in aids",
  "no_in_aids_cd4" = "no. in aids",
  "no_treated"= "agents on treatment",
   "mean_spvl_pop_untreated" = "mean SPVL (untreated)",
   "median_spvl_pop_untreated" = "median SPVL (untreated)",
   "variance_spvl_pop_untreated" = "variance SPVL (untreated)",
   "mean_spvl_pop_all" = "mean SPVL (all)",
   "median_spvl_pop_all" = "median SPVL (all)",
   "variance_spvl_pop_all" = "variance SPVL (all)",
  "mean_spvl_incident"= "mean SPVL",
   "median_spvl_incident"= "median SPVL",
   "variance_spvl_incident"= "variance",
   "mean_vl_pop_untreated" = "mean VL",
   "median_vl_pop_untreated" = "median VL",
   "variance_vl_pop_untreated" = "variance",
   "mean_vl_pop_all" = "mean VL",
   "median_vl_pop_all" = "median VL",
   "variance_vl_pop_all" = "variance",
   "total_pills_taken" = "Pills Taken",
   "mean_age_incident" = "age",
   "mean_age_susceptibles" = "age",
   "mean_age_infecteds" = "age" ,
   "mean_age_died_AIDS" = "age" ,
   "mean_age_infected_died_natural"= "age" ,
   "mean_age_susceptibles_died_natural"= "age",
   "diagnosed" = "count",
   "no_edges" = "edges",
   "mean_degree"= "degree",
   "mean_degree"= "degree",
   "no_nodes_degree_0"="number nodes",
   "no_nodes_degree_1" ="number nodes",
   "no_nodes_concurrent"="number nodes")

titlevec=c("aids_deaths"="cumulative AIDS deaths",
           "natural_deaths"="cumulative non-AIDS deaths",
           "natural_deaths_infecteds"="cumulative non-AIDSdeaths, HIV positive",
           "natural_deaths_susceptibles"="cumulative non-AIDSdeaths, HIV negative",
           "aged_out"="cumulative number of aged-out agents",
           "births"= "cumulative 'births'",
           "new_infections" = "cumulative new infections",
           "total_infections" = "total infections,alive (no. tx dashed)",
           "total_infections_not_treated" = "Infected, but not treated",
           "susceptibles" = " total susceptibles (alive)",
           "alive" = "number alive",
           "no_in_aids_gamma"= "no. in aids (gamma death)",
           "no_in_aids_cd4"= "no. in aids (by cd4 category)",
           "no_treated"= "no. on tx / total inf (dashed)",
           "mean_spvl_pop_untreated" = "mean SPVL infecteds (no therapy)",
           "median_spvl_pop_untreated" = "median SPVL  infecteds (no therapy)",
           "variance_spvl_pop_untreated" = "variance SPVL  infecteds (no therapy)",
           "mean_spvl_pop_all" = "mean SPVL infecteds (all)",
           "median_spvl_pop_all" = "median SPVL  infecteds (all)",
           "variance_spvl_pop_all" = "variance SPVL  infecteds (all)",
           "mean_spvl_incident"= "mean SPVL per incident",
           "median_spvl_incident"= "median SPVL per incident",
           "variance_spvl_incident" = "variance SPVL per incident (>1 infection)",
           "mean_vl_pop_untreated" = "mean VL infecteds (no therapy)",
           "median_vl_pop_untreated" = "median VL  infecteds (no therapy)",
           "variance_vl_pop_untreated" = "variance VL infecteds (no therapy)",
           "mean_vl_pop_all" = "mean VL infecteds (all)",
           "median_vl_pop_all" = "median VL  infecteds (all)",
           "variance_vl_pop_all" = "variance VL infecteds(all)",
           "total_pills_taken" = "Cum pills taken in the population",
           "mean_age_incident" = "mean age per incident",
           "mean_age_susceptibles" = "mean age susceptibles",
           "mean_age_infecteds" = "mean age infecteds",
           "mean_age_died_AIDS" = "mean age AIDS deaths" ,
           "mean_age_infected_died_natural"= "mean age non-AIDS deaths, HIV+" ,
           "mean_age_susceptibles_died_natural"= "mean age non-AIDS deaths, HIV-" ,
           "diagnosed" = "new diagnoses",
           "no_edges" = "mean number of network edges",
           "mean_degree"= "mean network degree",
           "mean_degree_infected" = "mean network degree [Placeholder pending fixes]",
           "no_nodes_degree_0"="mean proportion nodes degree 0",
           "no_nodes_degree_1" ="mean proportion nodes degree 1",
           "no_nodes_concurrent"="mean proportion nodes degree concurrent")

#line plots that use cumulative sums
plot_type1<-c("aids_deaths","natural_deaths","natural_deaths_infecteds",
              "natural_deaths_susceptibles","births","new_infections","diagnosed",
              "aged_out")

#line plots that use untranformed time series 
plot_type2<-c("total_infections","total_infections_not_treated","susceptibles","alive","no_treated",
              "mean_spvl_pop_untreated","median_spvl_pop_untreated" ,"variance_spvl_pop_untreated",
              "mean_spvl_pop_all","median_spvl_pop_all" ,"variance_spvl_pop_all",
              "mean_vl_pop_untreated","median_vl_pop_untreated","variance_vl_pop_untreated",
              "mean_vl_pop_all","median_vl_pop_all","variance_vl_pop_all","total_pills_taken",
              "mean_age_susceptibles","mean_age_infecteds",
              "no_in_aids_gamma","no_in_aids_cd4")

#scatter plots
plot_type3 <-c("mean_spvl_incident","median_spvl_incident","variance_spvl_incident",
               "mean_age_incident", "mean_age_died_AIDS" ,
               "mean_age_infected_died_natural",
               "mean_age_susceptibles_died_natural")

plot_type4 <- c("no_edges","mean_degree","mean_degree_infected", "no_nodes_degree_0",
              "no_nodes_degree_1" ,"no_nodes_concurrent")

loess_plots <- c("mean_spvl_incident","median_spvl_incident","variance_spvl_incident",
                 "mean_age_incident","mean_age_died_AIDS")

#-------------------------------------------
#qaqc to make sure all key vectors are of same length. This ensures that when
#new variables are added, that all three vectors are appropriately modified.

ylab_vec_length <- length(ylab_vec)
titlevec_length <- length(titlevec)
plot_type_length <- length(c(plot_type1,plot_type2,plot_type3,plot_type4))

if(!identical(c(ylab_vec_length,titlevec_length),rep(plot_type_length,2))){
  cat("\n ylab_vec_length: ", ylab_vec_length)
  cat("\n titlevec_length: ", titlevec_length)
  cat("\n plot_type_length: ", plot_type_length)
  
  stop("Function stopped. Input vectors not all the same length")
}
#-------------------------------------------


name_vec <- names(model$popsumm[[1]])

#loop through each summary variable
for(ii in 1:length(name_vec))
{

  #print(ii)
  if(!nw_stats){
     if(name_vec[ii] %in% c("no_edges","mean_degree","mean_degree_infected","no_nodes_degree_0",
                         "no_nodes_degree_1","no_nodes_concurrent" )){
     next
    }
   }
  


  
  all_data_vec <- unlist(lapply(model$popsumm,function(x) x[[name_vec[ii]]]))
  if(all(is.na(all_data_vec))  ){ #| any(all_data_vec == -Inf)
    plot(1:10,1:10,ylab=NA,xlab=NA,type='n')
    mtext("All values NA or zero or some -Inf",side=3,line=-4)
    mtext(titlevec[name_vec[ii]],side=3,line=1)
    next
  }
  
  
  temp_which=which(all_data_vec==-Inf) 
  if(length(temp_which>0)  ){ #| any(all_data_vec == -Inf)
    plot(1:10,1:10,ylab=NA,xlab=NA,type='n')
    mtext("Error: some values are -Inf",side=3,line=-4)
    mtext(titlevec[name_vec[ii]],side=3,line=1)
    next
  }
  
  if(name_vec[ii] %in% plot_type1){    
  ymax <- max(unlist(lapply(model$popsumm,function(x) cumsum(x[[name_vec[ii]]]))),na.rm=T)
  ymin <- min(unlist(lapply(model$popsumm,function(x) cumsum(x[[name_vec[ii]]]))),na.rm=T)
  }else{
    ymax <- max(unlist(lapply(model$popsumm,function(x) x[[name_vec[ii]]])),na.rm=T)
    ymin <- min(unlist(lapply(model$popsumm,function(x) x[[name_vec[ii]]])),na.rm=T)    
  }

  if(name_vec[ii] %in% c("no_edges","mean_degree","mean_degree_infected","no_nodes_degree_0","no_nodes_degree_1" , "no_nodes_concurrent")){
    lty_value <- 2
  }else{lty_value<- 1}
  

  if(name_vec[ii] %in% c(plot_type4)){ 
    xvec <- (1:model$param[[1]]$n_steps) / model$param[[1]]$timesteps_per_year
    temp_index <- 1:length(model$popsumm)
    temp_vec <- unlist(lapply(temp_index,function(x) model$popsumm[[x]][[ii]]))
    temp_matrix <- matrix(temp_vec,nrow=length(model$popsumm),
                          ncol=model$param[[1]]$n_steps)
    mean_vec <- colMeans(temp_matrix,na.rm=T)
    plot(xvec,mean_vec,type='l',xlab="years",ylab=ylab_vec[name_vec[ii]],
         axes=F,tcl=tcl_value,lty=lty_value,col=1,cex.lab=1.5)
    axis(2,cex.axis=1.3)
    axis(1,cex.axis=1.3)
    box(,col="brown")
    mtext(titlevec[name_vec[ii]],side=3,line=1)    
  } 
  
  #if(length(model$pop)==1){nsims=1}
  #if(length(model$pop)>1){nsims=length(model$pop)}
  nsims=length(model$pop)
  
  for(jj in 1:nsims)
  {
    #time series vector for variable x simulation number
    summary_data <- model$popsumm[[jj]][[ii]]
    
    if(all(is.na(summary_data))){next}
    
    #have to do this because initial infecteds don't have full vl progression,
    #they go from vl of 2 to spvl in timestep 
    #if(name_vec[ii] %in% c("mean_vl_pop","median_vl_pop","variance_vl_pop"))
     #  summary_data[1]<- summary_data[2]
    
    #set up x-axis for plots
    xvec <- (1:model$param[[nsims]]$n_steps) / model$param[[nsims]]$timesteps_per_year
    
    #type1 plots use cumulative sums, not raw data
    if(name_vec[ii] %in% plot_type1){
      yvec <- cumsum(summary_data)
    }
    
    #other types just use raw data
    if(name_vec[ii] %in% c(plot_type2,plot_type3)){
      yvec <- summary_data
    }
    
    if(name_vec[ii] %in% c(plot_type1,plot_type2)){ 
      if(jj==1){
        plot(xvec,yvec,type='l',xlab="years",ylab=ylab_vec[name_vec[ii]],
             axes=F,tcl=tcl_value,lty=lty_value,col=jj,cex.lab=1.5,ylim=c(ymin,ymax))
        axis(2,cex.axis=1.3)
        axis(1,cex.axis=1.3)
        box(,col="brown")
        mtext(titlevec[name_vec[ii]],side=3,line=1)
        
      }else{
        lines(xvec,yvec,type='l',col=jj,lty=lty_value)   
      }
    }

    
    if(name_vec[ii] %in% plot_type3 & !(name_vec[ii] %in%  loess_plots)){
      if(jj==1){
        plot(xvec,yvec,xlab="years",ylab=ylab_vec[name_vec[ii]],
           axes=F,tcl=tcl_value,pch=jj,cex.lab=1.5,col=jj,ylim=c(ymin,ymax))
        axis(2,cex.axis=1.3)
        axis(1,cex.axis=1.3)
        box(,col="brown")
        mtext(titlevec[name_vec[ii]],side=3,line=1)
        
      }else{
        points(xvec,yvec,pch=jj,col=jj) 
      }
    } 
    
    if(name_vec[ii] %in% plot_type3 & (name_vec[ii] %in%  loess_plots)){
      if(jj==1){
        plot(xvec,yvec,xlab="years",ylab=ylab_vec[name_vec[ii]],type='n',
             axes=F,tcl=tcl_value,pch=jj,cex.lab=1.5,col=jj,ylim=c(ymin,ymax))
        axis(2,cex.axis=1.3)
        axis(1,cex.axis=1.3)
        box(,col="brown")
        mtext(titlevec[name_vec[ii]],side=3,line=1)
        
      }
    }
  }#END OF NSIMS LOOP  
    
  #overlay "no_treated" on "total_infections  plot
  if(name_vec[ii] %in%  "total_infections"){
    for(kk in 1:nsims)
    {
      yvec <- model$popsumm[[kk]]$no_treated
      xvec <- (1:model$param[[kk]]$n_steps) / model$param[[kk]]$timesteps_per_year
      lines(xvec,yvec,col=kk,lty=2)
    }
  }
  
  
  if(name_vec[ii] %in%  loess_plots & name_vec[ii] %in% plot_type3){
    for(kk in 1:nsims)
    {
      summary_data <- model$popsumm[[kk]][[ii]]
      xvec <- (1:model$param[[nsims]]$n_steps) / model$param[[nsims]]$timesteps_per_year
      yvec <- summary_data
      navec<- is.na(yvec)
      
      yvec_real <- which(!is.na(yvec))
      if(length(yvec_real)>250){
        temp_sample<-sample(yvec_real,250)
        points(xvec[temp_sample],yvec[temp_sample],col=kk,pch=kk,cex=.4)
      }else{points(xvec,yvec,col=kk,pch=kk,cex=.3)}
      
      if(any(navec)){
        lines(lowess(xvec[!navec],yvec[!navec], f=0.05),col=kk,lwd=1)        
      }else{lines(lowess(xvec,yvec, f=0.05),col=kk,lwd=1)}
      
    }#end of nsims loop in addin loess smoother
  }#end if() for  adding loess smoother
} #end off ii popsumm loop
dev.off()

}
