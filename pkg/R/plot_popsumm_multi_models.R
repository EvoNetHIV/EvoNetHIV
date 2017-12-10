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
plots_popsumm_multi_models<-function(model_paths,model_names,outpath,
                                     out_names,
                        nw_stats=TRUE,max_points_rep=50)
{
  if(length(model_paths)!=length(model_names))
    stop("different number of model paths and model names")
  
  if(length(out_names)!=length(model_names))
    stop("different number of model names and output names")
  
  if(length(out_names)!=length(model_paths))
    stop("different number of model names and output paths")
  
for(tt in 1:length(model_paths))
{
  
  final_name <- paste(out_names[tt],"_popsumm_figs.pdf",sep="")
  pdf(file.path(outpath,final_name ),
      8.5,11)
  
  
  par(mfrow=c(3,2),mgp=c(2.5,1,0))
  load(model_paths[tt])
  model=NULL
  model=get(model_names[tt])
  fast_el <- model$param[[1]]$fast_edgelist
  plot_fxns <- summary_popsumm_fxns(model$param[[1]]$generic_nodal_att_values,fast_el)
  
    data <- model$popsumm
  popsumm_frequency=model$param[[1]]$popsumm_frequency
  vars <- names(model$popsumm[[1]])
  plot_type_vec <- lapply(plot_fxns,function(x) x$plot_type)
  plot_type_vec <- unlist(plot_type_vec)
  loess_vec  <- names(unlist(lapply(plot_fxns,function(x) x$loess)))
  overlay_vec <- unlist(lapply(plot_fxns,function(x) x$overlay))
  ymin_vec<- names(unlist(lapply(plot_fxns,function(x) x$ymin)))
  
  nsims<-length(data)
  
  if(popsumm_frequency==1)
    xx <- (1:model$param[[1]]$n_steps)/365
  else
    xx <- c(1,seq(popsumm_frequency,model$param[[1]]$n_steps,by=popsumm_frequency))/365
  
  #xx<-1:model$param[[1]]$n_steps/365
  
  #loop over variables
  for(ii in 1:length(vars))
  {
    #print(vars[ii])
    #if(vars[ii]=="natural_deaths"){browser()}
    
    if(!nw_stats){
      if(vars[ii] %in% c("no_edges","mean_degree","mean_degree_infected","no_nodes_degree_0",
                         "no_nodes_degree_1","no_nodes_concurrent" )){
        next
      }}
    
    all_na_flag <- plots_popsumm_all_NA_flag(vars[ii],model$popsumm)
    if(all_na_flag){
      plots_popsumm_all_NA_plot(vars[ii])
      next
    }
    ymin_ymax <- plots_popsumm_ymin_ymax(vars[ii],plot_type_vec[ii],model$popsumm)
    if(!(vars[ii] %in% ymin_vec)){ymin_ymax["ymin"]<-0}
    
    #loop over replicates
    data_list <- vector('list',length=length(data))
    for(jj in 1:length(data)){
      yy<- plots_popsumm_plotting(vars[ii],plot_type_vec[ii],nsim=jj,
                                  model$popsumm,xx,ymin_ymax,loess_vec,
                                  max_points_rep)
      data_list[[jj]]<- yy
      
      
    }
    
    if(vars[ii] %in% names(overlay_vec)){
      index <- which(names(overlay_vec)==vars[ii])
      plots_popsumm_overlay_lines(length(data),overlay_vec[index],model$param[[1]],
                                  model$popsumm,popsumm_frequency)
    }
    
    if(!(vars[ii] %in% loess_vec)){
      plots_popsumm_mean_value(data_list,xx)
    }
    
    
    if(vars[ii] %in% loess_vec){
      plots_popsumm_mean_value_loess(data_list,xx,vars[ii])
    }
    
    
  }
  dev.off()
  remove(model)
}

}

#--------------------------------------------------
