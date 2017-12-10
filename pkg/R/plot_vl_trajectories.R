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
plot_vl_trajectories <- function(model,sim=1,Therapy_Type=1,outpath=NULL,name="evomodel"){
  if(model$param[[1]]$VL_Function=="aim3"){
    
    plot_vl_trajectories_aim3(model=model,sim,Therapy_Type=Therapy_Type,
                              outpath=outpath,
                              name=name)
  }else{
    
    plot_vl_trajectories_aim2(model=model,sim=sim,outpath=outpath,
                              name=name)
    
  }
  
}

