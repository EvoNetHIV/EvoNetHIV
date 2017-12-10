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
initialize_restart<-function(x,param,init,control,s){
  #if(s==2){browser()}
  #------------------------------
  #code directly from EpiModel for sim. restart
  dat <- list()
  dat$nw <- x$network[[s]]
  dat$param <- x$param[[s]] #modified slighly from original EpiModel code (dat$param <- x$param)
  dat$control <- control
  dat$nwparam <- x$nwparam
  dat$epi <- sapply(x$epi, function(var) var[s])
  names(dat$epi) <- names(x$epi)
  dat$attr <- x$attr[[s]]
  dat$stats <- sapply(x$stats, function(var) var[[s]])
  dat$temp <- list()
  #---------------------
  # evonet code for sim. restart
  dat$pop<-x$pop[[s]]
  dat$popsumm <-x$popsumm[[s]]
  if(s==1){
  dat$coital_acts_list <- x$coital_acts_list[[s]]
  dat$vl_list <- x$vl_list[[s]]
  dat$InfMat <- x$InfMat[[s]]
  }
  if(s>1){
    if(length(x$coital_acts_list)>=s){
    dat$coital_acts_list <- x$coital_acts_list[[s]]
    }
    if(length(x$vl_list)>=s){
      dat$vl_list <- x$vl_list[[s]]
    }
    if(length(x$InfMat)>=s){
      dat$InfMat<- x$InfMat[[s]]
    }
  }
  
  #attach fxns to calculte population statistics to "dat" object
  fast_el <- param$fast_edgelist
  aim3 <- param$VL_Function=="aim3"
  popsumm_fxns <- summary_popsumm_fxns(generic_nodal_att_values= param$generic_nodal_att_values,aim3,fast_el)
  dat$popsumm_fxns <- lapply(1:length(popsumm_fxns),function(x) popsumm_fxns[[x]]$model_value_fxn)
  names(dat$popsumm_fxns)<- names(popsumm_fxns)
  #---------------------
  return(dat)
}
