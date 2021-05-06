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
  #browser()
  #------------------------------
  #code directly from EpiModel initalize_net() for sim. restart
  dat <- list()
  if("network" %in% names(x[[s]])){
   dat$nw <- x[[s]]$network
  }else{
    dat$el <- x[[s]]$el
  }
  dat$param <- x[[s]]$param #modified slighly from original EpiModel code (dat$param <- x$param)
  dat$control <- control
  dat$nwparam <- x[[s]]$nwparam
  #dat$epi <- sapply(x$epi, function(var) var[s])
  #dat$epi <- sapply(1:length(x[[1]]$epi), function(yy) x[[s]]$epi[[yy]])
  #names(dat$epi) <- names(x[[1]]$epi)
  dat$epi <- x[[s]]$epi
  dat$attr <- x[[s]]$attr
  #dat$stats <- sapply(x$stats, function(var) var[[s]])
  #dat$stats <- sapply(1:length(x[[1]]$stats), function(yy) x[[s]]$stats[[yy]])
  dat$stats <- x[[s]]$stats
  dat$temp <- list()
  dat$p <- x[[1]]$p
  #---------------------
  # evonet code for sim. restart
  #note below data structures are evonet specific output that may or may not be present
  #based on whether appropriate flags were set (though "pop" and "age_list" are always output)

  
  if(length(x[[s]]$pop)>=0){
    dat$pop<-x[[s]]$pop
  }
  if(length(x[[s]]$age_list)>=0){
    dat$age_list<-x[[s]]$age_list
  }
  
  if(length(x[[s]]$no_births)>=0){
    dat$no_births<-x[[s]]$no_births
  }
  if(length(x[[s]]$no_deaths_aids)>=0){
    dat$no_deaths_aids<-x[[s]]$no_deaths_aids
  }
  if(length(x[[s]]$no_deaths_nonaids)>=0){
    dat$no_deaths_nonaids<-x[[s]]$no_deaths_nonaids
  }
  if(length(x[[s]]$no_aged_out)>=0){
    dat$no_aged_out <- x[[s]]$no_aged_out
  }
  if(length(x[[s]]$total_agents)>=0){
    dat$total_agents  <- x[[s]]$total_agents 
  }
  if(length(x[[s]]$coital_acts_list)>=0){
      dat$coital_acts_list <- x[[s]]$coital_acts_list
  }
  if(length(x[[s]]$vl_list)>=0){
    dat$vl_list <- x[[s]]$vl_list
  }
  if(length(x[[s]]$InfMat)>=0){
    dat$InfMat <- x[[s]]$InfMat
  }
  if(length(x[[s]]$coital_acts_list)>=0){
    dat$coital_acts_list <- x[[s]]$coital_acts_list
  }
  if(length(x[[s]]$vl_list)>=0){
      dat$vl_list <- x[[s]]$vl_list
  }
  if(length(x[[s]]$InfMat)>=0){
      dat$InfMat<- x[[s]]$InfMat
  }
###############################
  if(F){
    
    if(length(x$pop)>=0){
      dat$pop<-x$pop[[s]]
    }
    if(length(x$age_list)>=0){
      dat$age_list<-x$age_list[[s]]
    }
    
    if(length(x$no_births)>=0){
      dat$no_births<-x$no_births[[s]]
    }
    if(length(x$no_deaths_aids)>=0){
      dat$no_deaths_aids<-x$no_deaths_aids[[s]]
    }
    if(length(x$no_deaths_nonaids)>=0){
      dat$no_deaths_nonaids<-x$no_deaths_nonaids[[s]]
    }
    if(length(x$no_aged_out)>=0){
      dat$no_aged_out <- x$no_aged_out[[s]]
    }
    if(length(x$total_agents)>=0){
      dat$total_agents  <- x$total_agents [[s]]
    }
    if(length(x$coital_acts_list)>=0){
      dat$coital_acts_list <- x$coital_acts_list[[s]]
    }
    if(length(x$vl_list)>=0){
      dat$vl_list <- x$vl_list[[s]]
    }
    if(length(x$InfMat)>=0){
      dat$InfMat <- x$InfMat[[s]]
    }
    if(length(x$coital_acts_list)>=0){
      dat$coital_acts_list <- x$coital_acts_list[[s]]
    }
    if(length(x$vl_list)>=0){
      dat$vl_list <- x$vl_list[[s]]
    }
    if(length(x$InfMat)>=0){
      dat$InfMat<- x$InfMat[[s]]
    }
    
  }
############################################  
  #---------------------
  return(dat)
}
