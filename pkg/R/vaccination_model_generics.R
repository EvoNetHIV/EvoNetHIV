#file has "generic" definitions for
# update_mu, update_sigma, initialize_phi, draw_m, calculate_theta
#model specific fxns are in vaccine_model_xx.R, xx=model number


#file also has function definitions for
#  update_mu_and sigma, initialize_and_update_phi, initial_vaccine_agents

#' @export
update_mu <- function(dat,at,...) UseMethod("update_mu")
#' @export
update_sigma <- function(dat,at,...) UseMethod("update_sigma")
#' @export
initialize_phi <- function(dat,at,...) UseMethod("initialize_phi")
#' @export
update_phi <- function(dat,at,...) UseMethod("update_phi")
#' @export
draw_m <- function(dat,at,...) UseMethod("draw_m")
#' @export
calculate_theta <- function(dat,m,...) UseMethod("calculate_theta")

#' @export
update_mu_and_sigma<-function(dat,at){
  dat$vacc_model$agents <- update_mu(dat,at)
  dat$vacc_model$agents <- update_sigma(dat,at)
  return(dat)
}

#' @export
initialize_and_update_phi<-function(dat,at){
  
  if(at<dat$param$start_vacc_campaign[1]){return(dat)}
  if(!is.element(at,dat$param$start_vacc_campaign)){return(dat)}
  
  dat <- update_phi(dat,at)
  dat <- initialize_phi(dat,at)
  return(dat)
}



#' @export
initialize_vaccine_agents <- function(dat,at){
  
  #at=2 is first time step after model setup/initialization
  if(at==2){
    #create agent object (list of lists attached to dat)
    agent_list <- list(phi=NA,mu=NA,sigma=NA)
    no_current_agents <- length(dat$pop$Status)
    dat$vacc_model$agents <- vector('list',length=no_current_agents)
    #add class 'dat$param$vacc_model' to ojects to trigger appropriate model fxn method
    class(dat$vacc_model$agents)<- c("list",dat$param$vacc_model_id)
    class(dat)<- c("list",dat$param$vacc_model_id)
    dat$vacc_model$agents <- lapply(dat$vacc_model$agents, function(x) x <- agent_list)
    #if start of model initialize mu/sigma or initialize for new agents  
    dat$vacc_model$agents <- update_mu(dat,at)
    dat$vacc_model$agents <- update_sigma(dat,at) 
  }
  
  #if start of model initialize mu/sigma or initialize for new agents   
  if(at>2 & length(dat$pop$Status) > length(dat$vacc_model$agents)){
    #if(at>500) browser()
    agent_list <- list(phi=NA,mu=NA,sigma=NA)
    total_new_agents <- length(dat$pop$Status)- length(dat$vacc_model$agents)
    new_agents_index <- (length(dat$vacc_model$agents)+1):(length(dat$vacc_model$agents) + total_new_agents)
    invisible(lapply(new_agents_index,function(x) dat$vacc_model$agents[[x]] <<- agent_list ))
  }
  
  return(dat)
  
}
