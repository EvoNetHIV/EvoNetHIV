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

netsim_restart <- function (x, param, init, control,type="initial",dat_input=NULL) 
{
  
  if(type=="initial"){
    nsteps=param$n_steps
    nsteps_initial=param$n_steps_initial
  crosscheck.net(x, param, init, control)
  if (!is.null(control[["verbose.FUN"]])) {
    do.call(control[["verbose.FUN"]], list(control, type = "startup"))
  }
  nsims <- control$nsims
  ncores <- ifelse(nsims == 1, 1, min(parallel::detectCores(), 
                                      control$ncores))
  dat_list <-  vector('list',length=nsims)
  control$ncores <- ncores
  if (ncores == 1) {
    for (s in 1:control$nsims) {
      if (!is.null(control[["initialize.FUN"]])) {
        dat <- do.call(control[["initialize.FUN"]], 
                       list(x, param, init, control, s))
      }
      
        for (at in max(2, control$start):nsteps_initial) {
          morder <- control$module.order
          if (is.null(morder)) {
            lim.bi.mods <- control$bi.mods[-which(control$bi.mods %in% 
                                                    c("initialize.FUN", "verbose.FUN"))]
            morder <- c(control$user.mods, lim.bi.mods)
          }
          for (i in seq_along(morder)) {
            dat <- do.call(control[[morder[i]]], list(dat, 
                                                      at))
          }
          
        }
      
      if (s == 1) {
        out <- saveout.net(dat, s)
      }
      else {
        out <- saveout.net(dat, s, out)
      }
      class(out) <- "netsim"
      dat_list[[s]]<-dat
      out$dat_list <- dat_list
    }
  }
  return(out)
  }#type=initial
#---------------------------------------------------------------------------
  if(type=="restart"){
    nsteps=param$n_steps
    nsteps_initial=param$n_steps_initial
    
    nsims <- control$nsims
    ncores <- ifelse(nsims == 1, 1, min(parallel::detectCores(), 
                                        control$ncores))
    control$ncores <- ncores
    if (ncores == 1) {
      for (s in 1:control$nsims) {
        dat <- dat_input[[s]]
        for (at in (nsteps_initial+1):nsteps) {
          morder <- control$module.order
          if (is.null(morder)) {
            lim.bi.mods <- control$bi.mods[-which(control$bi.mods %in% 
                                                    c("initialize.FUN", "verbose.FUN"))]
            morder <- c(control$user.mods, lim.bi.mods)
          }
          for (i in seq_along(morder)) {
            dat <- do.call(control[[morder[i]]], list(dat, 
                                                      at))
          }
          
        }
        
        if (s == 1) {
          out <- saveout.net(dat, s)
        }
        else {
          out <- saveout.net(dat, s, out)
        }
        class(out) <- "netsim"
      }
    }
    return(out)
  }#type=restart
  
}
