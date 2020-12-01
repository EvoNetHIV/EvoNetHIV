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
vital_births_bookkeeping_misc <- function(dat,at){
  #Description:
  #expands network object if births occurs and 
  #modifies nodal values on network "generic attribute" or "role" (msm model) 
  #if these attributes are being used.
  #modify/update network object and dat$attr vectors (vectors that track key attributes of network agents)
  #input: dat$popsumm$births[at] (number of births)
  #output: dat$nw, "status" vectors (dat$attr$status):  dat$attr$status_evo, dat$attr$status, dat$attr$active, 
  #dat$attr$entrTime, dat$attr$exitTime, dat$attr$infTime, dat$attr$att1, dat$attr$role
  
  
  #---  
  
  nBirths<- length(which(dat$attr$arrival_time==at))  
  
  

  dat$attr$status     <- c(dat$attr$status,   rep("s",nBirths))
  dat$attr$active     <- c(dat$attr$active,   rep(1, nBirths))
  dat$attr$entrTime   <- c(dat$attr$entrTime, rep(at, nBirths ) )
  dat$attr$exitTime   <- c(dat$attr$exitTime, rep(NA_real_, nBirths ) )
  dat$attr$infTime    <- c(dat$attr$infTime,  rep(NA_real_, nBirths ) )

  # setting nw attributes for "id","role","att1","sex"
  # note: values for these attributes created in "vital_new_additions()" in "births" section
  # which is called in "vital_births_bookkeeping"
  
  # attributes will only be attached to the network object if it exists, otherwise just in dat$attr
  
  # indices (and IDs) for new agents on "pop" list
  # temp_ix     <- (length(dat$attr$Status)-nBirths+1) : length(dat$attr$Status)
  # 
  # dat$attr$sex  <- c(dat$attr$sex,dat$attr$sex[temp_ix])
  # dat$attr$age  <- c(dat$attr$age,dat$attr$age[temp_ix])
  # dat$attr$sqrt_age  <- c(dat$attr$sqrt_age,dat$attr$sqrt_age[temp_ix])
  # 
  # 
  # 
  # if(!is.logical(dat$param$generic_nodal_att_values)){  
  #   dat$attr$att1  <- c(dat$attr$att1,dat$attr$att1[temp_ix])
  # }
  # 
  # if(!is.logical(dat$param$role_props) && dat$param$model_sex=="msm"){  
  #   dat$attr$role  <- c(dat$attr$role,dat$attr$role[temp_ix])
  # }
  # ###############
  # 
  # # consistency check that all of dat$attr are the same length
  # if(length(unique(sapply(dat$attr,length)))>1){
  #   browser()
  #   stop('inconsistent lengths of dat$attr, check bookeeping code')
  # }
  # #################
  # 
  
  #old code before epimodel 2.0, keeping temporarily 08/28/20
  #-----------------------------------------------
  #add new nodes/vertices and activate them (epimodel) 
  # if(!is.null(dat[['nw']])){
  #   n <- network.size(dat$nw)
  # } else {
  #   n <- attr(dat$el[[1]],'n')
  # }
  # newNodes <- (n + 1):(n + nBirths)
  # dat <- initiate_vertices(dat,at,nBirths)
  # 
  #-----------------------------------------------
  
  
  #-----------------------------------------------
  
  
  #code used before EpiModel 2.0, not necessary, keeping temporarily 08/28/20
  # 
  # if(!is.null(dat[['nw']])){
  #   set.vertex.attribute(x = dat$nw, 
  #                        attr = "sex",
  #                        value = dat$attr$sex[temp_ix],
  #                        v = newNodes)
  #   set.vertex.attribute(x = dat$nw, 
  #                        attr = "age",
  #                        value = dat$attr$age[temp_ix],
  #                        v = newNodes)
  #   set.vertex.attribute(x = dat$nw, 
  #                        attr = "sqrt_age",
  #                        value = sqrt(dat$attr$age[temp_ix]),
  #                        v = newNodes)
  #   set.vertex.attribute(x = dat$nw, attr = "id",
  #                       value = temp_ix, 
  #                       v = newNodes)
  #   
  #   if(!is.logical(dat$param$generic_nodal_att_values)){  
  #      set.vertex.attribute(x = dat$nw, 
  #                          attr = "att1",
  #                          value = dat$attr$att1[temp_ix],
  #                          v = newNodes)
  #   }
  #   if(!is.logical(dat$param$role_props) && dat$param$model_sex=="msm"){  
  #      set.vertex.attribute( x     = dat$nw, 
  #                           attr  = "role",
  #                           value = dat$attr$role[temp_ix],
  #                           v     = newNodes )
  #   }
  # }
  # 
  # 

  #-----------------------------------------------

  
  return(dat)
}
