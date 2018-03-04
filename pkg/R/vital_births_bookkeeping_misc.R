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
  #dat$attr$entrTime, dat$attr$exitTime, dat$attr$infTime, dat$attr$att1, dat$pop$role
  
  
#---  
nBirths<- length(which(dat$pop$arrival_time==at))  
#-----------------------------------------------
#add new nodes/vertices and activate them (epimodel) 
if(!is.null(dat[['nw']])){
  n <- network.size(dat$nw)
} else {
  n <- attr(dat$el[[1]],'n')
}
newNodes <- (n + 1):(n + nBirths)
dat <- EpiModel:::initiate_vertices(dat,at,nBirths)

#-----------------------------------------------

dat$attr$status_evo <- c(dat$attr$status_evo,  rep(0,nBirths))
dat$attr$status     <- c(dat$attr$status,   rep("s",nBirths))
dat$attr$active     <- c(dat$attr$active,   rep(1, nBirths))
dat$attr$entrTime   <- c(dat$attr$entrTime, rep(at, nBirths ) )
dat$attr$exitTime   <- c(dat$attr$exitTime, rep(NA_real_, nBirths ) )
dat$attr$infTime    <- c(dat$attr$infTime,  rep(NA_real_, nBirths ) )

## Set vaccination/risk compensation status for all new entries to 0
if(dat$param$risk_comp_degree) {
  dat$attr$vacc_rc <- c(dat$attr$vacc_rc, rep(0, nBirths))
}

#-----------------------------------------------

# setting nw attributes for "id","role","att1","sex"
# note: values for these attributes created in "vital_new_additions()" in "births" section
# which is called in "vital_births_bookkeeping"

# attributes will only be attached to the network object if it exists, otherwise just in dat$attr

# indices (and IDs) for new agents on "pop" list
temp_ix     <- (length(dat$pop$Status)-nBirths+1) : length(dat$pop$Status)

dat$attr$sex  <- c(dat$attr$sex,dat$pop$sex[temp_ix])
dat$attr$age       <- c(dat$attr$age,      dat$pop$age[temp_ix])
dat$attr$age_cat   <- c(dat$attr$age_cat,  dat$pop$age_cat[temp_ix])

if(!is.null(dat[['nw']])){
  set.vertex.attribute(x = dat$nw, 
                       attr = "sex",
                       value = dat$pop$sex[temp_ix],
                       v = newNodes)
  set.vertex.attribute(x = dat$nw, 
                       attr = "age",
                       value = dat$pop$age[temp_ix],
                       v = newNodes)
}


#"id" is for qaqc (to match id on network to id on "pop")
#and is always set
dat$attr$id  <- c(dat$attr$id,dat$pop$id[temp_ix])
if(!is.null(dat[['nw']])){
  network::set.vertex.attribute(x = dat$nw, attr = "id",
                              value = temp_ix, v = newNodes)
}

if(!is.logical(dat$param$generic_nodal_att_values)){  
  dat$attr$att1  <- c(dat$attr$att1,dat$pop$att1[temp_ix])
  if(!is.null(dat[['nw']])){
    set.vertex.attribute(x = dat$nw, 
                         attr = "att1",
                         value = dat$pop$att1[temp_ix],
                         v = newNodes)
  }
}

if(!is.logical(dat$param$role_props) && dat$param$model_sex=="msm"){  
  dat$attr$role  <- c(dat$attr$role,dat$pop$role[temp_ix])
  if(!is.null(dat[['nw']])){
    set.vertex.attribute( x     = dat$nw, 
                          attr  = "role",
                          value = dat$pop$role[temp_ix],
                          v     = newNodes )
  }
}
#-----------------------------------------------
# consistency check that all of dat$attr are the same length
if(length(unique(sapply(dat$attr,length)))>1){
  browser()
  stop('inconsistent lengths of dat$attr, check bookeeping code')
}


return(dat)
}