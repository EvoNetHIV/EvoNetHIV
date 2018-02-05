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

risk_group_changes <- function(dat,at){
  #wrapper for various transition functions for social attributes
  
  
  #-- update generic attribute
  if(!is.logical(dat$param$generic_nodal_att_values)){
  dat <- social_generic_attribute_transition(dat,at)
  }
  
  #-- update agent roles
  if(!is.logical(dat$param$role_props) && dat$param$model_sex=="msm"){
  tempvals <- dat$pop$role
  for( ii in names(dat$param$role_props))
  {
    index <- which(dat$pop$role == ii & dat$popStatus >= 0)
    size <- length(index)
    #role_trans_mat must have rownames "I","R","V"
    probs <- dat$param$role_trans_mat[ii,]
    new_vals <- sample( names(dat$param$role_props),
                        size = size,
                        prob = probs,
                        replace = T)
    tempvals[index] <- new_vals
  }
  dat$pop$role <-tempvals
  temp_match<- match(dat$attr$id,dat$pop$id)
  #qaqc for now (10/8/15)
  if(any(is.na(temp_match))){browser()}
  if(!is.null(dat[['nw']])){
      network::set.vertex.attribute( x = dat$nw, attr = "role",
                                   value = dat$pop$role[temp_match])
    }
    # update the version of the attribute in dat$attr as well
    dat$attr$role <- dat$pop$role[temp_match]
  }
  #-- end of role updates --------------------
  
  return(dat)
    
}