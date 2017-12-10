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
social_set_msm_role_on_nw <- function(params,init_nw)
{
  #Description:
  #Assign role values to network nodes for msm model
  # Helper function that sets role attribute value (I, R, or V for MSM) on network
  # based on proportion of individuals in each role category from params$role_props from "input_paramters_primary"
  # needed for estimation/simulation; called from “setup_nodal_attributes”
  #input: "role_props",dat$nw
  #output: vertex attribute on network
  
  role_values <- utilities_apportion.lr(vector.length = params$initial_pop,
                              values = names(params$role_props),
                              proportions = params$role_props ,
                              shuffled = T)
  
  #assign values of initial generic_attribue to initial network
  network::set.vertex.attribute(x = init_nw, attr = "role",
                                value = role_values)
  
  return(init_nw)
}
