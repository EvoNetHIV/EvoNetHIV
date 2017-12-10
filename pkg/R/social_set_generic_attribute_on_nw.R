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
social_set_generic_attribute_on_nw <- function(params,init_nw){

  #Description:
  # Helper function that sets “att1”, generic attribute values on network
  # based on user set generic nodal attributes from "input_paramters_primary"
  # includes generic_nodal_att_values (names of generic attributes (eg, 1:5)) and 
  # generic_nodal_att_values_props (proportions of each attribute in initial pop)
  # needed for estimation/simulation; called from “setup_nodal_attributes”
  #fxn to set initial generic att values on initial network
  #input:generic_nodal_att_values, generic_nodal_att_values_props, dat$nw
  #output: vertex attribute for dat$nw
  
  temp_length <- length(params$generic_nodal_att_values)
  
  #if user supplies own proportions to apportion attribute among population
  #(with some qaqc on user input)

    # Definitions given in input_paramters_primary
    generic_nodal_att_values        = NA   # names of generic attributes (eg, 1:5)
    generic_nodal_att_values_props  = NA   # proportions of each att in initial pop
    generic_nodal_att_no_categories = NA   # how many generic att categories
    generic_nodal_att_trans_mat     = NA
  
  if(!is.logical(params$generic_nodal_att_values_props)){
    
      if(temp_length != length(params$generic_nodal_att_values_props))
           stop("number of attributes and number of elements in user distribution for generic attribute not equal")
      if(sum(params$generic_nodal_att_values_props)!=1)
          stop("user specified proportions do not sum to one")
      if(any(params$generic_nodal_att_values_props<=0))
         stop("invalid values in user input for generic attribute proportions")

       generic_att_values <- utilities_apportion.lr(vector.length = params$initial_pop,
                                 values = params$generic_nodal_att_values,
                                 proportions = params$generic_nodal_att_values_props,
                                 shuffled = T)      
  }# end of user specified proportions block
  
  #default uniform apportionment (more or less), generic_nodal_att_values_props=NA
  if(is.logical(params$generic_nodal_att_values_props)){
     
     temp_props  <-  rep(1/temp_length, temp_length) 
  
     generic_att_values <- utilities_apportion.lr(vector.length = params$initial_pop,
                                values = params$generic_nodal_att_values,
                                proportions = temp_props, shuffled = T)
  } #end of defualt apportionment
  
  #assign values of initial generic_attribue to initial network
  network::set.vertex.attribute( x = init_nw, attr = "att1",
                                  value = generic_att_values)
  
  return(init_nw)
}


