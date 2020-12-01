#' @title Initialize agent attributes
#'
#'
#' @param dat master data object
#' @param at timestep
#' 
#' @return 'dat' object with 'pop'list of attributes for each agent appended.
#' @details
#' Subfunction in 'initialize_module'. 
#' Creates pop data object which contains the agent attribute values.  The attribute names for each agent is returned by input_parameters_agent_attributes. Default values are NA, other values are calculated by subfunction new_additions_fxn.
#' @examples
#' dat  <- initialize_agents(dat,at=1)

#' @export
initialize_agents <- function(dat,at)
{
  #Description: (8/2/16)
  # 1) Creates the "pop" list which contains all info for each individual agent. 
  # The length of pop list is equal to length of number of attributes contained in 
  # "input_parameters_agent_attributes" fxn. The length of each vector in the initial pop list (eg, age vector)
  # is equal to value for the initial number of agents (parameter 'initial_pop')
  # Default values for each attribute are initially NA; for attributes where default values are other than NA 
  # (eg, age attribute) are filled in with appropriate values with "new_additions_fxn".
  # 2) Creates data structures for "aim3" models
  # Inputs: param$popVariables, param$initial_pop
  # Outputs: dat$pop, V_vec_length, pop$V_vec, pop$I_vec, pop$M_vec, pop$L_vec
  
  popVars  <- input_parameters_agent_attributes()$popVariables
  pop        <- vector('list', length( popVars ) )
  pop        <- lapply(pop, function(x){ rep( NA_real_, times = dat$param$initial_pop ) } )
  names(pop) <- popVars
  
  pop <- new_additions(input_list = pop, dat = dat,
                           index = 1 : dat$param$initial_pop,
                           type = "initial", at = at)
  
  #Aim 3 variables (ask john for clarification); each agent gets a row in matrix
  #V,I,M,L that tracs number of mutations
  if(dat$param$VL_Function == "aim3"){
  V_vec_length   <- 2^dat$param$Max_Allowable_Loci 
  pop$V_vec <- matrix(0,nrow=dat$param$initial_pop, ncol=V_vec_length)
  pop$I_vec <- matrix(0,nrow=dat$param$initial_pop, ncol=V_vec_length)
  pop$M_vec <- matrix(0,nrow=dat$param$initial_pop, ncol=V_vec_length)
  pop$L_vec <- matrix(0,nrow=dat$param$initial_pop, ncol=V_vec_length)
  }
  
  #remove duplicates
  #Depending on network attributes, there may be redundancy of agent attributes
  #created by "new_addtions()" and attributes on network. The way epimodel is
  #setup, requires values for network attributes before the creation of the
  #agent "population". Kind of involved explanation, but this removes 
  #attributes that were already created  as they were necessry for network
  #estimation
  while(T){
  ix=which(names(pop) %in% names(dat$attr))
  if(length(ix)>0){
    #need to update index as elements are removed
    ix=which(names(pop) %in% names(dat$attr))
    attr_to_remove <- ix[1]
    pop[[attr_to_remove]] <- NULL
  }else{break}
  }
  
  #assigns "pop" object to main "dat" epimodel data structure
  
  dat$attr <- c(dat$attr,pop)  
  
  return(dat)
}
