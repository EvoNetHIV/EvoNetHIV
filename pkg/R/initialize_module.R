#' @title EvoNet model initialization
#'
#' @param x An EpiModel object of class netest
#' @param y A list of EvoNet parameters
#' @param init An EpiModel object of class init.net
#' @param control An EpiModel object of class control.net
#' @param s Simulation number

#' @return Returns "dat" list, which is master data structure for simulation
#' @details Sets up internal data structures and initial virological and demographic values for the initial population. with the following steps:  i) setups internal evonet data structures pop and popsumm with initialize_agents(); ii) runs EpiModel function initialize.net(); iii) fills in virological and associated values for initial infected with initialize_infecteds_vl(); iv) updates VL values of initial infected based on assumed time of infection with viral_update_aim2(); v) creates data structure (vl_list) to store daily VL values of infected agents; vi) creates optional data structures based on user input with initialize_evonet_misc; vii) calculates summary statistics for initial population with summary_popsumm(). 



#' @export
initialize_module <- function(x, param, init, control, s)
{  
  #Description:
  # runs  EpiModel function EpiModel::initialize.net()
  # fills in viral load values for initial infected with initialize_infecteds_vl()
  #	fills in cd4 variable values for initial infected (initial CD4 value based on SPVL)
  # with initialize_infecteds_cd4()
  #	does a few bookkeeping steps with initialize_evonet_misc()
  #	updates vl of initial infecteds (otw, vl jumps from initial value to spvl in one
  # timestep) with viral_update_module_gamma()
  #	creates and inserts initial values for "popsumm" stats (calculated at end of
  # each timestep_ with initialize_popsumm_dynamics()
  

    #sets up basic EpiModel structure
  dat  <-  initialize.net(x, param, init, control,s)

  ## Remove relationships specified as prohibited in network formation terms
  if(dat$param$rm_offset_rel) {
    dat <- remove_offset_relationships(dat)
  }
  
  # need to ensure that sex attribute as been copied to dat$attr from the network.
  #  ideally for consistency we'd like to have all of the attributes 
  # included on the dat$attr list.  However, when in network mode, only the 
  # attributes included in the formula (usually 'role') will be coppied
  # so need to force copy it here until we figure a better system.  
  dat$attr$status_evo <- rep(0,length(dat$attr$status))
  dat$attr$status_evo[which(dat$attr$status=="i" )] <- 1
  
  if(!is.null(dat[['nw']])){
    dat$attr$sex <- get.vertex.attribute(dat$nw,'sex')
    dat$attr$age <- get.vertex.attribute(dat$nw,'age')
    dat$attr$sqrt_age <- sqrt(dat$attr$age)
    dat$attr$id <- get.vertex.attribute(dat$nw,'id')
    if(!is.logical(dat$param$generic_nodal_att_values))
    dat$attr$att1 <- get.vertex.attribute(dat$nw,'att1')
  }
  
  # likewise, if there is going to be roles in the model, need to ensure they are copied in
  if(dat$param$model_sex=="msm" && 
     (is.null(dat$attr[['role']]) & !is.null(dat[['nw']]))){
    dat$attr$role <- get.vertex.attribute(dat$nw,'role')
  }
  
  
  #sets up agent attributes and initial values 
  dat  <-  initialize_agents(dat, 1)
  #fills in vl variable values for initial infecteds
  dat  <-  initialize_infecteds_vl(dat,1)
  #fills in cd4 variable values for initial infecteds
  #note, this must come after initialize_infecteds_vl because initial cd4 based on spvl 
  dat  <-  initialize_infecteds_cd4(dat,1)
  #does a few random bookkeeping steps
  dat  <-   initialize_evonet_misc(dat)
  #updates vl of initial infecteds (otw, jumps from initial value to spvl)
  #but shouldn't happen for aim3 runs
  if(param$VL_Function != "aim3"){
  dat  <-  viral_update_gamma(dat,1)
  dat  <-  viral_update_cd4_intial_pop(dat)
  }
  #create list, if "save_vl_list" = TRUE to save
  #individual agent's vl/cd4 values for each timestep
  dat<- summary_vl_list(dat,1)
  #creates and fills in initial values for "popsumm" stats (stats calculated at
  #end of each timestep)
  dat <- summary_popsumm(dat,1)
  #keep track of current simulation/replicate
  dat$simulation <- s
  return(dat)
}
