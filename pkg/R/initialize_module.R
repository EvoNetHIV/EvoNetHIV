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
  # Sets up model structure in 15 steps
  
  #0: If model is restarting previous model, load 
  #   dat file from previous model from current directory, then end function
  #   (dat file can be saved with flag "save_dat_file=T" &
  #   "save_dat_file_step = xyz" where xyz is desired time step)
  #if(param$restart){
  #  if(file.exists(param$restart_dat_path)){
  #    load(param$restart_dat_name)
  #    dat$control$start <- param$restart_step
  #    return(dat)
  #  }else{
  #    stop("Incorrect path for original dat file")
  #  }
  #}
  
  if(control$start>1){
    dat <- initialize_restart(x, param, init, control, s)
    return(dat)
  }

  
  
  #1st step is epimodel function, rest evonet specific
  #1 
  # sets up basic EpiModel structure with EpiModel function initialize.net
  dat  <-  initialize.net(x, param, init, control,s)
  
  
  #1b 
  #if evonet parameters are to be sampled from distributions calculated here
  #e.g., a different value for each replicate 
  if(length(dat$param$random_params)>0){
    random_params <-  names(param_list$random_params)
    for(ii in 1:length(random_params)){
      parameter <- random_params[ii]
      dat$param[[parameter]]= do.call(dat$param$random_params[[ii]],list())
    }
  }
  
  #2 
  # create empty list of population summary stats
  # e.g. dat$epi$prevalence,dat$epi$incidence, etc.
  #filled in "summary_popsumm"
  #actual stats depend on type of model (msm/hetero/ART/aim3 )
  #adds evonet stats to the 2 default epimodel stats (i.num and s.num)
  
  popsumm_vars  <- summary_popsumm_vars(dat)
  for(ii in 1:length(popsumm_vars)){
    dat <-  add_epi(dat,popsumm_vars[ii])
  }

  #3 
  # Remove relationships specified as prohibited in network formation terms
  if(dat$param$rm_offset_rel) {
    dat <- remove_offset_relationships(dat)
  }
  
  #4 
  # sets up agent attributes and initial values 
  dat  <-  initialize_agents(dat, 1)
  
  #fills in vl variable values for initial infecteds
  dat  <-  initialize_infecteds_vl(dat,1)
  
  #5 fills in cd4 variable values for initial infecteds
  #note, this must come after initialize_infecteds_vl because initial cd4 based on spvl 
  dat  <-  initialize_infecteds_cd4(dat,1)
  
  #6 removed
  
  #7 updates vl of initial infecteds (otw, jumps from initial value to spvl)
  #but shouldn't happen for aim3 runs
  if(param$VL_Function != "aim3"){
  dat  <-  viral_update_gamma(dat,1)
  dat  <-  viral_update_cd4_intial_pop(dat)
  }
  
  #8 create list, if "save_vl_list" = TRUE to save
  #individual agent's vl/cd4 values for each timestep
  dat<- summary_vl_list(dat,1)
  
  #9 creates and fills in initial values for "popsumm" stats (stats calculated at
  #end of each timestep)
  dat <- summary_popsumm(dat,1)
  
  #10 keep track of current simulation/replicate
  dat$simulation <- s
  
  #11 create "pop" list as permanent record of agents 
  #filled in when agents die/age-out in summary_misc
  #if(param$VL_Function != "aim3"){
  dat$pop <- list()
  #note: now at top of summary-misc
  #dat$pop <- rep(list(NULL),length(dat$attr))
  #}else{
  #  dat$pop <- rep(list(NULL),length(dat$attr)-4)#remove I_vec,M_vec,L_vec,V_vec matrices
  #}
  
  
  #12 create (or not) coital acts list, which is used to save (if flagged)
  #coital acts df for each time step for qaqc purposes
  if(dat$param$save_coital_acts)
    dat$coital_acts_list <- list()
  else
    dat$coital_acts_list  <- NULL
  
  #13 dat$InfMat: data object filled in "transmission_bookkeeping_module"
  #if TRUE, then timestep, id of newly infected agent, and id of infector saved
  #for qaqc purposes
  if(dat$param$save_infection_matrix)
    dat$InfMat <- list()
  else 
    dat$InfMat<-NULL
  
  #14 create age list for plotting age distributions during model
  #run (at start,1/4,1/2,3/4, end of model time period)
  dat$age_list<-vector('list',length=5)
  
  #15 save fast edgelist if flag=T
  if(dat$param$fast_edgelist & dat$param$save_partner_list){
    dat$partner_list<-vector('list',length=dat$param$n_steps)
  }
  
  #16
  #also need to track number of agents that i) aged out ii) died-aids,
  # iii) died-naturally. If summary stats are not-calculated every timestep
  #will then need to track these stats separately because these agents are
  #removed from the "attr" list at the end of the timestep that they reach
  #these conditions. Used in "summary_popsumm"
  dat$no_births <- 0
  dat$no_deaths_aids <- 0
  dat$no_deaths_nonaids <- 0
  dat$no_aged_out <- 0
  
  #17 this is used to help create a permanent id for agents
     # as new arrivals occur, these are updated in the vital_new_births_bookeeping_pop function

   dat$total_agents <- dat$param$initial_pop
   
  return(dat)
}
