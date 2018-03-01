#' @title Initialization of EvoNet data objects
#'
#' @param dat master data object
#' @return Returns "data" object with additional EvoNet data objects appended.
#' @details Performs miscellaneous initialization steps to setup evonet model run:  i) creates 'coital_acts_list', which saves the object discord_coital df (table of relevant agent attributes in determining transmission probability) created each time step in social_coital_act; list filled in summary_misc; ii) creates object 'InfMat' which for each new infections saves time of infections, id of infectee, and id of infector; list filled in transmission_bookkeeping_module; iii) creates the popsumm data object which stores model summary statistics each timestep iv) creates age_list, which stores age data of population at beginning of model run and at quarterly inverals for histogram plots created in plots_popsumm; v) creates partner_list object which track for each agent the time a new relationship starts.
#' @examples
#' dat <- initialize_evonet_misc(dat)

#' @export
initialize_evonet_misc <- function(dat)
{
  #Description:
  # Miscellaneous internal bookkeeping for model setup:
  
  # 1) #create "dat$attr$status_evo" vector which tracks status of agent on network: 
  # infected(1) ,susceptible(0),dead aids(-2),dead other(-1), aged out (-1.5) 
  # note: dead/aged-out agents removed at end of daily timestep
  # note: dat$attr$status is an EpiModel object but uses different terms than in evonet
  
  # 2) Creates dat$attr$id vector which tracks id of agent on network. Allows to track agent info
  #in "pop" list to network; initial "id" value created in "vital_additions_new()"
  
  #3) Creates "role","sex","att1" vectors which store agent info for three variables on network
   #att1 variable is generic variable that can be used to group agents (eg, risk group, geography)
  # Creates "dat$attr$evo_status” vector for status of agents on network 
  # Sets "agent id" nodal attribute on network
  #Sets sex on network
  # VL list (if flagged): save VL/CD4 data for each agent per timestep
  # QAQC steps (optional): coital_acts_list (all coital acts info per timestep), and 
  # infection matrix (InfMat) (list of newly infected agent and their partner per  timestep)
  
  #create (or not) coital acts list, which is used to save (if flagged)
  #coital acts df for each time step for qaqc purposes
  if(dat$param$save_coital_acts)
    dat$coital_acts_list <- list()
  else
    dat$coital_acts_list  <- NULL
  
  #-----
  #dat$InfMat: data object filled in "transmission_bookkeeping_module"
  #if TRUE, then timestep, id of newly infected agent, and id of infector saved
  #for qaqc purposes
  if(dat$param$save_infection_matrix)
    dat$InfMat <- list()
  else 
    dat$InfMat<-NULL
  
  #---
  #attach fxns to calculte population statistics to "dat" object
  #and plotting methods; see "summary_popsumm_fxns"
  
 #if not aim3 run, then aim3 stats/figures not produced
 #if(dat$param$VL_Function=="aim3"){aim3=T}else{aim3=F}
 #if(dat$param$fast_edgelist==TRUE){fast_el=T}else{fast_el=F}
 # popsumm_fxns <- summary_popsumm_fxns(generic_nodal_att_values=dat$param$generic_nodal_att_values,
  #                                     aim3=aim3,fast_el=fast_el,params=dat$param)
 # dat$popsumm_fxns <- lapply(1:length(popsumm_fxns),function(x) popsumm_fxns[[x]]$model_value_fxn)
  #names(dat$popsumm_fxns)<- names(popsumm_fxns)
  popsumm2_vars  <- summary_popsumm_vars(dat)
  popsumm2_initial       <- vector('list',length(popsumm2_vars) )
  popsumm2        <- lapply(popsumm2_initial, function(x){ rep( NA_real_,
                                  times = ceiling(dat$param$n_steps/dat$param$popsumm_frequency)) } )
  names(popsumm2) <- popsumm2_vars
  dat$popsumm <-  popsumm2
  
  #create an empty list based on number of variables, fill in with NAs                                                          
  #popsumm_vars     <- names(popsumm_fxns)
  #dat$popsumm        <- vector('list', length(popsumm_vars))
  
  #popsumm length is how many times summary stats will be recorded
  #during model run; popsumm_frequency=1 means stats recorded
  #each timestep; setting popsumm_frequency >1 (eg, 30) saves time
  #by skipping calculations of summary stats
  #if(dat$param$popsumm_frequency==1)
   # popsumm_length <- dat$param$n_steps
  #else
  #  popsumm_length <- (dat$param$n_steps/dat$param$popsumm_frequency)+1
  
 #  dat$popsumm        <- lapply(dat$popsumm,function(x){ rep(NA_real_,times=popsumm_length)})
 # dat$popsumm        <- lapply(dat$popsumm,function(x){ rep(0,times=popsumm_length)})
#  names(dat$popsumm) <- popsumm_vars
  #-----------------------------
  #create age list for plotting age distributions during model
  #run (at start,1/4,1/2,3/4, end of model time period)
  dat$age_list<-vector('list',length=5)
  
   #-----------------------------
    #save fast edgelist if flag=T
   if(dat$param$fast_edgelist & dat$param$save_partner_list){
     dat$partner_list<-vector('list',length=dat$param$n_steps)
   }

    
   return(dat)
  
}
