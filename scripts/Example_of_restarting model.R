#--------------------------------------------------------------
# OVERVIEW
#Simple example of running a model for 2 years, then running it for
# 2 more years.  This is a way to create a "base" model for different experiments
# where the parameters can be changed for the restarted model but the first "burn in" period
# is the same for all experiments.

#--------------------------------------------------------------


library(evonet)

#--------------------------------------------------------------
# change warnings to errors and open browser on error if working on code
#options(error=browser) # go into debug mode on error
#--------------------------------------------------------------

#Read in / set parameter values as list object
#For alphabetical list of input parameter names: sort(names(evonet_setup()))


param_list=list(
  nsims = 2,
  ncores=1,
  initial_pop = 200,
  initial_infected = 40,
  n_steps = 365*2,
  popsumm_frequency=30,
  raw_output= T)
#Note: The parameter "raw_output" means the output of the first model is essentially
#      the EpiModel "dat" list data structure. This parameter is only set to true when 
#      starting/restarting a model


#create parameter list with updated values
evoparams <- do.call(evonet_setup,param_list)


#-------------------------------------------------------------
#network setup 
nw <- nw_setup(evoparams)

#--------------------------------------------------------------


#---  Create list of modules to run for input into epimodel_control_fxn() below
# "initialize_module" automatically added, no need to specify

modules <- c(
  "aging",
  "testing",
  "treatment",
  "viral_update",
  "coital_acts",
  "transmission",
  "evo_departures",
  "evo_arrivals",
  "summary_module")
#--------------------------------------------------------------


#first model output
evomodel_base <- evorun(modules,evoparams,nw)

#--------------------------------------------------------------
#then change total model duration to 4 years but start a day after
#the end of the first model; need tochange values for each replicate/simulation ([[ii]] index)
for(ii in seq_along(evomodel_base)){
  evomodel_base[[ii]]$control$start <- 365*2+1
  evomodel_base[[ii]]$control$nsteps <- 365*4
  evomodel_base[[ii]]$param$n_steps <- 365*4
  evomodel_base[[ii]]$control$raw.output= F
  #add any other non-network parameter to change (e.g., treatment regimens, condom use, etc.)
}

#final evonet ouptut that will have output for all 4 years
#"evomodel_base[[1]]$init" is expected by the function but not used; The [[1]] element
#indicates the first output from the first replicate is used but could be either replicate.
#And its the same idea for "evomodel_base[[1]]$control".
evomodel_final<- netsim(evomodel_base,
                        evoparams,
                        evomodel_base[[1]]$init,
                        evomodel_base[[1]]$control)

evoplot(model=evomodel_final)
