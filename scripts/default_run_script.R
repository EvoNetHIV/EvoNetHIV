#See installation instructions to install evonet
# https://github.com/EvoNetHIV/EvoNetHIV/blob/master/scripts/installation_instructions.R

#--------------------------------------------------------------

library(evonet)

#--------------------------------------------------------------
# change warnings to errors and open browser on error
options(error=browser) # go into debug mode on error
#--------------------------------------------------------------

#Read in / set parameter values as list object
#For alphabetical list of input parameter names: sort(names(evonet_setup()))


param_list=list(
nsims = 3,
initial_pop = 200,
initial_infected = 40,
n_steps = 365*5,
popsumm_frequency=30,
plot_nw=T)

#create parameter list with updated values
evoparams <- do.call(evonet_setup,param_list)


#-------------------------------------------------------------
#network setup 
nw <- nw_setup(evoparams)

#--------------------------------------------------------------


#---  Create list of modules to run for input into epimodel_control_fxn() below
# "initialize_module" and "resim.nets" modules automatically added, no need to specify

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

evomodel <- evorun(modules,evoparams,nw)

#--------------------------------------------------------------

#to specify name of output pdf and pathway, use arguments
# name="xyz.pfd",outpath="/path to folder"
evoplot(model=evomodel)

#--------------------------------------------------------------

