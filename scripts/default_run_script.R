#NOTE: 
#when working on CSDE Windows Server, R updates remove
#installed libraries and evonet doesn't build, here is 
#code to quickly re-install everythng. Uncomment code below
# to run. Also might have to rebuild evonet (Build tab)
  #install.packages("devtools")
  #install.packages("EpiModel")
  #install.packages("data.table")
  #install.packages("plotrix")
  #install.packages("testthat")
  #devtools::install_github( "statnet/tergmLite",force=T)
  #devtools::install_github( "statnet/EpiModel", ref ="fast_edgelist")
#--------------------------------------------------------------

#users can ignore these lines
#devtools::document("~/evonetGitHub/EvoNet/pkg")

#--------------------------------------------------------------

library(evonet)

#--------------------------------------------------------------
# change warnings to errors and open browser on error
options(error=browser) # go into debug mode on error
#--------------------------------------------------------------

#Read in / set parameter values
#For alphabetical list of input parameter names: sort(names(evonet_setup()))


#Need to set initial pop value as other parameters are dependent on its value
initial_pop=200

param_list=list(
nsims = 3,
initial_pop = initial_pop,
initial_infected = 40,
n_steps = 365*20,
popsumm_frequency=30,
fast_edgelist=F,
plot_nw=T)

evoparams <- do.call(evonet_setup,param_list)

#Alternative 1, without argument list:
# evoparams <- evonet_setup(nsims=1,initial_pop=initial_pop,
#   initial_infected=20)

#Alternative 2:
#evoparams <- evonet_setup()
#evoparams$nsim=1
#evoparams$initial_pop=initial_pop


#--------------------------------------------------------------

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
"deaths",
"births",
"summary_module")

#--------------------------------------------------------------

evomodel <- evorun(modules,evoparams,nw)

#--------------------------------------------------------------

#to specify name of output pdf and pathway, use arguments
# name="xyz.pfd",outpath="/path to folder"
evoplot(model=evomodel)

#--------------------------------------------------------------

