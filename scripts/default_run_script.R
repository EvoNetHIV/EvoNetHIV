#NOTE: when working on CSDE Windows Server and R updates remove
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
initial_pop=100

param_list=list(
nsims = 1,
initial_pop = initial_pop,
initial_infected = 20,
n_steps = 365*5,
target_stats= .35*initial_pop,
age_dist_new_adds="mixed",
VL_Function="aim2",
popsumm_frequency=30,
fast_edgelist=TRUE,
initial_agedata_male = "linear_decrease",
initial_agedata_female= "linear_decrease",
plot_nw=T,
save_network=F)

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
estimated_nw <- nw_setup(evoparams)

#--------------------------------------------------------------


#---  Create list of modules to run for input into epimodel_control_fxn() below
# "initialize_module" and "resim.nets" modules automatically added, no need to specify

module_list<- list(
  "plot_nw.FUN"        = plot_network_fxn,  
  "aging.FUN"          = vital_aging_module,
  "testing.FUN"        = social_testing_diagnosis_module,
  "treatment.FUN"      = social_treatment_module,
  "update_vl.FUN"      = viral_update_gamma,
  "update_cd4.FUN"     = viral_update_cd4_daily, #viral_update_cd4_diff_eqn
  "coital_acts.FUN"    = social_coital_acts_module,
  "trans.FUN"          = transmission_main_module,
  "trans_book.FUN"     = transmission_bookkeeping_module,
  "trans_cd4.FUN"      = transmission_cd4_module,
  "deaths.FUN"         = vital_deaths_module,
  "births.FUN"         = vital_births_module,
  "social_trans.FUN"   = social_attribute_transition_module,
  "summary.FUN"        = summary_module)


#--------------------------------------------------------------

evomodel <- evorun(module_list,evoparams,estimated_nw)

#--------------------------------------------------------------

#to specify name of output pdf and pathway, use arguments
# name="xyz.pfd",outpath="/path to folder"
evoplot(model=evomodel)

#--------------------------------------------------------------

