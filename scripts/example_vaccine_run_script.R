#---------------------
library(evonet)

options(error=recover)

devtools::load_all("pkg") #if local changes made

#if new functions added, export to namespace
#devtools::document(file.path(getwd(),"pkg"))

#---------------------

set.seed(123)
#-----------------------------------

#what vaccine model?
vacc_model_id <- "model_1" #"model_1b","model_2","model_2b"

#-----------------------------------

#initial values needed to calculate other parameter values
initial_pop       = 500
years_to_max_coverage = 1  #how long (years) to all eligible are vaccinated
max_perc_vaccinated =0.80   #maximum proportion of population eligible for vaccination (.99 upper limit)

#-----------------------------------

#specify  parameters to change from default or that are frequently changed
param_list=list(
  nsims            = 1,
  ncores           = 1,
  popsumm_frequency  = 30,   #frequency of timesteps (days) to calculate summrary stats
  fast_edgelist      = TRUE,
  min_spvl_allowed = .5,
  n_steps           = 365*15,
  initial_pop       = initial_pop,
  initial_infected  = initial_pop*.10,
  target_stats         = initial_pop*0.7/2,
  vl_peak_agent_flag   = TRUE,   #default FALSE
  plot_nw            = FALSE, #speed things up a bit for single sim/core runs
  
  #Vaccine parameters ----------------------------------------- #
  start_vacc_campaign = (5*365):(50*365),    # all vaccine models
  max_perc_vaccinated = max_perc_vaccinated , #all vacc. models, maximum percent/proportion of population to be vaccinated
  perc_vaccinated_rate = (max_perc_vaccinated /(1-max_perc_vaccinated ))/(years_to_max_coverage*365), #all vacc.models, prob of daily vaccination
  vacc_eff_duration = 365*3, #all vacc models
  vacc_trans_prob_decrease =0.8,  #models 1/1b/2/2b, proportion (percentage) decrease in trans probs due to vaccine for vaccine model "1" (baseline vaccine model),
 
   #model 1 specific
   perc_virus_vaccine_sens = 0.5, #model 1, proportion of initial viruses/marks that are sensitive to vaccine 
  
  #model 1b specific
  mu_mean  = 0.5, #model 1b, equilibrium mu value 
  vacc_time_to_mu_mean  = 365, # model 1b, days to reach mu_mean for infected agents (equilibrium value)
  vacc_model_min_mu = 0.01, #model 1b, lower bound of uniform dist. for initial dist. of mu values for model 1b
  vacc_model_max_mu = 0.99, #model 1b, upper bound of uniform dist. for initial dist. of mu values for model 1b
  
  #model 2 specific
  prob_loci_1               = 0.5, #probability of mu=1 (loci 1) for initially infected agent (model 2) 
  prob_loci_2               = 0.5,#probability of mu=1 (loci 2) for initially infected agent (model 2)
  trans_prob_decrease_scalar_model2 = 0.5 #proportional decrease in transmission for infected agents with
  # 0/1 or 1/0 mu values relative to mu=1/1 at each loci which
  #is equal to param. "trans_prob_decrease_scalar_model2"
) #end of parameter list


#---------------------

#what vaccine model to implement?
param_list$vacc_model_id <- "model_1"

#---------------------
#turn parameter list into "epimodel" parameter object 
evoparams <- do.call(evonet_setup,param_list)

#---------------------

#specify which processes/modules to be run
modules <- c(
  "initialize_vaccine_agents",
  "update_mu_and_sigma",
  "initialize_and_update_phi",
  "aging",
  "testing",
  "treatment",
  "viral_update",
  "coital_acts",
  "transmission",
  "evo_departures",
  "evo_arrivals",
  "summary_module")


#estimate network
nw <- nw_setup(evoparams) # Sets up the initial network

#run model
evomodel <- evorun(modules,evoparams,nw)

#assign model names
model_name = paste("vaccine_model.RData",sep="")
#save model
save(evomodel,file=model_name)
#plot results, prints to screen and saves pdf to working directory, getwd() to see
evoplot(evomodel,name=model_name) #plot to screen (if single sim/core) and write to pdf
