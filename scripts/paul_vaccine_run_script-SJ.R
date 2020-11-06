
##
## MIDAS Vaccine Run Script
##
##



# 1. Setup ----------------------------------------------------------------

library("evonet")
require("parallel")

# options(error=recover)
# devtools::load_all("pkg") #if local changes made



# 2. Parameterization -----------------------------------------------------

## Quick-set params
THE.SEED <- 123;
THE.MODEL.ID <- "model_1" #"model_1b","model_2","model_2b";
NUM.SIMS <- 1;
NUM.CORES <- parallel::detectCores();
NUM.YEARS <- 15;

START.POPULATION.N <- 500;
START.PREVALENCE <- 0.10;

VACCINE.ROLLOUT.YEAR <- 5;
VACCINE.ROLLOUT.YEARS.DURATION <- 1;
FRACTION.VACCINATED <- 0.60;

VACCINE.EFFICACY.YEARS <- 3;

set.seed(THE.SEED)

vacc_model_id <- THE.MODEL.ID;

# initial values needed to calculate other parameter values
initial_pop = START.POPULATION.N

# how long (years) to all eligible are vaccinated
years_to_max_coverage = VACCINE.ROLLOUT.YEARS.DURATION

# maximum proportion of population eligible for vaccination (.99 upper limit)
max_perc_vaccinated = FRACTION.VACCINATED
vaccine.efficacy.years = 3
revaccination.eligibility.years = 3

# models 1/1b/2/2b, proportion (percentage) decrease in trans probs due 
# to vaccine for vaccine model "1" (baseline vaccine model),
vaccine.efficacy.by.mark = c("sensitive" = 0.8)
initial.mark.distribution = c("sensitive" = 1) 

## Built-in EvoNet parameterization functions
##  Wrapping evonet_setup
##  TODO (SJ): rewrite this as param_evonet
params <- input_params(nsims            = NUM.SIMS,   #number of simulations (replicates)
                       ncores           = NUM.CORES,
                       popsumm_frequency  = 30,   #frequency of timesteps (days) to calculate summary stats
                       fast_edgelist      = TRUE,  #no longer needed as of EpiModel v 2.0
                       min_spvl_allowed = .5,
                       n_steps           = 365*NUM.YEARS,
                       initial_pop       = initial_pop,
                       initial_infected  = initial_pop*START.PREVALENCE,
                       target_stats         = initial_pop*0.7/2, # network edge density parameter
                       vl_peak_agent_flag   = TRUE,   #default FALSE; if TRUE, allow cor bn spvl and pvl.
                       plot_nw            = FALSE,
                       perc_virus_vaccine_sens = 1)
params <- input_parameters_derived(params)

params$evonet.initialization.timestep = 2
params$fraction.vaccinated = FRACTION.VACCINATED
params$vaccine.rollout.year = VACCINE.ROLLOUT.YEAR
params$daily.vaccination.rate = FRACTION.VACCINATED / (VACCINE.ROLLOUT.YEARS.DURATION * 365)
params$vaccine.efficacy.days = 365*vaccine.efficacy.years
params$daily.vaccine.reversion.rate = 1 / (vaccine.efficacy.years * 365)
params$revaccination.eligibility.days = 365*revaccination.eligibility.years
params$vaccine.efficacy.by.mark = vaccine.efficacy.by.mark
params$initial.mark.distribution = initial.mark.distribution


evoparams <- params

#---------------------

# specify which processes/modules to be run
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

## TODO (SJ): unpack nw_setup to allow for flexible ERGM parameterization
# estimate network
nw <- nw_setup(evoparams) # Sets up the initial network

# run model
evomodel <- evorun(modules, evoparams, nw)


## Below is unpacked version of evorun
## TODO (SJ): rewrite this as init_evonet and control_evonet
module_list <- lapply(modules, get)
names(module_list) <- paste(modules, ".FUN", sep = "")

evo_module_list <- c(
  list("plot_network.FUN" = plot_network_fxn),  
  module_list)

evocontrol <- setup_epimodel_control_object(evonet_params = params,
                                            module_list   = evo_module_list)

status <- rep("s", params$initial_pop)
status[sample(1:params$initial_pop,size=params$initial_infected)]<- "i"
infected_list <- EpiModel::init.net(status.vector = status)
    
evomodel  <- netsim(x = nw, 
                    param = params,
                    init = infected_list,
                    control = evocontrol)


#assign model names
model_name = paste("vaccine_model.RData",sep="")
#save model
save(evomodel,file=model_name)
#plot results, prints to screen and saves pdf to working directory, getwd() to see
evoplot(evomodel,name=model_name) #plot to screen (if single sim/core) and write to pdf

detach( "vaccination" )
