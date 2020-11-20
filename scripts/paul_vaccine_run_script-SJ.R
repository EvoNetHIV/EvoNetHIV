
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

THE.SEED <- 123
set.seed(THE.SEED)

THE.MODEL.ID <- "model_1" #"model_1b","model_2","model_2b";
NUM.SIMS <- 1
NUM.CORES <- parallel::detectCores()
NUM.YEARS <- 15

START.POPULATION.N <- 500
START.PREVALENCE <- 0.10

VACCINE.ROLLOUT.YEAR <- 5
VACCINE.ROLLOUT.YEARS.DURATION <- 1
FRACTION.VACCINATED <- 0.60

VACCINE.EFFICACY.YEARS <- 3
vacc_model_id <- THE.MODEL.ID

vaccine.efficacy.years <- 3
revaccination.eligibility.years <- 3

vaccine.efficacy.by.mark = c("sensitive" = 0.8)
initial.mark.distribution = c("sensitive" = 1)

evoparams <- param_evonet(evonet.initialization.timestep = 2,
                          initial_pop = START.POPULATION.N,
                          fraction.vaccinated = FRACTION.VACCINATED,
                          vaccine.rollout.year = VACCINE.ROLLOUT.YEAR,
                          daily.vaccination.rate = FRACTION.VACCINATED / (VACCINE.ROLLOUT.YEARS.DURATION * 365),
                          vaccine.efficacy.days = 365*vaccine.efficacy.years,
                          daily.vaccine.reversion.rate = 1 / (vaccine.efficacy.years * 365),
                          revaccination.eligibility.days = 365*revaccination.eligibility.years,
                          vaccine.efficacy.by.mark = vaccine.efficacy.by.mark,
                          initial.mark.distribution = initial.mark.distribution)


# 3.  ERGM Specification --------------------------------------------------

nw <- nw_setup(evoparams) # Sets up the initial network

nw <- setup_initialize_network(evoparams)

est <- netest(nw            =  nw,
              formation     =  as.formula(evoparams$nw_form_terms),
              target.stats  =  evoparams$target_stats,
              coef.form     =  evoparams$nw_coef_form,
              constraints   =  as.formula(evoparams$nw_constraints),
              verbose       =  FALSE,
              coef.diss     =  dissolution_coefs( dissolution =  as.formula(evoparams$dissolution),
                                                  duration    =  evoparams$relation_dur,
                                                  d.rate      =  evoparams$d_rate))


# 4. Module Settings ------------------------------------------------------

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



# 5. Epidemic Simulation --------------------------------------------------

# run model
evomodel <- evorun(modules, evoparams, est)


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
status[sample(1:params$initial_pop, size = params$initial_infected)] <- "i"
infected_list <- EpiModel::init.net(status.vector = status)

evomodel  <- netsim(x = nw,
                    param = params,
                    init = infected_list,
                    control = evocontrol)


#assign model names
model_name = paste("vaccine_model.RData",sep = "")
#save model
save(evomodel, file = model_name)
#plot results, prints to screen and saves pdf to working directory, getwd() to see
evoplot(evomodel, name = model_name) #plot to screen (if single sim/core) and write to pdf
