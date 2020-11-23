
##
## MIDAS Vaccine Run Script
##

# 1. Setup ----------------------------------------------------------------

library("evonet")

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

params <- param_evonet(evonet.initialization.timestep = 2,
                       initial_pop = START.POPULATION.N,
                       fraction.vaccinated = FRACTION.VACCINATED,
                       vaccine.rollout.year = VACCINE.ROLLOUT.YEAR,
                       daily.vaccination.rate = FRACTION.VACCINATED /
                                               (VACCINE.ROLLOUT.YEARS.DURATION * 365),
                       vaccine.efficacy.days = 365*vaccine.efficacy.years,
                       daily.vaccine.reversion.rate = 1 / (vaccine.efficacy.years * 365),
                       revaccination.eligibility.days = 365*revaccination.eligibility.years,
                       vaccine.efficacy.by.mark = vaccine.efficacy.by.mark,
                       initial.mark.distribution = initial.mark.distribution)


# 3.  ERGM Specification --------------------------------------------------

nw <- setup_initialize_network(params)

formation <- ~edges + offset(nodematch("role", diff = TRUE, levels = 1:2))
target.stats <- START.POPULATION.N*0.7/2

coef.diss <- dissolution_coefs(dissolution = ~offset(edges),
                               duration = 50,
                               d.rate = 3e-05)

est <- netest(nw = nw,
              formation = formation,
              target.stats = target.stats,
              coef.form = c(-Inf, -Inf),
              coef.diss = coef.diss)



# 4. Initial Conditions ---------------------------------------------------

initial_infected <- 20

status <- rep("s", START.POPULATION.N)
status[sample(1:START.POPULATION.N, size = initial_infected)] <- "i"
init <- init.net(status.vector = status)



# 5. Control Settings ------------------------------------------------------

# Adds three new module/fx pairs to defaults
control <- control_evonet(nsteps = 730,
                          initialize_vaccine_agents.FUN = initialize_vaccine_agents,
                          update_mu_and_sigma.FUN = update_mu_and_sigma,
                          initialize_and_update_phi.FUN = initialize_and_update_phi)


# 5. Epidemic Simulation --------------------------------------------------

evomodel <- netsim(x = est,
                   param = params,
                   init = init,
                   control = control)

