##
## MIDAS Vaccine Run Script
##

# 1. Setup ----------------------------------------------------------------

library("evonet")

# options(error=recover)
# devtools::load_all("pkg") #if local changes made


# 2. Parameterization -----------------------------------------------------

#debugonce( param_evonet)
#debugonce(get_args)

if(T){
  
  #parameter values used in multiple locations
  START.POPULATION.N = 200
  START.INFECTED.N   = 30
  MODEL.DAYS         = 365*20
  MAX_AGE             = 55
  N.SIMS             = 2
  N.CORES            = 2
  MODEL.NAME         = "vacc_sens_0.625_ve_0.8_cov_0.5_rc_0.7"

  
  params <- param_evonet(
  
  nsims                  = N.SIMS  ,
  ncores                 = N.CORES,
  
  ## Override selected default parameters
  initial_pop            = START.POPULATION.N,
  initial_infected       = START.INFECTED.N,
  model_sex              = "hetero",
  n_steps                = MODEL.DAYS,
  popsumm_frequency      = 30,
  #poisson_birth_lambda   = 0.0137*(initial_pop/100),
  vl_peak_agent_flag     = T,
  fast_edgelist          = T,
  mean_sex_acts_day      = 0.36,
  min_age                = 16,
  max_age               = MAX_AGE  ,
  sti_prob               = 0.692,
  asmr_data_male         = "south_africa_male_1990",
  asmr_data_female       = "south_africa_female_1990",
  initial_agedata_male   = "south_africa_male_16_to_100_1990",
  initial_agedata_female = "south_africa_female_16_to_100_1990",
  trans_RR_STI           = 2.14,
  AverageLogSP0          = 4.83,
  trans_lambda           = 0.000720,
  prop_AI                = 0.077,
  mean_prop_acts_AI      = 0.235,
  sd_prop_acts_AI        = 0.120,
  
  
  ## Circumcision parameters
  circum_prob        = 0.3,
  circum_prob_chg    = c(0.45,   0.5,    0.55,   0.6,    0.65,   0.7,    0.75,   0.85),
  circum_prob_yr_chg = c(12*365, 13*365, 15*365, 16*365, 18*365, 20*365, 22*365, 24*365),
  
  ## Condom use parameters
  condom_prob        = 0,# This value will be used only for coital acts at first timestep. Value will be reassigned by Hill function in subsequent timesteps.
  condom_prob_change = T,
  
  ## Risk compensation in form of decreased condom use among those who are vaccinated
  risk_comp_cond    = T,
  risk_comp_cond_rr = 0.7,
  
  ## Treatment parameters
  trtmnt_sex_age            = T,
  cd4_treatment_threshold   = 4, # Treatment eligibility is initially <200
  start_treatment_campaign  = c(0, 21*365, 24*365, 26*365), # Treatment eligibility changes at years 21, 24, and 26
  cd4_trt_guidelines_chgs   = list(4, 3:4, 2:4, 1:4), # Treatment eligibility changes over time from <200, <350, <500, and all +
  tx_in_acute_phase         = T,
  cov_prob                  = c(0, 0.010, 0.021, 0.030, 0.049, 0.100, 0.191, 0.283, 0.402, 0.560), # 0.56 from UNAIDS estimate, 2016
  cov_prob_yrs              = c(0, 11:18, 27), # Years at which coverage changes 
  cov_prob_scal = matrix(c(0.569, 1.240, 1.240, 0.421, 0.919, 0.919), ncol = 2, 
                                      dimnames = list(c("15-24", "25-34", "35+"), c("f", "m"))),
  cov_prob_ageg = list(c(15, 25), c(25, 35), c(35, MAX_AGE   + 1)),
  
  ## Vaccination parameters
  vacc_wane               = T,
  start_vacc_campaign     = (29*365):(44*365),
  perc_virus_vaccine_sens = 0.625, # prop_virus_sens * 100% of virus is sensitive and VE at 24 months is ve_24_months
  ve_24_months            = 0.8, # Value used in waning vaccine-induced immunity function to determine daily RR against sensitive virus
  perc_vaccinated         = 1/(3*365), # daily probability of vaccination is equal to prob_care/time to achieve target coverage
  vacc_eff_duration       = 365*5, # After 5 years, vaccinated individuals will receive a booster vaccine and immunity will return to initial value
  prob_care               = 0.5)

  
}



# 3.  ERGM Specification --------------------------------------------------


nw_form_terms = ~edges + concurrent('sex') + absdiffby('age', 'sex', 3) + offset(nodematch('sex', diff=FALSE))
nw_coef_form  = -Inf
nEdges                  = 0.76*(START.POPULATION.N/2)
target_stats  = c(nEdges, 0.079*(START.POPULATION.N/2), 0.176*(START.POPULATION.N/2), nEdges)
relation_dur  = 1409


nw = setup_initialize_network(params)

coef.diss = dissolution_coefs(dissolution = ~offset(edges),
                               duration = relation_dur,
                               d.rate = 3e-05)
est = netest(nw = nw,
              formation = nw_form_terms,
              target.stats = target_stats,
              coef.form = nw_coef_form ,
              coef.diss = coef.diss)



# 4. Initial Conditions ---------------------------------------------------

status = rep("s", START.POPULATION.N)
status[sample(1:START.POPULATION.N, size = START.INFECTED.N)] = "i"
init = init.net(status.vector = status)

# 5. Control Settings ------------------------------------------------------


control <- control.net(type=NULL, #necessary for stochastic network models
      #---------------------------------------------------  
      #user will not likely have to change settings in this section    
       nsteps=  MODEL.DAYS ,      nsims = N.SIMS,  ncores = N.CORES ,
       resimulate.network = TRUE, tergmLite = TRUE,
       save.nwstats = TRUE,       nwstats.formula = "formation",
       verbose = F,               verbose.int = 1e5,
       skip.check = TRUE,         raw.output = FALSE,
       initialize.FUN = initialize_module,
       nwupdate.FUN = evo_nwupdate, #have to use slightly modified version of default "nwupdate.net"
       save.other    = c("attr","pop","param","nw","coital_acts_list",
                       "popsumm","vl_list","InfMat","age_list","el",
                       "partner_list"),
     
    #user modules--------------------------------------------
     #user will have to specify desired modules (sets of functions representing key processes)
     #note: modules run in order given
     aging.FUN = aging,
     testing.FUN = testing,
     social_treatment_sex_age.FUN = social_treatment_sex_age,
     social_treatment_vaccination_waning.FUN = social_treatment_vaccination_waning,
     viral_update.FUN = viral_update,
     coital_acts.FUN = coital_acts,
     transmission.FUN = transmission,
     evo_departures.FUN = evo_departures,
     evo_arrivals.FUN = evo_arrivals,
     summary_module.FUN = summary_module )     
  

# 5. Epidemic Simulation --------------------------------------------------

evomodel = netsim(x = est,
                   param = params,
                   init = init,
                   control = control)

evoplot(evomodel,outpath=getwd(),name=MODEL.NAME)


#debugonce(evoplot)
