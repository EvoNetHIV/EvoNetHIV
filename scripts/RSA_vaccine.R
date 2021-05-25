library(evonet)
#Core model parameters
INITIAL.POP <- 10000
MODEL.YEARS <- 10
INITIAL.INFECTED <- 250


param_list <-  list(
### basic model parameters
  initial_pop = INITIAL.POP,
  initial_infected = INITIAL.INFECTED,
  n_steps = MODEL.YEARS*365,
  popsumm_frequency=30,
  nsims = 5,
  ncores = 5,
  plot_nw=F,
  min_age = 15, #youngest agent age
  max_age = 55, #oldest agent age
  vaccine_age_range = c(15, 18), #age range that can be vaccinated
  model_sex = "hetero", #heterosexual model (vs "msm" model)
  initial_agedata_male   = "south_africa_male_15_to_100_2014",
  initial_agedata_female = "south_africa_female_15_to_100_2014",
  asmr_data_male        =   "south_africa_male_1990",
  asmr_data_female       = "south_africa_female_1990", # vaccine model parameters
  
####vaccine parameters
  vaccine_model = T,
   
  #wax/wane
  vacc_type="linear", #vacc. eff (phi) increases from initial value to 1 based on "vacc_phi_daily_increase"
  initial_phi_value= .0001,
  vacc_phi_daily_increase = 1/(7*30), #daily phi value increment, phi goes from 0 to one in 7 months
  vacc_min_efficacy_duration = 1*365, #minimum time vaccine effective after 1st dose,can start waning after this period
  vaccine_waning_type = "cliff-edge", #or "daily_prob" or "exponential"
  vacc_exp_decline_rate = -0.004,#about 5 year exponential decline from 1 to near 0,  exp(rate*(1:(5*365)))
  
  #coverage & rollout 
  fraction.vaccinated = 0.7,
  vaccine.rollout.year = 5, #timestep = value x 365
  vaccine.rollout.duration.years = 0, #value of "0", means all at once, values 
  # >1 mean constant daily vacc. rate during that time to reach
  
  #vaccine efficacy
  vaccine.efficacy.by.mark = list( mark1 = c( "strain1" = 0.7 )), 
  initial.mark.distribution = list( mark1 = c( "strain1" = 1 )), 
  
### network parameters
  #parameterization for heterosexual model with two age-based risk groups, younger group
  # has about 60% higher mean degreee
  age_nw_groups <- list( c(17.0,30),c(30,55)), # (age1,age2] 
  nw_form_terms <-  "~edges + nodefactor('att1')+ offset(nodematch('sex', diff=FALSE))",
  target_stats <- c(INITIAL.POP*0.4,INITIAL.POP*0.4) ,
  nw_coef_form  <- -Inf)



#calculate number of marks based on length of parameter vaccine.efficacy.by.mark
param_list$no_marks <- length(param_list$vaccine.efficacy.by.mark )

#---- create EpiModel parameter list with updated values
evoparams <- do.call(evonet_setup,param_list)

#---- network setup 
nw <- nw_setup(evoparams)

#---  Create list of modules (core processes) to run for input into epimodel_control_fxn() below
modules <- c(
  "vaccine_dynamics",
  "aging",
  "testing",
  "treatment",
  "viral_update",
  "coital_acts",
  "transmission",
  "evo_departures",
  "evo_arrivals",
  "summary_module")
#-- run model
evomodel <- evorun(modules,evoparams,nw)
save(evomodel,file="SA_BMGF_LeakyVaccine.RData")
# -- plot results
evoplot(model=evomodel,name="SA_BMGF_LeakyVaccine")