library(evonet)

if(T){

  scale_factor=4
  
  params_list=list()
  
  params_list$nsims                  <- 10
  params_list$ncores                 <- 10
  params_list$plot_nw                     <-  F
  ## Override selected default parameters
  params_list$initial_pop            <- 10000
  params_list$initial_infected       <- 250
  params_list$model_sex              <- "hetero"
  params_list$n_steps                <- 365 * 10
  params_list$popsumm_frequency      <- 30
  params_list$save_trans_probs       <- F
  
  #params_list$poisson_birth_lambda   <- 0.0137*(params_list$initial_pop/100)
  params_list$vl_peak_agent_flag     <- T
  params_list$fast_edgelist          <- T
  params_list$mean_sex_acts_day      <- 0.25
  params_list$min_age                <- 15
  params_list$max_age                <- 55
  params_list$sti_prob               <- 0.23
  params_list$asmr_data_male         <- "south_africa_male_1990"
  params_list$asmr_data_female       <- "south_africa_female_1990"
  params_list$initial_agedata_male   <- "south_africa_male_15_to_100_2014"
  params_list$initial_agedata_female = "south_africa_female_15_to_100_2014"
  params_list$trans_RR_STI           <- 2.14
  params_list$AverageLogSP0          <- 4.83
  params_list$trans_lambda           <- 0.000720*5
  params_list$prop_AI                <- 0.077
  params_list$mean_prop_acts_AI      <- 0.235
  params_list$sd_prop_acts_AI        <- 0.120
  
  
  ## Circumcision parameters
  params_list$circum_prob        <- 0.123
 
  ## Condom use parameters
  params_list$condom_prob        <- .28
  params_list$condom_prob_change <- F
  
  
## Treatment parameters
  params_list$tx_type                  = "random" 
  params_list$mean_trtmnt_delay        = 7      #default
  params_list$start_treatment_campaign = 10*365 
  params_list$proportion_treated       = .13     #default 1
  params_list$tx_in_acute_phase        = TRUE   #default FALSE
  params_list$min_inf_time_for_treat   = 18      #default
  params_list$testing_model            = "interval" #default
  params_list$mean_test_interval_male  = 365*1       #default
  params_list$disclosure_prob          = 0.6       #default 0.9
  params_list$vl_full_supp        = 100 
  params_list$ save_vl_list = FALSE
  params_list$test_result_delay = 35
  
##vaccine parameters
  params_list$vaccine_model = T
  
  params_list$vacc_type="linear" #vacc. eff (phi) increases from initial value to 1 based on "vacc_phi_daily_increase"
  params_list$initial_phi_value= .0001
  params_list$vacc_phi_daily_increase = 1/(7*30) #daily phi value increment, phi goes from 0 to one in 7 months
  params_list$vacc_min_efficacy_duration = 1*365 #minimum time vaccine effective after 1st dose,can start waning after this period
  params_list$vaccine_waning_type = "cliff-edge" #or "daily_prob" or "exponential"
  params_list$vacc_exp_decline_rate = -0.004#about 5 year exponential decline from 1 to near 0,  exp(rate*(1:(5*365)))
  
  #coverage & rollout 
  params_list$fraction.vaccinated = 0.7
  params_list$vaccine.rollout.year = 5 #timestep = value x 365
  params_list$vaccine.rollout.duration.years = 0 #value of "0", means all at once, values 
  
  
  # params_list$daily.vaccine.reversion.rate= 1/(100*365)#eg, permanent
  # params_list$revaccination.eligibility.years = 3
  
  # leaky
  params_list$vaccine.efficacy.by.mark = list( mark1=c( "strain1" = 0.7 ) )
  params_list$initial.mark.distribution = list(mark1= c( "strain1" = 1 ))
  
  # #AON
  # params_list$vaccine.efficacy.by.mark = list(mark1= c( "strain1" = 1.0,"strain2"=0.0 ) )  #models 1/1b/2/2b, proportion (percentage) decrease in trans probs due to vaccine for vaccine model "1" (baseline vaccine model),
  # params_list$initial.mark.distribution = list( mark1= c( "strain1" = 0.7,"strain2"=0.3 )) ## TODO: This is an idea I'm working on, so I can make one constructor for all existing models 1,1a,2,and 2a, and any discrete-mark variant of that.
  
  
  params_list$vaccine_trial = FALSE
  # params_list$prop_vaccinated_placebo = 0.5
  # params_list$initial_trial_participants = 3 * params_list$initial_pop/1000
  # params_list$trial_status_time_switch = 365*2
  

## Network formation parameters
  
  params_list$nw_form_terms <- "~edges + concurrent('sex') + absdiffby('age', 'sex', 3) + offset(nodematch('sex', diff=FALSE)) "
  params_list$nw_coef_form  <- c(-Inf)
  nEdges                  <- 0.76*(params_list$initial_pop/2)
  params_list$target_stats  <- c(nEdges, 0.079*(params_list$initial_pop), 0.176*(nEdges), nEdges)
  params_list$relation_dur <- 220

  
  params_list$no_marks <- length(params_list$vaccine.efficacy.by.mark )
  
} #end if T



#create parameter list with updated values


evoparams <- do.call(evonet_setup,params_list)


nw <- nw_setup(evoparams)



#modules to run each timestep
modules <- c(
  "aging",
  "testing",
  "vaccine_dynamics",
  "social_treatment_sex_age",
  "social_testing_diagnosis_module", 
  "social_treatment_module_john_v3",         
  "social_treatement_prep_SES",
  "viral_update",
  "coital_acts",
  "transmission",
  "evo_departures",
  "evo_arrivals",
  "summary_module")

options(error=recover) # go into debug mode on error
evomodel <- evorun(modules,evoparams,nw)

model_name = paste("5.27.2021_SA_BMGF_AON_newNW.RData",sep="")

#save model to working directory
save(evomodel,file=model_name)

#generate and save pdf of autoplots of summary measures
evoplot(evomodel,name=model_name)


#

