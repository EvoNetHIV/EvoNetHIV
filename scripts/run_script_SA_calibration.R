library(evonet)

if(T){
prop_virus_sens <- 0.625
ve_24_months    <- 0.8
prop_coverage   <- 0.5
risk_comp_rr    <- 0.7
model_name      <- "vacc_sens_0.625_ve_0.8_cov_0.5_rc_0.7"

params_list=list()

params_list$nsims                  <- 10
params_list$ncores                 <- 10
params_list$plot_nw               <-  F
## Override selected default parameters
params_list$initial_pop            <- 30000
params_list$initial_infected       <- 60
params_list$model_sex              <- "hetero"
params_list$n_steps                <- 365 * 30
params_list$popsumm_frequency      <- 30
#params_list$poisson_birth_lambda   <- 0.0137*(params_list$initial_pop/100)
params_list$vl_peak_agent_flag     <- T
params_list$fast_edgelist          <- T
params_list$mean_sex_acts_day      <- 0.36
params_list$min_age                <- 16
params_list$max_age                <- 55
params_list$sti_prob               <- 0.692
params_list$asmr_data_male         <- "south_africa_male_1990"
params_list$asmr_data_female       <- "south_africa_female_1990"
params_list$initial_agedata_male   <- "south_africa_male_16_to_100_1990"
params_list$initial_agedata_female <- "south_africa_female_16_to_100_1990"
params_list$trans_RR_STI           <- 2.14
params_list$AverageLogSP0          <- 4.83
params_list$trans_lambda           <- 0.000720
params_list$prop_AI                <- 0.077
params_list$mean_prop_acts_AI      <- 0.235
params_list$sd_prop_acts_AI        <- 0.120


## Circumcision parameters
params_list$circum_prob        <- 0.3
params_list$circum_prob_chg    <- c(0.45,   0.5,    0.55,   0.6,    0.65,   0.7,    0.75,   0.85)
params_list$circum_prob_yr_chg <- c(12*365, 13*365, 15*365, 16*365, 18*365, 20*365, 22*365, 24*365)

## Condom use parameters
params_list$condom_prob        <- 0 # This value will be used only for coital acts at first timestep. Value will be reassigned by Hill function in subsequent timesteps.
params_list$condom_prob_change <- T

## Risk compensation in form of decreased condom use among those who are vaccinated
params_list$risk_comp_cond    <- T
params_list$risk_comp_cond_rr <- risk_comp_rr

## Treatment parameters
params_list$trtmnt_sex_age            <- T
params_list$cd4_treatment_threshold   <- 4 # Treatment eligibility is initially <200
params_list$start_treatment_campaign  <- c(0, 21*365, 24*365, 26*365) # Treatment eligibility changes at years 21, 24, and 26
params_list$cd4_trt_guidelines_chgs   <- list(4, 3:4, 2:4, 1:4) # Treatment eligibility changes over time from <200, <350, <500, and all +
params_list$tx_in_acute_phase         <- T
params_list$cov_prob                  <- c(0, 0.010, 0.021, 0.030, 0.049, 0.100, 0.191, 0.283, 0.402, 0.560) # 0.56 from UNAIDS estimate, 2016
params_list$cov_prob_yrs              <- c(0, 11:18, 27) # Years at which coverage changes 
params_list$cov_prob_scal <- matrix(c(0.569, 1.240, 1.240, 0.421, 0.919, 0.919), ncol = 2, 
                                  dimnames = list(c("15-24", "25-34", "35+"), c(0, 1)))
params_list$cov_prob_ageg <- list(c(15, 25), c(25, 35), c(35, params_list$max_age + 1))

## Vaccination parameters
params_list$vacc_wane               <- T
params_list$start_vacc_campaign     <- (29*365):(44*365)
params_list$perc_virus_vaccine_sens <- prop_virus_sens # prop_virus_sens * 100% of virus is sensitive and VE at 24 months is ve_24_months
params_list$ve_24_months            <- ve_24_months # Value used in waning vaccine-induced immunity function to determine daily RR against sensitive virus
params_list$perc_vaccinated         <- 1/(3*365) # daily probability of vaccination is equal to prob_care/time to achieve target coverage
params_list$vacc_eff_duration       <- 365*5 # After 5 years, vaccinated individuals will receive a booster vaccine and immunity will return to initial value
params_list$prob_care               <- prop_coverage # eligible_care == 1 is vaccination criteria, so a maximum of prop_coverage * 100% susceptible agents will be vaccinated

## Network formation parameters

params_list$nw_form_terms <- "~edges + concurrent('sex') + absdiffby('age', 'sex', 3) + offset(nodematch('sex', diff=FALSE))"
params_list$nw_coef_form  <- -Inf
nEdges                  <- 0.76*(params_list$initial_pop/2)
params_list$target_stats  <- c(nEdges, 0.079*(params_list$initial_pop/2), 0.176*(params_list$initial_pop/2), nEdges)
params_list$relation_dur  <- 1409

#params_list$age_dist <- seq(50, 10, -10/9)/1110

}



#create parameter list with updated values
evoparams <- do.call(evonet_setup,params_list)
evoparams$plot_nw=F


#network setup
nw <- nw_setup(evoparams)


#modules to run each timestep
modules <- c(
  "aging",
  "testing",
  "social_treatment_sex_age",
  "social_treatment_vaccination_waning",
  "viral_update",
  "coital_acts",
  "transmission",
  "evo_departures",
  "evo_arrivals",
  "summary_module")

options(error=recover) # go into debug mode on error
evomodel <- evorun(modules,evoparams,nw)

save(evomodel,file="evomodel.RData")

#to specify name of output pdf and pathway, use arguments
# name="xyz.pfd",outpath="/path to folder"
evoplot(model=evomodel)

