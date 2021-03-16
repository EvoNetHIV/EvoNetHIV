#Toy example of using ABC estimation with EvoNet to calibrate to
# external data (South Africa prevalence data for this example)
#Population size is deliberately small to decrease runtime
#Runtime about 13 hours
###############################################


library(evonet)
library(EasyABC)
options(error=recover) # go into debug mode on error


## Function to be passed to ABC_sequential function

get_params <- function(x) {

  set.seed(x[1])
  require(evonet)
  
  
  params_list=list()
  
  params_list$nsims                  <- 1
  params_list$ncores                 <- 1
  params_list$plot_nw                     <-  F

  ## Override selected default parameters
  params_list$initial_pop            <- 500
  params_list$initial_infected       <- 10
  params_list$model_sex              <- "hetero"
  params_list$n_steps                <- 365*25
  params_list$popsumm_frequency      <- 1
  params_list$poisson_birth_lambda   <- 0.0137*(params_list$initial_pop/100)
  
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
  params_list$trans_lambda           <- x[2]
  params_list$prop_AI                <- x[3]
  params_list$mean_prop_acts_AI      <- x[4]
  params_list$sd_prop_acts_AI        <- x[5]
  
  
  
  
  ## Circumcision parameters
  params_list$circum_prob        <- 0.3
  params_list$circum_prob_chg    <- c(0.45,   0.5,    0.55,   0.6,    0.65,   0.7,    0.75,   0.85)
  params_list$circum_prob_yr_chg <- c(12*365, 13*365, 15*365, 16*365, 18*365, 20*365, 22*365, 24*365)
  
  ## Condom use parameters
  params_list$condom_prob        <- 0 # This value will be used only for coital acts at first timestep. Value will be reassigned by Hill function in subsequent timesteps.
  params_list$condom_prob_change <- T
  
  ## Treatment parameters
  params_list$trtmnt_sex_age            <- T
  params_list$cd4_treatment_threshold   <- 4 # Treatment eligibility is initially <200
  params_list$start_treatment_campaign  <- c(0, 21*365, 24*365, 26*365) # Treatment eligibility changes at years 21, 24, and 26
  params_list$cd4_trt_guidelines_chgs   <- list(4, 3:4, 2:4, 1:4) # Treatment eligibility changes over time from <200, <350, <500, and all +
  params_list$tx_in_acute_phase         <- T
  params_list$cov_prob                  <- c(0, 0.01, 0.021, 0.030, 0.049, 0.100, 0.191, 0.283, 0.402, 0.78)
  params_list$cov_prob_yrs              <- c(0, 11:18, 23) # Years at which coverage changes 
  params_list$cov_prob_scal <- matrix(c(0.718571, 1.000558, 1.239242, 0.584926, 0.814467, 1.008758), ncol = 2, 
                                 dimnames = list(c("15-24", "25-34", "35+"),c(0, 1)))
  params_list$cov_prob_ageg <- list(c(15, 25), c(25, 35), c(35, params_list$max_age + 1))
  
  

  ## Network formation parameters
  
  params_list$nw_form_terms <- "~edges + concurrent('sex') + absdiffby('age', 'sex', 3) + offset(nodematch('sex', diff=FALSE))"
  params_list$nw_coef_form  <- -Inf
  nEdges                  <- 0.76*(params_list$initial_pop/2)
  fem_conc                <- x[6]
  male_conc               <- x[7]
  params_list$target_stats  <- c(nEdges, fem_conc*(params_list$initial_pop/2), male_conc*(params_list$initial_pop/2), nEdges)
  params_list$relation_dur  <- x[8]
  
  
#create parameter list with updated values
evoparams <- do.call(evonet_setup,params_list)

#network setup
nw <- nw_setup(evoparams)


#modules to run each timestep
modules <- c(
  "aging",
  "testing",
  "social_treatment_sex_age",
  "viral_update",
  "coital_acts",
  "transmission",
  "evo_departures",
  "evo_arrivals",
  "summary_popsumm_abc_only") 
  
   #note: "summary_popsumm_abc_only" only calculates prevalence data, the only data
   # needed for model fitting to speed things up.



evomodel <- evorun(modules,evoparams,nw)

## Calculate prevalence summary statistic from simulated data at years for which empirical data is available
sa.year   <- c(seq(1,13,1), 16, 19, 23) # Year 1 corresponds to end of 1990, 16 to end of 2005
#model.year <- sa.year * 365/evomodel$param[[1]]$popsumm_freq

model.year <- sa.year * 365

out1 <- evomodel$epi$prev_15to49[model.year,1] # Ages 15-49, 1990-2001, 2002, 2005, 2008, 2012

out2 <- c(evomodel$epi$prev_m_15to24[13 * 365,1], # 2002, males 15-24
          evomodel$epi$prev_f_15to24[13 * 365,1], # 2002, females 15-24
          evomodel$epi$prev_m_15to49[13 * 365,1], # 2002, males 15-49
          evomodel$epi$prev_f_15to49[13 * 365,1]) # 2002, females 15-49

out3 <- c(evomodel$epi$prev_m_15to24[16 * 365,1], # 2005, males 15-24
          evomodel$epi$prev_f_15to24[16 * 365,1], # 2005, females 15-24
          evomodel$epi$prev_m_15to49[16 * 365,1], # 2005, males 15-49
          evomodel$epi$prev_f_15to49[16 * 365,1]) # 2005, females 15-49

out4 <- evomodel$epi$prev_15to24[19 * 365,1] # 2008, 15-24

out5 <- c(evomodel$epi$prev_m_15to24[23 * 365,1], # 2012, males 15-24
          evomodel$epi$prev_f_15to24[23 * 365,1], # 2012, females 15-24
          evomodel$epi$prev_m_15to49[23 * 365,1], # 2012, males 15-49
          evomodel$epi$prev_f_15to49[23 * 365,1]) # 2012, females 15-49

out <- c(out1, out2, out3, out4, out5)

return(out)
}

## Specify priors for per-act infectivity and relationship duration
priors  <- list(c("unif", 0.0005005, 0.005), # trans_lambda
                c("unif", 0.01, 0.15),           # prop_AI 
                c("unif", 0.01, 0.60),           # mean_prop_acts_AI
                c("unif", 0.05, 0.30),           # sd_prop_acts_AI
                c("unif", 0.01, 0.15),           # fem_conc
                c("unif", 0.07, 0.20),           # male_conc
                c("unif", 254, 1971))            # rel_dur


## Specify prevalence targets for ABC fitting procedure
sa.prev <- c(.002, .005, .010, .019, .031, .048, .067, .088, .108, .126, .141, .153, .156, .162, .169, .188, # Ages 15-49, 1990-2001, 2002, 2005, 2008, 2012
             0.061, 0.120, 0.128, 0.177, # 2002: males, 15-24; females, 15-24; males, 15-49; females, 15-49
             0.044, 0.169, 0.117, 0.202, # 2005: males, 15-24; females, 15-24; males, 15-49; females, 15-49
             0.087,                      # 2008: 15-24
             0.029, 0.114, 0.145, 0.232  # 2012: males, 15-24; females, 15-24; males, 15-49; females, 15-49
)


runtime= system.time({
abc_out_original_small2 <- ABC_sequential(method = "Lenormand",
                    model = get_params,
                    prior = priors,
                    nb_simul = 100,
                    summary_stat_target = sa.prev,
                    p_acc_min = 0.01,
                    n_cluster = 40,
                    use_seed = TRUE)

save(abc_out_original_small2, file = "abc_out_original_small2.RData")

})
print(runtime)

##########

#
#plotting

vars=c("trans_lambda","prop_AI",'mean_prop_acts_AI',"sd_prop_acts_AI","fem_conc","male_conc","rel_dur")
if(T){
load("abc_out_original_small.RData")
par(mfrow=c(4,2),mar=c(2,2,1,1),oma=c(2,2,3,1))
for(ii in 1:7){
  d=density(abc_out_original_small$param[,ii],weights=abc_out_original_small$weights)
  plot(d,type="l",col="blue",lwd=2,xlab="Posterior Distribution",main="",cex.lab=1.5)
  rr=seq(as.numeric(priors[[ii]][2]),as.numeric(priors[[ii]][3]),length=100)
  rrl=rr[length(rr)]-rr[1]
  x1=c(rr[1],rr[length(rr)],rr[length(rr)],rr[1])
  y1=c(0,0,1/rrl,1/rrl)
  polygon(x1,y1,border="grey",lwd=5)
  title(vars[ii],line=.3)  
}
mtext("Posterior density (unnormalized) and prior density",outer=T,side=3,line=1)
}
 