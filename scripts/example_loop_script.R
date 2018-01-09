# Override default values
evoparams$initial_pop      = 200
evoparams$initial_infected = 40
evoparams$n_steps = 365*20
evoparams$nsims=4 # 4 simulations (replicates) for each target stat value

#Use default modules
modules <- c(
  "aging",
  "testing",
  "treatment",
  "viral_update",
  "coital_acts",
  "transmission",
  "deaths",
  "births",
  "summary_module")

#Three values of target stats, corresponding to mean degrees of 0.7, 1.0 and 2.0.
target_stats_values <- c(0.35,0.7,1.0)*evoparams$initial_pop
#Create list to hold output of three model runs
output_list <- vector('list',length=length(target_stats_values))

#Run model for each value of specified target stats
for(ii in 1:length(target_stats_values)){
  evoparams$target_stats <- target_stats_values[ii] 
  nw <- nw_setup(evoparams)
  output_list[[ii]] <- evorun(modules,evoparams,nw)
}  



# Plot and compare prevalence time series for each target stat value (prevalence increases with increasing target stat # / mean degree values)
par(mfrow=c(2,2))
evoplot(output_list[[1]],variables = "prevalence")
evoplot(output_list[[2]],variables = "prevalence")
evoplot(output_list[[3]],variables = "prevalence")


#Plot all output for first model
evoplot(output_list[[1]])
