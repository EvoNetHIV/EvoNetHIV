Using Loops with EvoNet
================

Using Loops for EvoNet
----------------------

Likely, the effects of a range of parameter values on epidemic dynamics will be of interest. This will require using a loop structure. A simple example is given below where three values for the parameter “target\_stats” (which determines the number of relationships between agents).

Here is standard EvoNet model setup

``` r
#Read in default parameters (as list)
evoparams <- evonet_setup()    


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
```

A vector called "target\_stats\_values" is created that will hold the different number of "target\_stats" values that will be used. Multiplying 0.35, 0.7, and 1.0 by the initial population size will result in mean degrees of 0.7, 1.4, and 2.0 for the initial network structure (and will be expected to remain more or less at those values for the entire model run in this example).

``` r
#Three values of target stats, corresponding to mean degrees of 0.7, 1.0 and 2.0.
target_stats_values <- c(0.35,0.7,1.0)*evoparams$initial_pop
```

An empty list object of length 3 is created that will hold the output for each model.

``` r
#Create list to hold output of three model runs
output_list <- vector('list',length=length(target_stats_values))
```

A loop structure is used to assign the desired target\_stats value, create a network with the resulting target\_stats value (and all the other default network parameter values), and run the epidemic model. Note: Since "target\_stats" affects network structure, a new network has to be created for each new value of "target\_stats"" used. Note: If a parameter does not directly influence network structure, e.g., condom usage (parameter "condom\_prob"), then a new network would not have to be estimated for each value of "condom\_prob" used.

``` r
#Run model for each value of specified target stats
for(ii in 1:length(target_stats_values)){
  evoparams$target_stats <- target_stats_values[ii] 
  nw <- nw_setup(evoparams)
  output_list[[ii]] <- evorun(modules,evoparams,nw)
}  
```

Plot and compare prevalence time series for each target stat value (prevalence increases with increasing target stat \# / mean degree values)

``` r
par(mfrow=c(2,2))
evoplot(output_list[[1]],variables = "prevalence")
evoplot(output_list[[2]],variables = "prevalence")
evoplot(output_list[[3]],variables = "prevalence")
```

![](https://github.com/EvoNetHIV/EvoNetHIV/blob/master/documentation/imgs/loop_ex_fig.png)
