Concurrency example with EvoNet
================

Simple example with concurrency
-------------------------------

Blurb about concurrency and its implemenatation here.

Read in parameters, change default values for nsims, n\_steps, popsumm\_frequency, initial\_pop, initial\_infected. Use default modules.

``` r
#Read in default parameters (as list)
evoparams <- evonet_setup(
nsims=3,
popsumm_frequency =30,
 n_steps           = 365*30,
 initial_pop       = 300,
 initial_infected  = 30,
 nw_form_terms = "~edges + concurrent + offset(nodematch('role', diff=TRUE, keep=1:2))")

#Use default modules
modules <- c(
  "aging",
  "testing",
  "treatment",
  "viral_update",
  "coital_acts",
  "transmission",
  "evo_arrivals",
  "evo_departures",
  "summary_module")
```

Create two variable vectors, each of length 2: concurrency model needs two target stats values: i) total edges and ii) total edges in concurrent relationship.

``` r
target_stats_monogamy <- c(0.35*evoparams$initial_pop, 0) #complete monomagy
target_stats_concurrency <- c(0.35*evoparams$initial_pop, 0.25*evoparams$initial_pop) #25% relationships
target_stats_list=list(target_stats_monogamy,target_stats_concurrency)
```

Create list to hold output of three model runs

``` r
output_list <- vector('list',length=length(target_stats_list))
```

Run model for each value of specified target stats

``` r
for(ii in 1:length(target_stats_list)){
  evoparams$target_stats <- target_stats_list[[ii]]
  nw <- nw_setup(evoparams)
  output_list[[ii]] <- evorun(modules,evoparams,nw)
}  
```

save parameters and output to current working directory for future work if needed (e.g., getwd() )

``` r
save(evoparams,output_list,file="concurrencny_Sim.RData")
```

Plot and compare prevalence time series for each target stat value (prevalence increases with increasing target stat \# / mean degree values), line at 0.25 for reference

``` r
par(mfrow=c(2,2))
evoplot(output_list[[1]],variables = "prevalence",main="prevalence: monogamy")
abline(h=0.25)
evoplot(output_list[[2]],variables = "prevalence",main="prevalence: concurrency")
abline(h=0.25)
```
![](https://github.com/EvoNetHIV/EvoNetHIV/blob/master/documentation/imgs/conc_example.png)
