Treatment example with EvoNet
================

To initiate treatment requires setting two parameters: i) "start\_treatment\_campaign": When treatment of tested and diagnosed agents begins (in days) since model start.

1.  "tx\_type": This determines the criteria used for an agent to receive treatment, given they are infected and have tested positive. The default setting is NA, thus a treatment type must be specified. The possible values are "VL", "CD4", "time", "vl\_and\_cd4", "vl\_and\_time", "vl\_and\_cd4\_and\_time", which indicat refer to viral load / CD4 values or time since infection. Note: Default value is "NA", so a value must be specified.

Read in parameters, change default values.

``` r
evoparams <- evonet_setup(
nsims=3,
popsumm_frequency =30,
 n_steps           = 365*10,
 initial_pop       = 300,
 initial_infected  = 30,
 tx_type          = "VL",
start_treatment_campaign = 3*365,
fast_edgelist=TRUE)
  
#Use default modules
modules <- c( "aging", "testing","treatment",
  "viral_update",  "coital_acts", "transmission",
  "deaths",  "births", "summary_module")

#Create initial network
nw <- nw_setup(evoparams)

#Run model
evomodel <- evorun(modules,evoparams,nw)

#Plot summary variables: prevalence, mean_vl_pop_all, no_treated
par(mfrow=c(2,2))
evoplot(evomodel,variables="prevalence")
evoplot(evomodel,variables="mean_vl_pop_all",main="mean VL of infected agents")
evoplot(evomodel,variables="no_treated",main="no. agents on treatment")
```
![](https://github.com/EvoNetHIV/EvoNetHIV/blob/master/documentation/imgs/tx_example.png)
