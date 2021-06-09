# Quick start guide to running EvoNetHIV 

Download and install package and dependencies (other necessary packages). Easiest to do with a newly opened instance of RStudio or R.

``` r
if (!require("devtools")) {
install.packages("devtools")
library(devtools)}
install_github("EvoNetHIV/EvoNet",subdir="pkg")
library(evonet)
```

Load default parameters.

``` r
evoparams <- evonet_setup() 
```

Change default parameters. In this example, we change initial population size to 200 (from 100); number of initially infected agents to 40 (from 20); and the run time 5 years.

``` r
evoparams$initial_pop      = 200
evoparams$initial_infected = 40
evoparams$n_steps          = 365*5
```

Create the initial network (as a function of input parameters)

``` r
nw <- nw_setup(evoparams)
```

Specify modules/functions (as character strings) to simulate epidemic with desired dynamics.

``` r
modules <- c(
"aging",
"testing",
"treatment",
"viral_update",
"coital_acts",
"transmission",
"evo_departures",
"evo_arrivals",
"summary_module") 
```

Run the simulation

``` r
evomodel <- evorun(modules,evoparams,nw)
```

Save model output. Default location is current working directory (use getwd() to identify).

``` r
save(evomodel, file ="evomodel.RData"))
```

Plot time course data (automatically to screen and saved as pdf)

``` r
evoplot(model=evomodel, name = "evonet_plots", path= "\path")
```

Example of two default output plots ![](https://github.com/EvoNetHIV/EvoNetHIV-Overview/blob/master/img/example_output_plots1.png)
