# Quick start guide to running EvoNetHIV 

Download and install package and dependencies (other necessary packages). Easiest to do with a newly opened instance of RStudio or R.

``` r
if (!require("devtools")) {
install.packages("devtools")
library(devtools)}
install_github("EvoNetHIV/EvoNet",subdir="pkg")
devtools::install_github( "statnet/tergmLite",force=T)
devtools::install_github( "statnet/EpiModel", ref ="fast_edgelist")
library(evonet)
```

Load default parameters.

``` r
evoparams <- evonet_setup() 
```

Change default parameters. In this example, we change initial population size to 200 (from 100); number of initially infected agents to 40 (from 20); and the duration of the model to 20 years.

``` r
evoparams$initial_pop      = 200
evoparams$initial_infected = 40
evoparams$n_steps          = 365*20
```

Create initial network (as a function of input parameters)

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
"deaths",
"births",
"summary_module") 
```

Run the simulation

``` r
evomodel <- evorun(modules,evoparams,nw)
```

Initial network plot will be plotted by default at simulation start (colored circles represent infected agents). (Day 2 is first day of simulation as model initialization is considered day 1.) ![](https://github.com/EvoNetHIV/EvoNetHIV-Overview/blob/master/img/example_initial_network.png)

Save model output. Default location is current working directory (use getwd() to identify).

``` r
save(evomodel, file ="evomodel.RData"))
```

Plot summary figures (automatically to screen and saved as pdf)

``` r
evoplot(model=evomodel)
```

Example of two default output plots ![](https://github.com/EvoNetHIV/EvoNetHIV-Overview/blob/master/img/example_output_plots1.png)

Approximately 30 default output plots are produced at the end of each model run. These plots are both printed to the screen and saved as a pdf file. The name of the pdf file and its location path can be specided with

``` r
evoplot(model=evomodel, names = "name.pdf", path= "\path")
```
