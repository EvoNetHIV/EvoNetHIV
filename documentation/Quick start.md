# Quick start guide to running EvoNetHIV 

Download/install/load EvoNetHIV
```{r chunk1, eval=FALSE}
if (!require("devtools")) install.packages("devtools")
install_github("EvoNetHIV/TestRepo",subdir="pkg")
library(evonet)
```

Load default parameters.
```{r chunk2, eval=F}
primary_parameters  <- input_parameters_primary()
cd4_data            <- input_parameters_cd4_data()
```

Combine individual parameters into single list. Parameter list can be viewed by entering 'evoparams' in RStudio console.
```{r chunk3, eval=FALSE}
evoparams <- c(primary_parameters, cd4_data)
```
Change default parameters. In this example, we change initial population size to 200 (from 100); number of initially infected agents to 40 (from 20); and the duration of the model to 20 years.
```{r chunk4, eval=FALSE}
evoparams$initial_pop      = 200
evoparams$initial_infected = 40
evoparams$n_steps          = 365*20
```

Calculate derived parameters (parameters that are functions of other parameters)
```{r chunk5, eval=FALSE}
evoparams  <- input_parameters_derived(evoparams)
```
Convert initial parameter list into EpiModel parameter list (so EpiModel recognizes it as a parameter list)
```{r chunk6, eval=FALSE}
evoparams <- do.call(EpiModel::param.net,evoparams)
```
Check to make sure input parameters are valid (error returned if not)
```{r chunk7, eval=FALSE}
input_parameters_qaqc(evoparams)
```
Create initial network (as a function of input parameters)
```{r chunk8, eval=FALSE}
nw <- setup_initialize_network(evoparams)
```
Create list of arguments for EpiModel's network estimation function.
```{r chunk9, eval=FALSE}
netest_arg_list <- list(
  nw            =  nw,
  formation     =  as.formula(evoparams$nw_form_terms),
  target.stats  =  evoparams$target_stats,
  coef.form     =  evoparams$nw_coef_form,
  constraints   =  as.formula(evoparams$nw_constraints),
  verbose       =  FALSE,
  coef.diss     =  dissolution_coefs( dissolution =  as.formula(evoparams$dissolution),
                                      duration    =  evoparams$relation_dur,
                                      d.rate      =  3e-05) )
```
Estimate network (i.e., create desired network structure and dynamics)
```{r chunk10, eval=FALSE}
estimated_nw <- do.call(EpiModel::netest, netest_arg_list)
```
Create vector of infection status (0/1) as an epimodel object for initial population
```{r chunk11, eval=FALSE}
infected_list <- EpiModel::init.net(i.num=evoparams$initial_infected,
                                    status.rand = FALSE)
```
Create list with modules/functions to simulate epidemic with desired dynamics.
```{r chunk12, eval=FALSE}
evo_module_list<- list(
  "initialize.FUN"     = initialize_module,
  "plot_nw.FUN"        = plot_network_fxn,  
  "aging.FUN"          = vital_aging_module,
  "testing.FUN"        = social_testing_diagnosis_module,
  "treatment.FUN"      = social_treatment_module,
  "update_vl.FUN"      = viral_update_gamma,
  "update_cd4.FUN"     = viral_update_cd4_daily, 
  "coital_acts.FUN"    = social_coital_acts_module,
  "trans.FUN"          = transmission_main_module,
  "trans_book.FUN"     = transmission_bookkeeping_module,
  "trans_cd4.FUN"      = transmission_cd4_module,
  "deaths.FUN"         = vital_deaths_module,
  "births.FUN"         = vital_births_module,
  "summary.FUN"        = summary_module,
  "resim_nets.FUN"     = EpiModel::resim_nets,
  "verbose.FUN"        = NULL)
```


Create an EpiModel "control" object which contains both the input parameters and the modules
```{r chunk13, eval=FALSE}
evocontrol <- setup_epimodel_control_object(evonet_params = evoparams,
                                            module_list   = evo_module_list)
```
Run the simulation using EpiModel's 'netsim' function

```{r chunk14, eval=FALSE}
  evomodel  <- EpiModel::netsim(x = estimated_nw,
                                param = evoparams,
                                init = infected_list,
                                control = evocontrol)
```

Initial network plot will be plotted by default at simulation start (colored circles represent infected agents). (Day 2 is first day of simulation as model initialization is considered day 1.)
![](https://github.com/EvoNetHIV/EvoNetHIV-Overview/blob/master/img/example_initial_network.png)

Save model output. Default location is current working directory.
```{r chunk15, eval=FALSE}
save(evomodel,
     file = file.path(evoparams$output_path,"evomodel.RData"))
```
Create default output plots summarizing model run. Plots printed to screen and saved as pdf file (default name: "popsumm_figures.pdf") to working directory.
```{r chunk16, eval=FALSE}
plots_popsumm(evomodel,outpath=evoparams$output_path,
              name=NULL,nw_stats=TRUE,max_points_rep=100,
              evoparams$popsumm_frequency)
```

Example of two default output plots
![](https://github.com/EvoNetHIV/EvoNetHIV-Overview/blob/master/img/example_output_plots1.png)


