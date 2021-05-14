Parameter sampling with EvoNet Vaccination Models
================

In addition to having fixed parameter values for a given model, Evonet
models can be run where user-specified parameter values are drawn from a
user-specified distribution rather than having a fixed value across
simulation runs. While EpiModel, the package that EvoNet is based-on,
has recently added similar functionality, a slightly different framework
was developed for Evonet to allow for network parameters (e.g.,
relationship duration) to be drawn from a distribution in addition to
non-network parameters. In short, this framework allows for many
different models to be run (as many as the user wants, though time is
usually a limiting factor) that have different values for a givnen
parameter or set of parameters, and then those different models can be
combined to give a single output.

A motivating example is the use of approximate Bayesian computation
(ABC) to estimate model parameters. Skipping any detailed discussion of
the topic, ABC is a simulation-based statistical method that produces
posterior densities (probability distributions) of the parameters of
interest. An example of estimating Evonet parameters with ABC, using the
package “EasyABC”, is at
<https://github.com/EvoNetHIV/EvoNetHIV/blob/master/scripts/Example_abc_estimation.R>.
This example models the South African HIV epidemic from the 1990 to 2014
and is fit to age-specific prevalence data. (This was based on Evonet
work done by Dr. Kathryn Peebles). While 7 parameters were estimated, we
just focus on the results for one of them: relationship duration (the
mean relationship duration in days).

The below figure shows the prior distribution and the posterior
distribution for the relationship duration parameter in the bottom row.

![](https://github.com/EvoNetHIV/EvoNetHIV/blob/master/documentation/imgs/abc_example_output.png)

To incorporate the estimated variability in the relationship duration
parameter for other model simulations requires sampling from the
estimated posterior density for this parameter. To accomplish this the
posterior density was saved to file as a two column matrix with with
first column representing parameter values and the second column
representing corresponding density values. (This was accomplished using
the density() function; see the bottom of
<https://github.com/EvoNetHIV/EvoNetHIV/blob/master/scripts/Example_abc_estimation_output.R>).
(While this example uses output from an ABC analysis, a simpler approach
that samples from a uniform distribution is also shown as an example.)

To incorporate the variability in the relationship duration parameter as
estimated by the ABC run, an evonet model is run that samples from the
discretized posterior density using weighted probabilities. Since many
samples are required to adequately characterize the estimated
variability in the relationship duration parameter, ideally many model
runs, each with a sampled value for relationship duration, will be
executed. Parallelization of the model runs will greatly decrease the
required time for such sampling, and the example run script below
assumes use of a multi-core processor.

The example script below essentially converts the run script used in
<https://github.com/EvoNetHIV/EvoNetHIV/blob/master/documentation/vaccine_overview_RMD.md>
and converts it into a function. This function can then be run on
multiple cores at the same time. Each model run the value for the
parameter “relationship\_dur” is drawn from from the posterior density
estimated by an ABC analysis (). The output from the multiple model runs
are all stored in the same user-specified directory. The output files
can then be stitched together to create a single evonet output object
that can be analyzed. An important parameter is the “raw\_output”
parameter; by setting this to TRUE allows output of different models to
be stitched together into a single object.

``` r
library(evonet)
library(doParallel)
library(parallel)
library(foreach)

#note: the two output directories (one for numerical output and one for figures) need to be
#     created before model run. 

evo_parallel<- function(outpath=file.path(getwd(),"out") ,
                        outpath_plots=file.path(getwd(),"out_plots")){
  
  if(is.null(outpath)){outpath=getwd()}else{
    if(!dir.exists(outpath)){
      stop("outpath does not exist, please check")
    }
  }
  if(is.null(outpath_plots)){outpath_plots=getwd()}else{
    if(!dir.exists(outpath_plots)){
      stop("outpath_plots does not exist, please check")
    }
  }
  
  
  require(evonet)
  
  #note: for debugging purposes, an evonet model can be run
  #just by executing code within this if statement
  if(T){  
    #Core model parameters
    INITIAL.POP <- 10000
    MODEL.YEARS <- 7
    INITIAL.INFECTED <- 250
    
    #A list that specifies changes of default parameters to user-specified values
    # Default model parameters are at https://github.com/EvoNetHIV/EvoNetHIV/blob/master/pkg/R/input_params.R
    
    param_list <-  list(
      # basic model parameters
      initial_pop = INITIAL.POP,
      initial_infected = INITIAL.INFECTED,
      n_steps = MODEL.YEARS*365,
      popsumm_frequency=30,
      nsims = 1,
      ncores = 1,
      raw_output = TRUE,
      plot_nw=F,
      min_age = 15, #youngest agent age
      max_age = 55, #oldest agent age
      model_sex = "hetero", #heterosexual model (vs "msm" model)
      initial_agedata_male   ="south_africa_male_15_to_100_1990", #age distribution data
      initial_agedata_female = "south_africa_female_15_to_100_1990", #age distribution data
      asmr_data_male         <- "south_africa_male_1990", #age-specific mortality rate
      asmr_data_female       <- "south_africa_female_1990",#age-specific mortality rate
      # vaccine model parameters
      vaccine_model = T,
      vacc_type="linear", #vacc. eff (phi) increases from initial value to 1 based on "vacc_phi_daily_increase"
      initial_phi_value= .0001,
      vacc_phi_daily_increase = 1/(7*30), #daily phi value increment,
      #pi goes from 0 to one in 7 months
      vacc_min_efficacy_duration = 1*365, #minimum time vaccine effective after 1st dose,
      # can start waning after this period
      vaccine_waning_type = "cliff-edge", #or "daily_prob" or "exponential"
      vacc_exp_decline_rate = -0.004,#about 5 year expoential decline from 1 to near 0
      #exp(rate*(1:(5*365)))
      fraction.vaccinated = 0.7,
      vaccine.rollout.year = 5, #timestep = value x 365
      vaccine.rollout.duration.years = 0, #value of "0", means all at once, values 
      # >1 mean constant daily vacc. rate during that time tp reach
      #fraction.vaccinated value
      vaccine.efficacy.by.mark = list( mark1 = c( "strain1" = 0.7 )), 
      initial.mark.distribution = list( mark1 = c( "strain1" = 1 )), 
      # network parameters
      #parameterization for heterosexual model with two age-based risk groups, younger group
      # has abou 60% higher mean degreee
      age_nw_groups <- list( c(17.0,30),c(30,55)) # (age1,age2] 
      nw_form_terms <-  "~edges + nodefactor('att1')+ offset(nodematch('sex', diff=FALSE))"
      target_stats <- c(INITIAL.POP*0.4,INITIAL.POP*0.4) 
      nw_coef_form  <- -Inf)
    
    #calculate number of marks based on length of parameter vaccine.efficacy.by.mark
    param_list$no_marks <- length(param_list$vaccine.efficacy.by.mark )
    
    #-----------------------
    #Parameter sampling code
    
    #draw a value for relaionship duration based on posterior density of ABC model
    source("https://github.com/EvoNetHIV/EvoNetHIV/blob/master/scripts/Example_abc_output_relationship_duration?raw=TRUE")
    #this reads in the 2 column matrix "relationship_duration_post", where the first column is a parameter value
    #and the second value is the density value; 
    param_list$relation_dur <- sample(x=relationship_duration_post[,1],size=1,prob=relationship_duration_post[,2])
    
    #An example of using uniform distribution to sample from would be
    #param_list$relation_dur <- runif(1,100,1000)
    #--------------------------------
    
    
    #---- create EpiModel parameter list with updated values
    evoparams <- do.call(evonet_setup,param_list)
    
    #---- network setup 
    nw <- nw_setup(evoparams)
    
    #---  Create list of modules (core processes) to run for input into epimodel_control_fxn() below
    modules <- c(
      "vaccine_dynamics",
      "aging",
      "testing",
      "treatment",
      "viral_update",
      "coital_acts",
      "transmission",
      "evo_departures",
      "evo_arrivals",
      "summary_module")
    
    #--------------------------------------------------------------
    #run model
    evomodel <- evorun(modules,evoparams,nw)
  }
  
  #create model id based on how many files already in output folder then save model
  #using model id
  model_id <- length(list.files(outpath))+1
  outfile= paste(outpath,"/evomodel",model_id,".RData",sep="")
  save(evomodel,file=outfile)
  #--------------------------------------------------------------
  
  #if combining multiple different outputs from different simulations,
  #easier to use EpiModel's "raw_output" option, but can't use evonet's plotting functions
  #with this type of output (but these plotting functions can be used when the output is combinded, 
  # see below)
  if(evoparams$raw_output!=T){
    outfile_plots= paste(outpath_plots,"/evomodel",model_id ,".RData",sep="")
    evoplot(model=evomodel,name=paste("evomodel",model_id,sep=""),outpath=outpath_plots)
  }
  
}

#--------------------------------------------------------------

#run a parallel job


path1 <- "~/testout"
dir.exists(path1)
path2 <-"~/testout_plots"
dir.exists(path2)

#how many models to run?
number_sims <- 10
doParallel::registerDoParallel(number_sims)

time_parallel <- system.time({
  times(number_sims) %dopar% {
    evo_parallel(outpath=path1, outpath_plots=path2)
  }
})
stopImplicitCluster()

#-----------------------------
#post processing steps to combine different models into one final
# evonet object

outfiles <- list.files(path1,full.names=T)
outlist <- vector('list',length=length(outfiles))
for(ii in 1:length(outfiles)){
  load(outfiles[ii])
  outlist[[ii]] <- evomodel
  remove(evomodel)
}

datout <- lapply(1:length(outlist),function(x) outlist[[x]][[1]])

#final evonet object
evofinal <- EpiModel:::process_out.net(datout)
#------------------------------------

#plotting different model runs
evoplot(evofinal)
```
