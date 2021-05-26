EvoNet Vaccination Models
================

### Vaccine modeling in EvoNet

EvoNet (<https://github.com/EvoNetHIV/EvoNetHIV>) is an agent-based
network epidemic model based on the EpiModel package (see
<https://github.com/statnet/EpiModel> and <http://www.epimodel.org/>)
where attributes (e.g., sex, age, viral load, number of partners, MSM
sexual role, etc.) of the agents in the population determine the
epidemic dynamics in addition to the specified sexual network structure
(e.g., more highly connected sexual networks result in more severe
epidemics).

In addition to the core agents attributes expected in an agent-based
epidemic model (age, sex, infection status, viral load, testing and
treatment status, etc.), vaccine dynamics are specified by the
attributes *phi*, *mu*, *sigma*, and *m*. Flexible list structures are
used to hold the parameterizations for these attributes and thus, the
parameterizations can be of any arbitrary structure and complexity. In
this description these attributes have very simple parameterizations
consisting of one or two numeric values but much more complicated
parameterizations are possible.

#### Parameterizations of phi, mu, sigma, and m for simple models

*Phi* can have values of NA (agent never vaccinated), \>0 (currently
vaccinated with 1 representing full vaccination), 2 (vaccinated with
placebo), or 0 (formerly vaccinated). The value of *mu* specifies the
mean vaccine efficacy (proportion reduction in vaccine and varies from
0.0-1.0) for a given mark of an infected agent. *Sigma* indicates the
variability (standard deviation) of the mu value; in simple models,
*sigma* values are set to zero and there is no variability in *mu*
(i.e., *mu* represents a constant value rather than a mean of a
distribution). The attribute *m* represents the value of the mark drawn
from a distribution described by the mean value *mu* and *sigma*, the
standard deviation of *mu*; the value of this draw, *m*, is then used to
represent the vaccine efficacy for a given sex act between an infected
agent and a vaccinated, uninfected agent. In simple models, *sigma* is
set to zero and *m* thus is equal to *mu* in when *sigma* is set to
zero.

#### Vaccine Trial in EvoNet

##### Relevant parameters

A vaccine trial can be simulated in evonet where at a specified timestep
agents become vaccinated and either receive a vaccine or placebo. The
vaccine has a user-specified vaccine efficacy and different viral
strains can have different vaccine efficacies. To run a vaccine trial
the key vaccine-specific parameters are

  - *vaccine\_model*: Value should be set to TRUE to designate that
    vaccine functions will be used  
  - *vaccine\_trial*: Value should be set to TRUE to designate that in
    addition to a vaccine model, a trial will be simulated  
  - *fraction.vaccinated*: Specifies what fraction of the total
    population will be vaccinated (and in the vaccine trial; ideally,
    should be relatively low fraction around 0.10 to 0.25)  
  - *vaccine.rollout.year*: Specifies the beginning of model year
    vaccination of agents begins  
  - *vaccine.rollout.duration.years*: Specifies over how many years
    should the vaccinations occur to reach the target specified with
    parameter `fraction.vaccinated`  
  - *vacc\_type*: Specifies how quickly the vaccine takes effect. If the
    default value of “standard” then vaccine have maximum efficacy
    immediately (i.e, phi = 1). If “linear”, then the vaccine will take
    a specified duration to reach maximum efficacy(phi=1) with a
    constant daily increase in efficacy.  
  - *vacc\_phi\_daily\_increase*: When vacc\_type=“linear”, this sets
    the rate of daily increase of phi from near 0 (.0001, the default
    value of parameter *initial\_phi\_value*) to 1. If maximum vaccine
    efficacy should be reached in 7 months after vaccination for an
    agent, then vacc\_phi\_daily\_increase would be set to 1/210.  
  - *vacc\_min\_efficacy\_duration*: Specifies the minimum time period
    in days after vaccination before the vaccine effects begins to wane.
    If there is no ramp-up of vaccine efficacy (vacc\_type=“standard”),
    then this period is simply the period after vaccination. If
    vacc\_type=“linear”, the the duration of ramp-up needs to be
    considered; if the vaccine should last one year before waning after
    reaching maximum efficacy and the ramp up takes 7 months,
    vacc\_min\_efficacy\_duration should be set to 6935 ((12+7)\*365).  
  - *vaccine\_waning\_type*: Specifies how the vaccine effects wanes
    after the period specified by vacc\_min\_efficacy\_duration ends.
    “daily\_prob” specifies a daily binomial probability of the
    vaccine effect ending; “cliff\_edge” specifies the vaccine effects
    wanes all at once when the minimum efficacy duration is reached; and
    “exponential” specifies an exponential decay.  
  - *daily.vaccine.reversion.rate*: When
    vaccine\_waning\_type=“daily\_prob”, this parameter specifies the
    the daily probability of the vaccine effect ending (e..g, 1/365); a
    very, very small value indicates essentially no waning of the
    vaccine effect (e.g., 1e-6).  
  - *vacc\_exp\_decline\_rate*: When
    vaccine\_waning\_type=“exponential”, this number (e.g.,
    -0.0004), specifies the daily decay rate in vaccine efficacy.  
  - *revaccination.eligibility.years*: Specifies after how many years a
    vaccinated agent is eligible for another vaccination.  
  - *prop\_vaccinated\_placebo*: Specifies what proportion of the
    vaccinated population will receive a placebo if a vaccine trial is
    simulated.  
  - *initial\_trial\_participants*: Though the vaccine trial typically
    does not begin immediately in a trial model (`vaccine_trial` =
    TRUE), to allow a “burn-in” period, a small number of agents at the
    model start are required to be specified as being in the trial to
    ensure proper network estimation. When the vaccine trial starts
    (parameter vaccine.rollout.year), trial participants are not allowed
    to pair-up with each other; to implement this network constraint
    (and skipping network estimation details), a small number of agents
    are required to be specified as in a trial (about 3 agents per 1000
    agents of the initial population size seems adequate).

The virus of an infected agent is described by its “mark”; mark can be
defined as the genotype-specific vaccine efficacy. While the model
allows for virus marks to be represented in a highly flexible and
potentially complex manner, here we present simple examples where the
virus is described by only one or two discrete marks and the values for
the marks are time-invariant. The two parameters to specify the virus
marks for these simple examples are *vaccine.efficacy.by.mark* and
*initial.mark.distribution*. These parameters are described with list
structures and the length of the lists (the number of elements in each
list) implicitly denote the number of marks specified. Both parameters
should have lists of equal length. The parameter
*initial.mark.distribution* specifies the proportion of the initial
infected population that has the mark with the specified efficacy.
Agents with secondary infections then receive the corresponding marks
and efficacies from the infector.

###### One mark with single efficacy example

``` r
vaccine.efficacy.by.mark <- list( mark1 = c("strain1" = 0.7 ))
initial.mark.distribution <- list( mark1 = c("strain1" = 1 ))  
```

This parameterization specifies that all infected agents will have a
virus described by a single mark with a vaccine efficacy of 0.7
(transmission probability is reduced by
70%).

###### One mark with two efficacies per mark example

``` r
vaccine.efficacy.by.mark <- list( mark1 = c( "strain1" = 1.0,"strain2"=0.0 ) )
initial.mark.distribution <- list(  mark1 =  c( "strain1" = 0.7,"strain2"=0.3 ))
```

This parameterization specifies that all infected have agents will have
virus described by a single mark with two variants with different
efficacies.

###### Two marks with two efficacies per mark example

``` r
vaccine.efficacy.by.mark <- list( mark1= c( "strain1" = 0.8,"strain2"=0.2 ), mark2= c( "strain1" = 0.11,"strain2"=0.01 ))  
initial.mark.distribution <- list( mark1=  c( "strain1" = 0.5,"strain2"=0.5 ), mark2= ( "strain1" = 0.5,"strain2"=0.5 ))
```

#### Other basic EvoNet parameters

In addition to vaccine-related parameters, there are a number of other
important parameters required for model setup and execution.  
\- *initial\_pop*: Specifies the initial size of the population  
\- *initial\_infected*: Specifies how many agents in the initial
population are infected  
\- *n\_steps*: Specifies how many daily time-steps the model will run  
\- *popsumm\_frequency*: Specifies how frequently (number of timesteps)
summary statistics (e.g., prevalence) are calculated. It is a minor
parameter but can impact how fast the model runs especially for large
population sizes; typically a value of 30 is used (i.e., monthly
values).

#### EpiModel and network parameters

Evonet is based on the EpiModel package( see
<https://github.com/statnet/EpiModel> and <http://www.epimodel.org/>)
which allows the user to specify a network epidemic model with highly
variable network structure and connectivity. Critical parameters that
feed into EpiModel’s network simulation include  
\- *target\_stats*: Specifies how connected the network is  
\- *nw\_form\_terms*: Nnetwork formation terms; specifies how the
network is structured  
\- *nw\_coef\_form*: Network coefficients for formation terms -
*relation\_dur*: Specifies how long the average relationship between two
agents, in days

#### Risk groups based on sexual network

To define risk groups based on number of sexual contacts, discrete age
categories can be defined and these age categories can have differential
mean degree values. The age groups can be defined with the parameter
age\_nw\_groups.

``` r
age_nw_groups <- list( group1= c(17.0,30),group2=c(30,55)
```

This parameterization specifies two age groups. The first age group
contains all agents \>17 years in age and 30 years in age and the second
gorup contains all agents \>30 and 55 years (This assumes that the ages
of the agents vary from 17 to 55, which can be set with the parameters
min\_age and max\_age). For a heterosexual network with two age-based
risk groups, an example evonet parameterization for the network
parameters would be:

``` r
age_nw_groups <- list( c(17.0,30),c(30,55)) # (age1,age2] 
nw_form_terms <-  "~edges + nodefactor('att1')+ offset(nodematch('sex', diff=FALSE))"
target_stats <- c(INITIAL.POP*0.4,INITIAL.POP*0.4) 
nw_coef_form  <- -Inf
```

where INITIAL.POP is the number of agents at the start of the model. The
age category data (in this example the age categories would be 1 or 2)
are stored in the “att1” attribute (a generic attribute that can be used
for a variety of purposes) amd the nodefactor term specifies that mean
degree will differ by att1 values. The value of target\_stats as
specified above will result in the younger age category having roughly
60% higher mean degree than the older group (e.g., 1.05 vs. 0.65) In
addition to the EpiModel references already mentioned, for further
information on network estimation with the nodefactor term see the
section “Network model estimation and diagnostics” in [EpiModel: An R
Package for Mathematical Modeling of Infectious Disease over
Networks](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5931789/) by
Jenness et al.
(2018).

#### Alternative risk group formulation that assumes that the high-risk group has shorter relationship durations

The risk group model in the section above assumes that the high-risk
group has a higher mean degree (more contacts per person at any given
time). An alternative method is to assume that people in the high risk
group have faster relationship turnover. The code below implements a
model in which there are two risk groups, one with short relationship
durations, one with long relationship durations. In this model, most
individuals enter the population with a tendency to have short
relationship durations. As they age, they have a small per day
probability of aging into the long duration group. This code also
assumes that people tend to form relationships with people who are about
the same age, and that men tend to partner with younger women.

``` r
md= 0.75 # mean degree
male_female_age_diff = 4 # 4 means that women partner with men who are, on average, 4 years older than them
abs_diff_age = 4   # Difference in age AFTER accounting for the male-female age difference

evoparams$generic_nodal_att_no_categories = 2 # Two groups
evoparams$generic_nodal_att_values        = 1:2  # 1 = short-term, 2 = long-term relationship
D1 = 2*365     # Duration preference for people in group 1-1 pairs (short-term relationships)
D2 = 10*365    # Duration preference for people in group 2-2 pairs (long-term relationships)
f1_0 = 0.9         # Proportion of newly entering agents (usually at age 16) that have a preference/tendency for short-term relationships
f2_0 = 0.1         # Proportion of newly entering agents (usually at age 16) that have a preference/tendency for long-term relationships
p_long <- 0.00011  # Per day probability of an switching from short-to long-term preference (0.00011 amounts to ~4% probability/year)
f1 = 0.40          # Initial percentage of agents with a tendency to form short-term relationships.  This value was determined empirically!
f2 = 1 - f1        # Initial percentage of agents with a tendency to form long-term relationships

evoparams$generic_nodal_att_values_props  = c(f1, f2) # Proportion of agents belong to groups 1 and 2 at time 0
evoparams$generic_nodal_att_values_props_births = c(f1_0, f2_0) # Probabilty of newly introduced agents will belong to groups 1 and 2
evoparams$generic_nodal_att_trans_mat   <- matrix(
  c( 1- p_long, p_long,
     0,          1),
  nrow=2,byrow=T,dimnames=list(c("G1","G2"))
)

evoparams$nw_form_terms <- "~edges + nodemix('att1', base=1) + absdiffby('age','sex',4)  +offset(nodematch('sex', diff=FALSE))"
tot_edges = md * evoparams$initial_pop / 2   # Would add up to 150 for n=1000 and md=0.35
abs_diff_age_param <- tot_edges * abs_diff_age
```

Matrix for determining edge counts (1-1 edges assumed from total), followed by age-homophily term

``` r
evoparams$target_stats <- c(tot_edges, 2*tot_edges*f1*f2, tot_edges*f2*f2,   abs_diff_age_param)  # Implements idea above.  
evoparams$nw_coef_form <- -Inf
evoparams$dissolution <- "~offset(edges)"
```

Matrix of durations assuming a geometric mean for 1-2 pairs

``` r
evoparams$relation_dur=c(D1, sqrt(D1*D2), D2)
```

#### Vaccine-related functions

Vaccine-related functionality in two modules (a group of functions
related to a specific model process): the *vaccine\_dynamics* module
([github
link](https://github.com/EvoNetHIV/EvoNetHIV/blob/master/pkg/R/vaccine_dynamics.R))
and the [*transmission
module*](https://github.com/EvoNetHIV/EvoNetHIV/blob/master/pkg/R/transmission.R).
The vaccine dynamics module initializes and updates values of the agent
attributes *phi*, *mu*, and *sigma*. The *transmission* module contains
the function
[*transmission\_vaccine*](https://github.com/EvoNetHIV/EvoNetHIV/blob/master/pkg/R/transmission_vaccine.R)
which adjusts the HIV transmission probabilities of sex acts between
discordonant couples due to vaccine effects. Vaccine-related functions
are defined in the file
[vaccination\_model\_functions](https://github.com/EvoNetHIV/EvoNetHIV/blob/master/pkg/R/vaccination_model_functions.R).

#### Other basic model processes

In addition to vaccine related dynamics, typical individual and
population level processes are modeled: aging, testing, treatment
(unless specified by the parameter “start\_treatment\_campaign,
treatment does not occur automatically), deaths (natural or HIV/AIDS
related), aging out of model (model typically tracks agents from ages
18-55 and aging out is included in the death module), relationship
formation and daily sex acts determination, and risk of infection based
on relevant covariates (age, sex of infected/susceptible agent, viral
load, condom use, etc.). For vaccine-related research, these functions
will likely often be of little interest.

#### MSM vs Heterosexual models

The default model is a MSM model though heterosexual models are also
easily specified. A feature of MSM models is that agents are specified
sexual roles (versatile, insertive, receptive) which influences network
structure (e.g., insertive agents can not form relationships with other
insertive agents). In heterosexual models, agents can only pair up with
opposite-sex partners.

#### Model execution

The order of steps to run the model is i) specify relevant model
parameters; ii) estimate initial network; iii) run model with given
parameters and initial network; and iv) plot results and perform
post-model processing.

#### Output

The evoplot function produces over 30 diagnostic plots related to
epidemic and network dynamics (prevalence, number of new infections,
mean SPVL per incident, mean degree of network, etc.). They are used to
evaluate model performance and are not meant for publication quality
figures. These plots are both printed to the screen and printed to a pdf
(saved in working director) with the evoplot function. Publication and
presentation quality figures can be created using output from the “epi”
output list. The “epi” list contains timeseries of epidemic and network
related statistics.

#### Example run script for vaccine model

``` r
library(evonet)

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
    plot_nw=F,
    min_age = 15, #youngest agent age
    max_age = 55, #oldest agent age
    vaccine_age_range = c(15, 18), #age range that can be vaccinated
    model_sex = "hetero", #heterosexual model (vs "msm" model)
    initial_agedata_male   = "south_africa_male_15_to_100_2014",
    initial_agedata_female = "south_africa_female_15_to_100_2014",
    asmr_data_male        =   "south_africa_male_1990",
    asmr_data_female       = "south_africa_female_1990", # vaccine model parameters
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
    # has about 60% higher mean degreee
    age_nw_groups <- list( c(17.0,30),c(30,55)) # (age1,age2] 
    nw_form_terms <-  "~edges + nodefactor('att1')+ offset(nodematch('sex', diff=FALSE))"
    target_stats <- c(INITIAL.POP*0.4,INITIAL.POP*0.4) 
    nw_coef_form  <- -Inf)

#calculate number of marks based on length of parameter vaccine.efficacy.by.mark
param_list$no_marks <- length(param_list$vaccine.efficacy.by.mark )

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

#-- run model
evomodel <- evorun(modules,evoparams,nw)
save(evomodel,file="aon_trial.RData")

# -- plot results
evoplot(model=evomodel,name="aon_trial")
```
