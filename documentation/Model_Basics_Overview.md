Quick Start 2: Model Basics
================

### Agent attributes

Each agent has an associated set of variables that describe/quantify demographic, behavioral, and epidemiological conditions or states. The most important attributes are sex, age, sex role (for MSM), circumcision status (for males), infection status, treatment status, SPVL, and VL. Some attributes remain fixed for the duration of the model run (e.g., sex) while others are updated each timestep (e.g., age and VL). A full list of agent attributes and their default values are given in the Appendix.

### Sequence of Virological, Epidemiological, and Demographic Processes

#### Default modules

Model dynamics are specificed by a set of “modules” which execute a specific process: e.g., the “aging” module updates each agent’s age per timestep. Default modules are briefly described below and in more detail in later sections (names in bold). Users can write their own modules (either as modifications of existing ones or completely new).

-   **aging**: updates each agent’s age one day
-   **testing**: determines whether an agent will be tested that timestep; tested and infected agents are then assumed diagnosed as HIV+.
-   **treatment**: if a treatment campaign has started and diagnosed agents meet the specified criteria for receiving treatment, an agent will start treatment; the primary consequence of being on treatment is a decline of VL to a pre-specified undetectable level.
-   **vl\_update**: updates the VL of infected agents each timestep until death or initiation of treatment
-   **update\_cd4**: updates the the CD4 category of infected agents each timestep until death or initiation of treatment
-   **coital\_acts**: for each current relationship, determines the number of sexual acts per timestep, condom use, MSM sexual role,
-   **transmission**: for each serodiscordonant relationship with a sexual act in a given timestep, calculates the probability of infection based on risk factors of partners; for newly infected agents, associated SPVL, VL, and CD4 values are calculated.
-   **deaths**: labels agents as dead either due to AIDS and natural morality; also, removes agents that have aged-out of specified model age range (default 18-55 years old).
-   **births**: adds new agents to the model based on assumed 1% annual growth rate of initial population
-   **summary**: calculates population level summary statistics based on individual agent attrbitues (e.g., prevalence).

Reading in default parameters
-----------------------------

Evonet parameters are read in as a list using the “evonet\_setup” function. Parameters are a mixture of single numeric values, a vector or matrix of numeric values, or a character string. Changing default parameter values is crucial to running simulations. In this guide, evonet parameter values are stored in the list object “evoparams”; however, any name can be used. Alternatives one and two will catch any misspelling (typos) of parameter names (e.g., if parameter “initial\_pop” is typed in as “initial\_pup”, an error message wil be returned.)

### To view names of all evonet parameters

``` r
names(evonet_setup)
sort(names(evonet_setup)) #alphabetical order
```

### To view default parameter values

``` r
evoparams <- evonet_setup()
evoparams$nsims #how many replicates 
evoparams$nsteps # length of simulation in days
evoparams$intial_pop # size of initial population
```

### Reading in parameters: Alternative 1, using a list and the do.call function

``` r
param_list=list(
  initial_pop = 400,
  initial_infected = 40,
)

evoparams <- do.call(evonet_setup,param_list)
```

### Reading in parameters: Alternative 2: using arguments directly in the evonet\_setup function

``` r
evoparams <- evonet_setup(initial_pop=400,
                          initial_infected=20)
```

### Reading in parameters: Alternative 3: readin all default parameters to “evoparams” object \#then modify/override

``` r
evoparams <- evonet_setup()
evoparams$nsim=1
evoparams$initial_pop=400
```

Fundamental / Core Parameters
-----------------------------

While EvoNet has about several dozen parameters (excluding parameters related to drug resistance modeling), most can remain at default values by the user. Important parameters that the casusal user may want to change are described below. A comprehensive overview is given in Section xx.

### Model Setup

-   **initial\_pop**: the number of agents in the initial populations. For runs on a local desktop or laptop, maximum size should be 3000-4000 due to performance issue. The larger the number of agents, the slower the model runs. Prototyping models with smaller initial population sizes, and then running larger models with calibrated/refined models is recommended. Depending on model dynamics, the population will either grow or decay from this initial size. (default value: 100)
-   **initial\_infected**: The numbe of infected agents at the model start; determines the initial prevalence level (can not be greater than “initial\_pop”). (default value: 20)
-   \*\*n\_steps: The length of the model run in days; typically specified as multiples of 365; e.g., a 10 year run can be specified as n\_steps=10*365. (default value: 365*3)
-   **nsims**: The number of replicates for each set of parameters implemented. EvoNet has a moderate level of stochasticity; thus, the same parameters can return different results for each model run. By increasing nsims, the mean values of each model replicate can be used as the final output for analysis and as the number of replicates increases, the mean results for each model run will be increasingly similar. Typically, 5 or more replicates will produce similar mean results for each set of parameters. The downside of increasing values of nsims is that model runtime increases linearly (unless a parallel computing framework is implemented, discussed in Section xx.) (default value: 1)
-   **fast\_edgelist**: This implements a recently developed TERGM estimation/simulation algorithm that dramatically decreases model runtime. Except for advanced users who have developed customized network estimation terms, this should be set to TRUE. (default value: FALSE).
-   **model\_sex**: Whether a model is MSM or heterosexual (“hetero”). (default value: “msm”).

### Viral Load and SPVL dynamics

-   **VL\_Function**: This determines which of two possible VL progression frameworks are implemented. The “default” option results in the typical VL progression as seen in Figure xx: an exponential increase followed by decresease in the acute phase to the SPVL, a slight linear increase over time in the chronic phase from the SPVL value, and then a linear increase during AIDS. The "mult\_drug\_res” option ……. (default value: “default”).
-   **AverageLogSP0** and **VarianceLogSP0**: The parameters determine the mean and variance parameters of the normal distributed SPVL in in the initia infected population. Changing these values will result in a different distribution of initial SPVL in th population: (default values: 4.5 and 0.8).

### Testing

-   **mean\_test\_interval\_female** and **mean\_test\_interval\_female**: Determines the frequency of HIV testing for the population and can be sex-specific. Frequency of testing for default testion option is deterministic, once specified time interval has passed for each agent, testing occurs . (default values: 442 and 365 days).

### Treatment

-   **start\_treatment\_campaign**: When treatment of tested and diagnosed agents begins (in days) since model start.
-   **tx\_type**: This determines the criteria used for an agent to receive treatment, given they are infected and have tested positive. The default setting is NA, thus a treatment type must be specified. The possible values are "VL", "CD4", "time", "vl\_and\_cd4", "vl\_and\_time", "vl\_and\_cd4\_and\_time", which refer to viral load, CD4 count, or time since infection, either alone or in combination.

### Coital Acts

-   **mean\_sex\_acts\_day**: Specifies the mean of a Poisson draw to calculate the number of sex acts each day for each relationship. Higher/lower values will increase/decrease transmission probabilities between discordonant couples. (default value: 0.2).
-   **condom\_prob**: Deteremines the probability that a condom will be used by insertive partner for each sex act. Higher/lower values will increase/decrease transmission probabilities between discordonant couples. (default value: 0.5).
-   **circum\_prob**: Determinines the probability that a male agent is circumcised at time of entry in the model. (default value: 0.85).
-   **prob\_iev**: The probability that when two men with “versatile” sex roles will switch roles during a given sex act. (default value: 0.4).

MSM vs. Heterosexual models
---------------------------

MSM and heterosexual models are nearly identical. The major difference is that MSM agents have a “role” attribute that deteremines whether they are receptive or insertive for a given sex act. Default model structure assumes an MSM model. To switch from default MSM network model to a heterosexual model, the following three parameters are changed to the following values: “model\_sex”, “nw\_form\_terms” (network formation terms), “nw\_coef\_form” (network coefficients for formation terms). This changes the default model from a MSM “edges only” (random mixing) to a heterosexual edges only model. Further specification of network structure is described below.

``` r
#read in default parameters (as list)
evoparams <- evonet_setup()    
#override default values
evoparams$model_sex <- "hetero"                            #Defaault = “msm”
evoparams$nw_form_terms <- "~edges + offset(nodematch('sex',diff=F))"  #Default = “~edges”
evoparams$nw_coef_form <- -Inf                             #Note negative sign
```
