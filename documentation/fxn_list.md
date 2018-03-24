EvoNet Function Overview
================

(In progress, Needs updating)

Introduction
------------

The following outline is meant as a quick reference for all EvoNet functions. The function names are in bold. Function arguments have not been included, but more detail is provided in the other vignettes. Very few of these functions would be called directly by the user, as they are internal functions only. The vignette "Running EvoNet" shows what EvoNet functions would be called directly by the user.

### Input parameter functions

**input\_parameters\_primary** Returns list of default values of evonet parameter values.

**input\_parameters\_derived** Returns parameter values that are functions of other parameter values. Requires as input the list returned by input\_parameters\_primary. Important parameters returned are sex-specific daily natural mortality values, sex-specific initial age distributions, and heritability.

**input\_parameters\_agent\_attributes** Returns vector of individual agent attributes (e.g., “sex”, “age”,”SPVL”,etc.) that populate “pop” data structure. This vector is input to initialize\_agents (called in initialize\_module) which creates a list with the number of elements equal to the number of agent attributes and the length of each element equal to the size of the initial population.

**input\_parameters\_cd4\_data** Returns two tables that describe CD4+ T cell decline over time in untreated infectionns, as based on Cori, Pickles, et al (AIDS, 2015): i) cd4\_init\_probs is a 9x3 table that gives the probability of a newly infected agent starting with one of the first three cd4 categories base on agents SPVL; ii) CD4\_lookup is a 9x4 table that contains the mean expected passage time in each of the 4 CD4 categories based on agent’s SPVL.

**input\_parameters\_qaqc** Evaluates parameter values for plausible values, returns warning/error when warranted (e.g., initial population size is non-positive). Mostly placeholder currently.

### EpiModel related setup functions

**setup\_epimodel\_control\_object** Called in master\_script; helper function to fill in and run EpiModel’s control.net(). An important component is the save.other vector created within the function. This vector of object names specifies the objects that will be returned at the end of the model run (i.e., after netsim called) beyond the default EpiModel objects. Thus, any new output objects created must be specified with the save.other vector.

**setup\_initialize\_network** Sets network attributes “age”,”sqrt\_age”, “sex”, “role”, and “att1” for each node (agent) on the network, which is required for estimation of networks using these attributes. If additional attributes are desired, they can be added following the template within the function.

### Model initialization

**initialize\_module** Sets up internal data structures and initial virological and demographic values for the initial population. with the following steps: i) setups internal evonet data structures pop and popsumm with initialize\_agents(); ii) runs EpiModel function initialize.net(); iii) fills in virological and associated values for initial infected with initialize\_infecteds\_vl(); iv) updates VL values of initial infected based on assumed time of infection with viral\_update\_aim2(); v) creates data structure (vl\_list) to store daily VL values of infected agents; vi) creates optional data structures based on user input with initialize\_evonet\_misc; vii) calculates summary statistics for initial population with summary\_popsumm().

**initialize\_module subfunctions** Following functions are called within *initialize\_module*

**initialize\_evonet\_misc** Performs miscellaneous initialization steps to setup evonet model run: i) creates “coital\_acts\_list”, which saves the object discord\_coital df (table of relevant agent attributes in determining transmission probability) created each time step in social\_coital\_act; list filled in summary\_misc; ii) creates object “InfMat” which for each new infections saves time of infections, id of infectee, and id of infector; list filled in transmission\_bookkeeping\_module; iii) creates the popsumm data object which stores model summary statistics each timestep iv) creates age\_list, which stores age data of population at beginning of model run and at quarterly inverals for histogram plots created in plots\_popsumm; v) creates partner\_list object which track for each agent the time a new relationship starts.

**initialize\_infecteds\_vl** Calculates initial viral load and associated values for initial infected agents.

**initialize\_infecteds\_cd4** Description: fills in initial CD4 and associated values for initial infecteds; must be run after initial\_infecteds\_cd4 as SPVL values are required.

**initialize\_popsumm\_dynamics** Sets up popsumm list and fills in default value of NA or 0 for all elements; then fills in initial value; popsumm is list of various statistics calculated each timestep to describe network/population/epidemic.

**initialize\_agents** Creates pop data object which contains the agent attribute values. The attribute names for each agent is returned by input\_parameters\_agent\_attributes. Default values are NA, other values are calculated by subfunction new\_additions\_fxn.

### Coital acts

**social\_coital\_acts\_module** Wrapper function that calls: social\_discord\_edgelist\_df, social\_coital\_acts, social\_role\_msm, social\_act\_type\_het, social\_condom\_use. These functions identify discordonant relationships, organize relevant behavioral attributes, and calcuate number of sex acts per partnership per timestep.

**social\_discord\_edgelist\_df** Creates table of discordonant couples with their relevant attributes (e.g., sex, infection status, role): “discord\_edgelist\_df”. This dataframe is main input into transmission functions. Couples with the infected agent past the specified threshold time in AIDS are not included (assumes no sex for these couples). Raw table of discordonant couple IDs is created with call to social\_discord\_edgelist, then attributes are appended to table to create a data.frame object.

**social\_coital\_acts** Description: takes the data.frame, discord\_edgelist\_df, returned from social\_discord\_edgelist\_df and calculates the number of acts per partnership for timestep then expands data.frame so each row represents single sex act if couples have more than one act per timestep; eliminates couples with zero acts for timestep.

**social\_role\_msm** Description: determines coital acts role for msm, any agents are other than “versatile”; also determines if “flipping” occurs per V-V couple – if so, adds row to discord\_edgelist\_df and is treated as new act.

**social\_condom\_use** Description: assigns condom use (0/1) to each row (analagous to act) in discord\_edgelist\_df (table used to calculate transmission probabilities).

**social\_testing\_diagnosis\_module** Determines whether agent is tested. Two types of testing model: "interval", "memoryless". If infected agent is tested, diagnosis\_status changes from 0 to 1 and eligibility for care determined (default: all agents eligible for care). Time of testing or diagnosis is recorded for all agents.

**social\_treatment\_module** Assigns treatment status (0/1) to eligible patients based on various criteria: the following criteria must be satisfied for all agents: a treatment campaign exists, agent is infected and diagnosed, and agent is eligible for care. Default setting is that no treatment occurs in acute phase but can be changed. Additionally, additional user specified treatment criteria includes: i) agent VL &gt; specified VL for treatment (option 'VL'); ii) agent CD4 counts &lt; minimum threshold for treatment (option “CD4”); iii) has agent been infected for a minimum amount of specified time (option “time”) and combinations of these three criteria: "vl\_and\_cd4","vl\_and\_time","vl\_and\_cd4\_and\_time". If agent meets given criteria then treatment status changes from 0 to 1.

### Transmission dynamics

**transmission\_main\_module** For discordonant couples that have sex in the given timestep (identified in the 'social\_coital\_acts\_module'), calculates infection probability of susceptible partner based on who is receptive/insertive, and condom use, sti status, age, VL, and transmission model (e.g., 'hughes','exponential;); after infection probability calculated for susceptible agent, infection determined by uniform draw. With default settings (parameter 'transmission\_model' = 'hughes'), the actual infection probability is caclculated by the 'transmission\_hughes\_and\_exp' sub-function.

**transmission\_bookkeeping** For newly infected agents identified in the "transmission\_main\_module", calculates viral load data and SPVL for agents and records donor's information.

**transmission\_hughes\_and\_exp** Calculates the infection probability based on risk factors of the discordonant couple using model from Hughes et al. (20xx). Called in function 'transmission\_main\_module'.

**transmission\_cd4\_module**
Calculates initial CD4 values for newly infected agents based on their SPVL values.

### Vital dynamics ('births', ageing, death)

#### 'Births' / New agents

**vital\_births\_module**
Wrapper function that calls 'vital\_births\_calculate\_new', 'vital\_births\_bookkeeping\_pop', 'vital\_births\_bookkeeping\_misc'

**vital\_births\_calculate\_new** Calculates number of new agents added to the population. Default setting, (parameter 'birth\_model' ='poisson\_birth\_numbers') increases population approximately 1% per year from initial population size.

**vital\_births\_bookkeeping\_pop** If new agents are added to the population in a given timestep, this function adds agents to the internal 'pop' data structure which stores all agent attributes (age, sex, infection status, etc.) via call to 'vital\_new\_additions' function.

**vital\_births\_bookkeeping\_misc** If new agents are added to the population, this function interfaces with the network related functions to add agents and relevant attributes (sex, msm role, age) to the network.

#### Ageing

**vital\_aging\_module** Increases agent's age one day per timestep

#### Mortality

**vital\_deaths\_module** Wrapper function that calls 'vital\_death\_aids', 'vital\_death\_aged\_out' , 'vital\_death\_non\_aids'.

**vital\_death\_aids**
For agents that have passed through the final CD4 stage (stage 4, &lt;200)

dat &lt;- vital\_death\_aids(dat,at) dat&lt;- vital\_death\_aged\_out(dat,at) dat &lt;- vital\_death\_non\_aids(dat,at)
