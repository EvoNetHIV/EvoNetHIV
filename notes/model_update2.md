### 02/10/20 Update
Main update is initial efforts to have a loose object-oriented framework with R’s S3 frameworks using generic functions.  To specify a given model, the parameter vacc_model_id is set; e.g.,  vacc_model_id <- “model_1” specifies that model 1 will be run. Functions pertaining to model 1 are found in R/vaccination_model_1.  Generic definitions are in R/vaccination_model_generics.

**New object:**  
  <br/>
  *dat``$``vacc_model`$`agents*:  
  list where each element hold vaccine related attributes for individual agent.  
  Element position corresponds to agent ID; eg., 10th element is agent 10.  Current attributes are *phi*, 
  *mu*, *sigma*. Each attribute is an arbitrary length vector; i.e., can hold as much information in whatever format as needed. For model 1 (analogous to original vaccine model), each of these attributes take single numeric values. This object is created in the module *initialize_vaccine_agents()*.  New agents created during model run are also added in this module.  
  <br/>
**New functions:**  

  *initialize_vaccine_agents*:  
  This is a stand alone-module that creates the *dat`$`vacc_model`$`agents* object (see above) at start of model and adds new agents during model runs.  Values of *mu* and *sigma* are initialized for the initially infected population as well as for secondary infections. Also, this assigns the dat object an additional class (i.e., in addition to the list class)  based on value of user-specified parameter *vacc_model_id*.  Given this class assignment, functions will dispatch appropriate methods (i.e., call the appropriate functions for the given model specified.)  
  <br/>
*initialize_and_update_phi*:  
Stand-alone module which calls functions *initialize_phi* and *update_phi*. Module and sub-functions called each timestep.  

  - *initialize_phi*:  
Given the vaccine campaign is ongoing, calculates phi values for selected agents (i.e., agents who undergo vaccination). For model 1, *phi* is 0/1.
  - *update_phi*:  
For vaccinated agents, updates *phi* values. For model 1, this means determining whether vaccine has waned (stochastically), *phi* -> 0.  

<br/>
*update_mu_and_sigma*:  
Stand-alone module that calls functions *update_mu* and *update_sigma*. This module and sub functions called each timestep.   


  - *update_mu*:  
Updates mu values each time step for infected agents. For model 1, infected agents mu is actually time-invariant.  For newly infected agents in model 1, value of mu is taken directly from infector.  

  - *update_sigma*:  Updates *sigma* values for infected agents each timestep. For model 1, *sigma* is 0 for all agents and is time-invariant.  
  

*draw_m*:  
draws a value of mark(s) for each virus from each infector, based on infector’s value of *mu* and *sigma*. For model 1, it returns value of *mu* from each infector. This is called in *transmission_main_module* and is only called after the vaccine campaign has started.  

*calculate_theta*:  
Called in *transmission_main_module*. Based on values of *m* and other agent attributes and parameters, calculates a vector of values which is then used to adjust initial transmission probabilities:  
```r
  trans_probs <- trans_probs_raw *(1-theta)
```





