Processing EvoNet output
================

Overview of important model outputs
-----------------------------------

The ouput of an EvoNet run is stored in a single data object (in all our examples it is called "evomodel" but can be any name.) This object is a list, which is a flexible R data structure where each element of a list can be any type of data object: e.g., a single number, a single character string, a vector of numbers, a matrix, a data frame, etc. We can see what objects this output list holds with the names function.

``` r
> names(evomodel)
[1] "param"            "control"          "nwparam"          "epi"              "stats"  
[6] "attr"             "pop"              "nw"               "coital_acts_list" "popsumm"
[11] "vl_list"          "InfMat"           "age_list"         "el"               "sessionInfo"
[16] "partner_list"     "popsumm_mats"
```

The structure of each object can be examined with the str function (see ?str). At level 0, str shows 17 objects and that the object evomodel is of class "netsim", and EpiModel class. This class attribute allows other functions to recognize the object as an EpiModel output object.

``` r
> str(evomodel,0)
List of 17
 - attr(*, "class")= chr "netsim"```
```

At level 1, some details of the the individual elements are shown. Here, we focus on three specific EvoNet objects: param, pop, and popsumm. The objects are coital\_acts\_list, partner\_list, age\_list, and vl\_list are also EvoNet objects but discsussed in later sections. The other objects are EpiModel specific objects and would be of interest to only very advanced users.

``` r
> str(evomodel,1)
List of 17
 $ param           :List of 3
 $ control         :List of 27
  ..- attr(*, "class")= chr "control.net"
 $ nwparam         :List of 1
 $ epi             :List of 3
 $ stats           : list()
 $ attr            :List of 3
 $ pop             :List of 3
 $ nw              :List of 1
 $ coital_acts_list:List of 1
 $ popsumm         :List of 3
 $ vl_list         :List of 1
 $ InfMat          :List of 1
 $ age_list        :List of 3
 $ el              :List of 3
 $ sessionInfo     :List of 3
 $ partner_list    :List of 1
 $ popsumm_mats    :List of 40
 - attr(*, "class")= chr "netsim"
```

We see that the objects param, popsumm, and pop are list of 3 elements. In this example, 3 replicate simulations were run and for each simulation an object was stored in each of those elements. The "param" objects hold the copy of parameters used in each replicate. In 99.99% of cases, each param list element will be an exact copy of the other ones and be exactly the same as the input parameter list for the mode run (called "evoparams" in our example) To access these objects, R's list indexing is used. The input parameters are stored in the ouput object so the user can readily identify the parameterization of the model as needed.

``` r
#using head functio to see first several elements
head(evomodel$param[[1]])
head(evomodel$param[[2]])
head(evomodel$param[[3]])
evomodel$param[[1]]
```

The "pop" object holds the values of the individual agent attributes for the three replicates. Unlike the "param" object, each element of "pop" will be different due to each model replicate having different dynamics due to the inherent stochasticity in the model. Each agent has has 131 separate attributes pertaining to demographic, epidemiological, treatment, and behavioral/sexual attributes. Below is a list of these attributes. Typicall, only a small subset of these are of interest to the user.

``` r
> sort(names(evomodel$pop[[1]]))
  [1] "adherence_start"                "adherence_type"                 "Adherence1"                    
  [4] "Adherence2"                     "Adherence3"                     "Adherence4"                    
  [7] "age"                            "age_infection"                  "ai_prob"                       
 [10] "aim3_mutations_long"            "aim3_no_muts"                   "Aim3RoundingErrors"            
 [13] "arrival_time"                   "att1"                           "CD4"                           
 [16] "cd4_at_test"                    "CD4_at_trtmnt"                  "CD4_initial_value"             
 [19] "CD4_nadir"                      "CD4_time"                       "CD4_time_death"                
 [22] "CD4_treatment_delay_index"      "CD4count"                       "CD4tot"                        
 [25] "ChronPhase"                     "circum"                         "condom_user"                   
 [28] "CYP_6_slow"                     "d_acute"                        "diag_resist_status"            
 [31] "diag_resist_time"               "diag_status"                    "diag_time"                     
 [34] "disclosure_status"              "Donors_age"                     "Donors_age"                    
 [37] "Donors_CD4"                     "Donors_d_acute"                 "Donors_diag_status"            
 [40] "Donors_EnvirContribToLogSP0"    "Donors_Generation"              "Donors_Index"                  
 [43] "Donors_LogSetPoint"             "Donors_SetPoint"                "Donors_Total_Time_Inf_At_Trans"
 [46] "Donors_treated"                 "Donors_treated_2nd_line"        "Donors_V"                      
 [49] "Donors_ViralContribToLogSP0"    "Drug1"                          "Drug2"                         
 [52] "Drug3"                          "Drug4"                          "eligible_2nd_line_ART"         
 [55] "eligible_ART"                   "eligible_care"                  "eligible_for_prep"             
 [58] "eligible_vl_test"               "enhanced_testing"               "EnvirContribToLogSP0"          
 [61] "ever_enhanced_testing"          "Generation"                     "have_diag_partner"             
 [64] "have_disc_partner"              "have_suppressed_partner"        "I_vec"                         
 [67] "id"                             "Imm_Trig"                       "insert_quotient"               
 [70] "K"                              "known_pos_partner_duration"     "L_vec"                         
 [73] "last_disc_sex"                  "last_neg_resist_test"           "last_neg_test"                 
 [76] "LogSetPoint"                    "M_vec"                          "min_time_tx"                   
 [79] "no_partners_now_prep"           "no_partners_past_prep"          "num_consec_VL_gt1k"            
 [82] "NumRecipients"                  "on_prep"                        "OnDrug"                        
 [85] "partner_recent_test"            "pos_partner_duration"           "PPP"                           
 [88] "prep_decrease"                  "prep_list"                      "r0"                            
 [91] "rand_prob_test"                 "rand_prob_test_init"            "RandomTimeToAIDS"              
 [94] "rate_phase2"                    "role"                           "s"                             
 [97] "SetPoint"                       "sex"                            "spvl_cat"                      
[100] "sqrt_age"                       "start_aids_cd4"                 "start_max_aids"                
[103] "Status"                         "sti_status"                     "Time_Death"                    
[106] "time_hiv_sex"                   "time_hiv_sex_act"               "Time_Inf"                      
[109] "Time_Inf_Adj"                   "time_init_2nd_line"             "total_acts"                    
[112] "treated"                        "treated_2nd_line"               "tx_dropout"                    
[115] "tx_init_time"                   "tx_schedule"                    "tx_stop_time"                  
[118] "V"                              "V_vec"                          "vacc_init_time"                
[121] "vaccinated"                     "ViralContribToLogSP0"           "virus_3_plus_drug_muts"        
[124] "virus_3_plus_drug_muts"         "virus_part_res_drug"            "virus_sens_drug"               
[127] "virus_sens_vacc"                "vl_at_test"                     "vl_expected"                   
[130] "vl_peak_agent"                  "vl_phase2_trans" 
```

Each element of pop is as long as the total number of agents that existed during the model run (for a particular replicate.) If a model starts with 300 agents and 117 enter the model by the end of the simulation, then each element of pop will consist of vectors of length 317. In the example below, the number of total agents in each replicate differed.

``` r
> length(evomodel$pop[[1]]$Status)
[1] 417
> length(evomodel$pop[[2]]$Status)
[1] 416
> length(evomodel$pop[[3]]$Status)
[1] 412
```

As an aside, using the lapply function is often useful for interacting with R lists (see ?lapply).

``` r
> unlist(lapply(1:3,function(x) length(evomodel$pop[[x]]$Status)))
[1] 417 416 412
```

Plotting the time of infection (days since model start) of the 3 simulations. Many infections occur before day 0. This is because the time of infection for the agents that are initially infected at the start of the model are randomly given time of infections between the start of the modle and two years prior (-730 - 0). Thus, positive times of infection are for agents that were not part of the initially infected population.

``` r
aa=evomodel$pop
time_inf_list=lapply(1:length(aa),function(xx) aa[[xx]]$Time_Inf)
par(mfrow=c(2,2))
lapply(time_inf_list,function(x) hist(x,main="Time of Infection",xlab="Day"))
```
![](https://github.com/EvoNetHIV/EvoNetHIV/blob/master/documentation/imgs/timeinf.png)


> The attribute "Status" refers to the current condition of an agent: 0, alive and uninfected; 1, alive and infected; -1.5, aged out of model (removed from model as maximum age was reached); -1, died of non-AIDS cause; -2, died of AIDS. The first 6 values for each Status vector can be viewed using the head function and the differences between replicates are clear.

``` r
> lapply(1:3,function(x) head(evomodel$pop[[x]]$Status))
[[1]]
[1] -1.5 -1.5 -2.0 -1.5  0.0  0.0

[[2]]
[1] -1.5 -1.5 -2.0 -1.5  1.0 -2.0

[[3]]
[1] -1.5 -1.5  0.0 -1.5  1.0 -2.0
```

To view all the values for replicate 1

``` r
evomodel$pop[[1]]$Status
```

Regular R functions can be applied to these vectors

``` r
> out=evomodel$pop[[1]]$Status
> table(out)
out
  -2 -1.5   -1    0    1 
  51   96    9  239   22 
```

The pop structure is such that each position in an attribute vector corresponds to the same agent.

``` r
> out=evomodel$pop[[1]]
> index=10
> out$sex[index]
[1] "m"
> out$age[index]
[1] 55.0016
> out$Status[index]
[1] -1.5
> out$Time_Inf[index]
[1] NA
> out$V[index]
[1] NA
> out$LogSetPoint[index]
[1] NA
```

Agent 80 is an example of infected agent. Note that time of infection "Time\_Inf" is in days since model start and V (viral load) is raw value and SPVL ("LogSetPoint") is on log10 scale.

``` r
> out=evomodel$pop[[1]]
> index=80
> out$sex[index]
[1] "m"
> out$age[index]
[1] 40.46227
> out$Status[index]
[1] 1
> out$Time_Inf[index]
[1] 4097
> out$V[index]
[1] 38006.65
> out$LogSetPoint[index]
[1] 4.0613
```

This shows that agent 10 was a male, that aged out (reached default maximum age at 55 years and Status of -1.5) and was never infected (Time\_Inf = NA) and therefore did not have a viral load (V), SPVL (LogSetPoint) value (both were NA).

Summary statistics
==================

At the end of each daily timestep, a suite of summary statistics is caclulated to quantify epidemioligical, behavioral, and network dynamics of the population. These statistics are based on individual agent attributes (e.g., infection status to calculate prevalence) and provide a time series of statistics that can be used to describe and quanitfy the evolution of the epidemic. The name of the data object that has these daily statistics is the "popsumm" object.

Again, here are output objects from a model run.

``` r
> names(evomodel)
[1] "param"            "control"          "nwparam"          "epi"              "stats"  
[6] "attr"             "pop"              "nw"               "coital_acts_list" "popsumm"
[11] "vl_list"          "InfMat"           "age_list"         "el"               "sessionInfo"
[16] "partner_list"     "popsumm_mats"
```

The has length of 3 because three simulations/replicates were run.

``` r
> length(evomodel$popsumm)
[1] 3
```

Here are the names of the popsumm variables in aphabetical order

``` r
> aa=evomodel
> sort(names(aa$popsumm[[1]]))
 [1] "aged_out"                          "aids_deaths"                      
 [3] "alive"                             "births"                           
 [5] "cd4_0_200"                         "cd4_200_350"                      
 [7] "cd4_gt_350"                        "mean_age_died_AIDS"               
 [9] "mean_age_incident"                 "mean_age_infecteds"               
[11] "mean_age_susceptibles"             "mean_degree"                      
[13] "mean_degree_30_50"                 "mean_degree_inf_untreated"        
[15] "mean_degree_over_50"               "mean_degree_under_30"             
[17] "mean_spvl_incident"                "mean_spvl_pop_all"                
[19] "mean_spvl_pop_untreated"           "mean_time_donor_infected_incident"
[21] "mean_trans_prob"                   "mean_vl_pop_all"                  
[23] "natural_deaths"                    "natural_deaths_infecteds"         
[25] "natural_deaths_infecteds"          "natural_deaths_susceptibles"      
[27] "new_diagnoses"                     "new_infections"                   
[29] "no_edges"                          "no_in_aids_cd4"                   
[31] "no_in_aids_gamma"                  "percent_donor_acute"              
[33] "prevalence"                        "prop_nodes_concurrent"            
[35] "prop_nodes_degree_0"               "prop_nodes_degree_1"              
[37] "susceptibles"                      "timestep"                         
[39] "total_infections_alive"            "total_infections_not_treated"     
>
```

Here are what the values look like with their first ten or so values.

``` r
> str(evomodel$popsumm[[1]])
List of 40
 $ timestep                         : num [1:244] 1 30 60 90 120 150 180 210 240 270 ...
 $ prevalence                       : num [1:244] 0.2 0.192 0.189 0.19 0.185 ...
 $ new_infections                   : num [1:244] 0 0 0 0 0 0 0 1 0 0 ...
 $ susceptibles                     : num [1:244] 160 160 159 158 159 160 159 159 159 161 ...
 $ total_infections_alive           : num [1:244] 40 38 37 37 36 36 36 36 36 36 ...
 $ births                           : num [1:244] 0 0 0 0 1 1 1 1 0 2 ...
 $ aids_deaths                      : num [1:244] 0 2 0 0 1 0 0 0 0 0 ...
 $ natural_deaths                   : num [1:244] 0 0 1 0 0 0 1 0 0 0 ...
 $ aged_out                         : num [1:244] 0 0 1 1 0 0 1 1 0 0 ...
 $ natural_deaths_infecteds         : num [1:244] 0 0 0 0 0 0 0 0 0 0 ...
 $ natural_deaths_susceptibles      : num [1:244] 0 0 1 0 0 0 1 0 0 0 ...
 $ alive                            : num [1:244] 200 198 196 195 195 196 195 195 195 197 ...
 $ no_in_aids_gamma                 : num [1:244] 3 1 1 1 1 1 1 1 2 3 ...
 $ no_in_aids_cd4                   : num [1:244] 3 1 1 1 1 1 1 1 1 2 ...
 $ natural_deaths_infecteds         : num [1:244] NA NA NA NA NA NA NA NA NA NA ...
 $ new_diagnoses                    : num [1:244] 0 7 1 6 4 3 2 3 3 3 ...
 $ percent_donor_acute              : num [1:244] NA NA NA NA NA NA NA 0 NA NA ...
 $ mean_time_donor_infected_incident: num [1:244] NaN NaN NaN NaN NaN NaN NaN 608 NaN NaN ...
 $ mean_age_incident                : num [1:244] NaN NaN NaN NaN NaN ...
 $ mean_age_died_AIDS               : num [1:244] NaN 43.6 NaN NaN 26 ...
 $ mean_spvl_pop_all                : num [1:244] 4.58 4.51 4.5 4.5 4.49 ...
 $ mean_vl_pop_all                  : num [1:244] 4.64 4.57 4.56 4.57 4.57 ...
 $ mean_spvl_incident               : num [1:244] NaN NaN NaN NaN NaN ...
 $ mean_spvl_pop_untreated          : num [1:244] 4.58 4.51 4.5 4.5 4.49 ...
 $ total_infections_not_treated     : num [1:244] 40 38 37 37 36 36 36 36 36 36 ...
 $ mean_age_infecteds               : num [1:244] 37.2 36.9 36.5 36.6 37 ...
 $ mean_age_susceptibles            : num [1:244] 36.4 36.5 36.5 36.4 36.4 ...
 $ mean_trans_prob                  : num [1:244] NA 0.00774 0.00145 0.00509 NA ...
 $ no_edges                         : num [1:244] 38 40 39 36 34 37 35 33 34 34 ...
 $ mean_degree                      : num [1:244] 0.38 0.404 0.398 0.369 0.349 ...
 $ mean_degree_inf_untreated        : num [1:244] 0.45 0.553 0.595 0.405 0.306 ...
 $ prop_nodes_degree_0              : num [1:244] 0.67 0.662 0.684 0.677 0.687 ...
 $ prop_nodes_degree_1              : num [1:244] 0.28 0.278 0.245 0.277 0.282 ...
 $ prop_nodes_concurrent            : num [1:244] 0.05 0.0606 0.0714 0.0462 0.0308 ...
 $ cd4_gt_350                       : num [1:244] 34 34 34 34 33 33 33 33 31 31 ...
 $ cd4_200_350                      : num [1:244] 3 3 2 2 2 2 2 2 4 3 ...
 $ cd4_0_200                        : num [1:244] 3 1 1 1 1 1 1 1 1 2 ...
 $ mean_degree_under_30             : num [1:244] 0.429 0.444 0.381 0.349 0.397 ...
 $ mean_degree_30_50                : num [1:244] 0.4 0.431 0.444 0.37 0.315 ...
 $ mean_degree_over_50              : num [1:244] 0.185 0.192 0.24 0.417 0.375 ...
```

Each popsumm variable above has a length of 244. This is because the model ran for 20 years (7300 days) and the summary statistics were calculated at the default frequency of 30 days. 30 days is the default frequency to not slow down the model too much as doing all the summary statistics calculations can take a bit of time. The first value for each popsumm variable represents the initial condition for the variable, that is what the population/network is like before any of the model processes begin. Thus, the length of 244 for the example above is calculated as

``` r
 pp=evomodel$param[[1]]
> pp$n_steps
[1] 7300
> pp$popsumm_frequency
[1] 30
> floor(7300/30)+1
[1] 244
```

We recommend keeping the "popsumm\_frequency" variable at 30 days, but it can be changed to larger or smaller values, e.g.,

``` r
evoparams$popsumm_frequency=1
#or
evoparams$popsumm_frequency=60
```

In this example there are 3 replicates, thus each variable has 3 separate vectors of output, using the head(), we'll look at the first 6 values of each replicate for the 'prevalence' variable. We notice that they start at the same value but then diverge, though are still quite similar at this point.

``` r
> head(round(aa$prevalence,3))
[1] 0.200 0.192 0.189 0.190 0.185 0.184
> aa=evomodel$popsumm[[2]]
> head(round(aa$prevalence,3))
[1] 0.200 0.175 0.174 0.174 0.174 0.174
> aa=evomodel$popsumm[[3]]
> head(round(aa$prevalence,3))
[1] 0.200 0.196 0.201 0.193 0.193 0.185
```

Plotting the three prevalence replicates.

``` r
#note: timestep variable will be the same for all replicates.
timestep=evomodel$popsumm[[1]]$timestep/365
popsumm=evomodel$popsumm
prev1=popsumm[[1]]$prevalence
prev2=popsumm[[2]]$prevalence
prev3=popsumm[[3]]$prevalence

plot(timestep,prev1,type='o',pch=16,col=1,ylab="prevalence",xlab="years",ylim=c(0,.21))
lines(timestep,prev2,type='o',pch=16,col=2)
lines(timestep,prev3,type='o',pch=16,col=3)
```
![](https://github.com/EvoNetHIV/EvoNetHIV/blob/master/documentation/imgs/prev_ex.png)


Below is an example of the default output summary plots automatically generated for each model run.

![](https://github.com/EvoNetHIV/EvoNetHIV/blob/master/documentation/imgs/output_popsumm1.png)
![](https://github.com/EvoNetHIV/EvoNetHIV/blob/master/documentation/imgs/output_popsumm2.png)
![](https://github.com/EvoNetHIV/EvoNetHIV/blob/master/documentation/imgs/output_popsumm3.png)
![](https://github.com/EvoNetHIV/EvoNetHIV/blob/master/documentation/imgs/output_popsumm4.png)
![](https://github.com/EvoNetHIV/EvoNetHIV/blob/master/documentation/imgs/output_popsumm5.png)
![](https://github.com/EvoNetHIV/EvoNetHIV/blob/master/documentation/imgs/output_popsumm6.png)
![](https://github.com/EvoNetHIV/EvoNetHIV/blob/master/documentation/imgs/output_popsumm7.png)
![](https://github.com/EvoNetHIV/EvoNetHIV/blob/master/documentation/imgs/output_popsumm8.png)
