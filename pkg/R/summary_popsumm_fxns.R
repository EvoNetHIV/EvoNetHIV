#' @title Title
#'
#' @description Description
#'
#' @param x A number.
#' @param y A number.
#' @return return value here.
#' @details
#' Additional details here
#' @examples
#' example function call here

#' @export
summary_popsumm_fxns<-function(generic_nodal_att_values=NULL,aim3,fast_el,params=NULL)
{
  # Description
  # Returns list "popsumm_fxns" where each list element is a "popsumm" statistic
  # eg, calculates a summary statistic of population or network at each timestep
  # or at frequency of parameter "popsumm_frequency". This list is processed
  # in "summary_popsumm" and in "plots_popsumm"
  # This format, allows a user to quickly add a summary stat and specify the 
  # type of plot it should have in the "popsumm_figures.pdf"
  # It's motivation was to consolidate all summary stats in one place so they
  #  can be quickly added/modified/deleted as necessary and to streamline plotting
  # The functions can reference the logical vectors and indices created in 
  #   "summary_popsumm" or can use whatever code is desired (ie, indices from
  #"   summary_popsumm" are helpful but not necessary,see "no_in_aids_gamma")
  # Generic_attributes: since the number of generic attributes can vary between
  #     model runs, they can not be pre-specified in advance and need special treatment.
  #     The function values for generic attributes are NA by default and calculated 
  #     in "summary_popsumm"  
  
  #plotting options are:
  # plot_type="line_raw"- standard timeseries plot, 
  #            "line_cumul" - cumulative sum of values
  #            "points" - scatterplot (helpful when NAs present or process being
  #                       plotted doesn't happen every timestep such as infections)
  # "overlay" option, by specifying overlay=xx, where xx is another summary stat
  #            variable xx will be added as a secondary variable to plot at same scale
  #            as original plot (see "susceptibles")
  # loess=TRUE,FALSE; when plot_type="points", this specifies whether a loess
  #       plot should be added to graph (see "mean_age_incident")
  # ymin="data", when ymin="data", the minimum values is based on range of data
  #        otw, default value is 0 if not specified
  
  popsumm_fxns=list()
  
  popsumm_fxns$"timestep"<-
    list(model_value_fxn   = function(...){at},
         plot_type="line_raw",description="")
  
  popsumm_fxns$"prevalence"<-
    list(model_value_fxn   = function(...){length(which(inf_index))/length(which(alive_index))},
         plot_type="line_raw",description="(agents infected / agents alive)")
  
  popsumm_fxns$"new_infections"<-
    list(model_value_fxn   = function(...){new_infections_count},
         plot_type="line_raw",description="")
  
  popsumm_fxns$"susceptibles"<-
    list(model_value_fxn   = function(...){length(which(sus_index))},
         plot_type="line_raw",overlay="total_infections_alive",description="")
  
  popsumm_fxns$"total_infections_alive"<-
    list(model_value_fxn   = function(...){length(which(inf_index))},
         plot_type="line_raw",description="")
  
  popsumm_fxns$"births"<-
    list(model_value_fxn   = function(...){length(which(new_births))},
         plot_type="line_cumul",description="")
  
  popsumm_fxns$"aids_deaths"<-
    list(model_value_fxn   = function(...){length(which(died_aids))},
         plot_type="line_cumul",description="")
  
  popsumm_fxns$"natural_deaths"<-
    list(model_value_fxn   = function(...){length(which(died_non_aids))},
         plot_type="line_cumul",description="")
  
  popsumm_fxns$"aged_out"<-
    list(model_value_fxn   = function(...){length(which(aged_out))},
         plot_type="line_cumul",description="")
  
  popsumm_fxns$"natural_deaths_infecteds"<-
    list(model_value_fxn   = function(...){length(which(died_non_aids_inf))},
         plot_type="line_cumul",description="")
  
  popsumm_fxns$"natural_deaths_susceptibles"<-
    list(model_value_fxn   = function(...){length(which(died_non_aids_sus))},
         plot_type="line_cumul",description="")
  
  if (params$start_treatment_campaign < 5e5) {  #only plot if treatment is available in model 
    popsumm_fxns$"total_infections_not_treated" <-
      list(model_value_fxn   = function(...){length(which(inf_index & not_treated_index))},
           plot_type="line_raw",description="")}
  
  popsumm_fxns$"alive"<-
    list(model_value_fxn   = function(...){length(which(alive_index))},
         plot_type="line_raw",description="")
  
  if(params$circum_prob != 0.85) {   # plot circumcision prevalence if not at default
    popsumm_fxns$"circum_prev" <-
      list(model_value_fxn = function(...) {circum_prev},
           plot_type = "line_raw", description = "Circumcision prevalence")}
  
  #hetero dynamics here
  if(!is.null(params)){
    if(params$model_sex=="hetero"){
      
      popsumm_fxns$"alive_female"<-
        list(model_value_fxn   = function(...){no_females_alive},
             plot_type="line_raw",description="")
      
      popsumm_fxns$"alive_male"<-
        list(model_value_fxn   = function(...){no_males_alive},
             plot_type="line_raw",description="")
      
      popsumm_fxns$"prev_15to24" <-
        list(model_value_fxn = function(...) { prev_15to24 },
             plot_type = "line_raw", description = "Prevalence, ages 15-24")
      
      popsumm_fxns$"prev_15to49" <-
        list(model_value_fxn = function(...) { prev_15to49 },
             plot_type = "line_raw", description = "Prevalence, ages 15-49")
      
      popsumm_fxns$"prev_f_15to24" <-
        list(model_value_fxn = function(...) { prev_f_15to24 },
             plot_type = "line_raw", description = "Prevalence, females ages 15-24")
      
      popsumm_fxns$"prev_f_15to49" <-
        list(model_value_fxn = function(...) { prev_f_15to49 },
             plot_type = "line_raw", description = "Prevalence, females ages 15-49")
      
      popsumm_fxns$"prev_m_15to24" <-
        list(model_value_fxn = function(...) { prev_m_15to24 },
             plot_type = "line_raw", description = "Prevalence, males ages 15-24")
      
      popsumm_fxns$"prev_m_15to49" <-
        list(model_value_fxn = function(...) { prev_m_15to49 },
             plot_type = "line_raw", description = "Prevalence, males ages 15-49")
      
    }
  }
  
  if (params$start_treatment_campaign < 5e5) {  #only plot if treatment is available in model 
    popsumm_fxns$"no_treated"<-
      list(model_value_fxn   = function(...){length(which(inf_index & treated_index))},
           plot_type="line_cumul",description="")
    
    popsumm_fxns$"percent_suppressed"<-
      list(model_value_fxn   = function(...){
        (length(which(treated_index & 
                        ((at-dat$pop$tx_init_time)>100) &
                        (log10(dat$pop$V)< dat$pop$LogSetPoint/10) &
                        length(inf_index))) / length(inf_index) )
      },
      plot_type="line_raw",description="")
  }
  
  popsumm_fxns$"no_in_aids_gamma"<-
    list(model_value_fxn   = function(...){length(which((at > (dat$pop$Time_Inf + 
                                                                 dat$pop$RandomTimeToAIDS))&
                                                          inf_index))},
         plot_type="line_raw",description="")
  
  popsumm_fxns$"no_in_aids_cd4"<-
    list(model_value_fxn   = function(...){length(which(cd4_aids & inf_index ))},
         plot_type="line_raw",description="")
  
  popsumm_fxns$"natural_deaths_infecteds"<-
    list(model_value_fxn   = function(...){length(which(died_non_aids_inf))},
         plot_type="line_cumul",description="")
  
  popsumm_fxns$"natural_deaths_susceptibles"<-
    list(model_value_fxn   = function(...){length(which(died_non_aids_sus ))},
         plot_type="line_cumul",description="")
  
  popsumm_fxns$"new_diagnoses"<-
    list(model_value_fxn   = function(...){length(which(new_diagnoses))},
         plot_type="line_cumul",description="")
  
  if (params$start_treatment_campaign < 5e5) {  #only plot if treatment is available in model 
    popsumm_fxns$"no_treated"<-
      list(model_value_fxn   = function(...){length(which(treated_index))},
           plot_type="line_raw",overlay="no_treated_undetectable",description="")}
  
  if(params$model_sex!="hetero"){               #only plot if women not in model
    popsumm_fxns$"inf_men"<-
      list(model_value_fxn   = function(...){length(which(inf_male_index))/length(which(male_index))},
           plot_type="line_raw",description="")}
  
  if(params$model_sex=="hetero"){               # only plot if women are in model
    popsumm_fxns$"inf_men"<-
      list(model_value_fxn   = function(...){length(which(inf_male_index))/length(which(male_index))},
           plot_type="line_raw",overlay="inf_women",description="")
    
    popsumm_fxns$"inf_women"<-
      list(model_value_fxn   = function(...){length(which(inf_female_index))/length(which(female_index))},
           plot_type="line_raw",description="")}
  
  if (params$start_treatment_campaign < 5e5) {  #only plot if treatment is available in model 
    popsumm_fxns$"treated_inf_men"<-
      list(model_value_fxn   = function(...){length(which(treated_inf_male_index))/length(which(inf_male_index))},
           plot_type="line_raw",overlay="treated_inf_women",description="")}
  
  if(params$model_sex=="hetero" & params$start_treatment_campaign < 5e5){       # only plot if women and treatment are in model
    popsumm_fxns$"treated_inf_women"<-
      list(model_value_fxn   = function(...){length(which(treated_inf_female_index))/length(which(inf_female_index))},
           plot_type="line_raw",description="")}
  
  popsumm_fxns$"inf_under30"<-
    list(model_value_fxn   = function(...){length(which(inf_under30_index))/length(which(under30_index))},
         plot_type="line_raw",overlay="inf_30to50",description="")
  
  popsumm_fxns$"inf_30to50"<-
    list(model_value_fxn   = function(...){length(which(inf_agents30to50_index))/length(which(agents30to50_index))},
         plot_type="line_raw",description="")
  
  popsumm_fxns$"inf_over50"<-
    list(model_value_fxn   = function(...){length(which(inf_over50_index))/length(which(over50_index))},
         plot_type="line_raw",description="")
  
  if (params$start_treatment_campaign < 5e5) {  #only plot if treatment is available in model 
    popsumm_fxns$"treated_inf_under30"<-
      list(model_value_fxn   = function(...){length(which(treated_inf_under30_index))/length(which(inf_under30_index))},
           plot_type="line_raw",description="")
    
    popsumm_fxns$"treated_inf_30to50"<-
      list(model_value_fxn   = function(...){length(which(treated_inf_agents30to50_index))/length(which(inf_agents30to50_index))},
           plot_type="line_raw",description="")
    
    popsumm_fxns$"treated_inf_over50"<-
      list(model_value_fxn   = function(...){length(which(treated_inf_over50_index))/length(which(inf_over50_index))},
           plot_type="line_raw",description="")
    
    popsumm_fxns$"no_treated_undetectable"<-
      list(model_value_fxn   = function(...){length(which(treated_undetectable))},
           plot_type="line_raw",description="")
    
    popsumm_fxns$"percent_treated_undetectable"<-
      list(model_value_fxn   = function(...){length(which(treated_undetectable))/length(which(treated_index))},
           plot_type="line_raw",description="")
  }
  
  popsumm_fxns$"percent_donor_acute"<-
    list(model_value_fxn   = function(...){
      donor_acute_count/length(which(new_infections))},
      plot_type="points",loess=F,description="")
  
  popsumm_fxns$"mean_time_donor_infected_incident" <-
    list(model_value_fxn   = function(...){mean(dat$pop$Donors_Total_Time_Inf_At_Trans[which(new_infections)])},
         plot_type="points",loess=T,ymin="data",description="")
  
  popsumm_fxns$"mean_age_incident" <-
    list(model_value_fxn   = function(...){mean(dat$pop$age[which(new_infections)])},
         plot_type="points",loess=T,ymin="data",description="")
  
  popsumm_fxns$"mean_age_died_AIDS"<-
    list(model_value_fxn   = function(...){mean(dat$pop$age[which(died_aids)])},
         plot_type="points",loess=T,ymin="data",description="")
  
  if (params$start_treatment_campaign < 5e5) {  #only plot if treatment is available in model 
    popsumm_fxns$"mean_spvl_pop_all"<-
      list(model_value_fxn   = function(...){mean(dat$pop$LogSetPoint[which(inf_index)])},
           plot_type="line_raw",ymin="data",description="")}
  
  if (params$start_treatment_campaign < 5e5) {  #only plot if treatment is available in model 
    popsumm_fxns$"mean_vl_pop_untreated"<-
      list(model_value_fxn   = function(...){mean(log10(dat$pop$V[which(inf_index &  not_treated_index)]))},
           plot_type="line_raw",ymin="data",description="")}
  
  popsumm_fxns$"mean_spvl_pop_untreated"<-
    list(model_value_fxn   = function(...){mean(spvl_untreated_values)},
         plot_type="line_raw",ymin="data",description="") 
  
  popsumm_fxns$"mean_vl_pop_all"<-
    list(model_value_fxn   = function(...){mean(log10_vl_values)},
         plot_type="line_raw",ymin="data",description="")
  
  popsumm_fxns$"mean_spvl_incident"<-
    list(model_value_fxn   = function(...){mean(dat$pop$LogSetPoint[which(new_infections)])},
         plot_type="points",loess=T,description="")
  
  if (params$perc_vaccinated != 0.5) {    #only plot if vaccine campaign in model
    popsumm_fxns$"new_infections_vacc_sens_virus"<-
      list(model_value_fxn   = function(...){ new_infections_virus_vacc_sens_count},
           plot_type="line_raw",description="")
    
    popsumm_fxns$"new_infections_vacc_resist_virus"<-
      list(model_value_fxn   = function(...){ new_infections_virus_vacc_notsens_count},
           plot_type="line_cumul",description="")
    
    popsumm_fxns$"percent_virus_sensitive_vacc"<-
      list(model_value_fxn   = function(...){percent_virus_sensitive},
           plot_type="line_raw",description="")
    
    popsumm_fxns$"percentAliveVaccinated"<-
      list(model_value_fxn   = function(...){percentVaccinated},
           plot_type="line_raw",description="")
  }
  
  if (params$start_treatment_campaign < 5e5) {  #only plot if treatment is available in model 
    #default value is NA, so need append zero to get a zero when at==1
    #eg, sum(0,NA,na.rm=T)=0 sum(NA,na.rm=T)=NA
    popsumm_fxns$"total_pills_taken"<-
      list(model_value_fxn   = function(...){sum(c(0,dat$popsumm$no_treated[1:popsumm_index]),na.rm=T)},
           plot_type="line_raw",description="")}
  
  popsumm_fxns$"mean_age_infecteds"<-
    list(model_value_fxn   = function(...){mean(dat$pop$age[which(inf_index)])},
         plot_type="line_raw",ymin="data",description="")
  
  popsumm_fxns$"mean_age_susceptibles"<-
    list(model_value_fxn   = function(...){mean(dat$pop$age[which(sus_index)])},
         plot_type="line_raw",ymin="data",description="")
  
  #  popsumm_fxns$"number_coit_acts"<-
  #    list(model_value_fxn   = function(...){number_coit_acts},
  #         plot_type="line_raw",ymin="data",description="")
  
  #  popsumm_fxns$"percent_acts_iev"<-
  #    list(model_value_fxn   = function(...){percent_iev},
  #         plot_type="line_raw",ymin="data",description="")
  
  #  popsumm_fxns$"transmission_opps_condom_percent"<-
  #    list(model_value_fxn   = function(...){transmission_opps_condom_percent},
  #         plot_type="line_raw",ymin="data",description="")
  
  popsumm_fxns$"mean_trans_prob"<-
    list(model_value_fxn   = function(...){trans_probs_mean},
         plot_type="line_raw",ymin="data",description="")
  
  
  popsumm_fxns$"no_edges"<-
    list(model_value_fxn   = function(...){number_edges},
         plot_type="line_raw",ymin="data",description="")
  
  popsumm_fxns$"mean_degree"<-
    list(model_value_fxn   = function(...){ number_edges*2/network_size},
         plot_type="line_raw",ymin="data",description="")
  
  popsumm_fxns$"mean_degree_inf_untreated"<-
    list(model_value_fxn   = function(...){  sum(edges_untreated)/length(not_treated_agents)},
         plot_type="line_raw",ymin="data",description="")
  
  if (params$start_treatment_campaign < 5e5) {  #only plot if treatment is available in model 
    popsumm_fxns$"mean_degree_inf_treated"<-
      list(model_value_fxn   = function(...){ sum(edges_treated)/length(treated_agents)},
           plot_type="line_raw",ymin="data",description="")}
  
  popsumm_fxns$"prop_nodes_degree_0"<-
    list(model_value_fxn   = function(...){nw_summary[1]/total_nodes},
         plot_type="line_raw",ymin="data",description="")
  
  popsumm_fxns$"prop_nodes_degree_1"<-
    list(model_value_fxn   = function(...){nw_summary[2]/total_nodes},
         plot_type="line_raw",description="")
  
  popsumm_fxns$"prop_nodes_concurrent"<-
    list(model_value_fxn   = function(...){nw_summary[3]/total_nodes},
         plot_type="line_raw",description="")
  
  popsumm_fxns$"mean_degree_under_30"<-
    list(model_value_fxn   = function(...){ 
      if (length(agents_under30) > 0) sum(edges_under30)/length(agents_under30)
      else NA
    },
    plot_type="line_raw",ymin="data",description="")
  
  popsumm_fxns$"mean_degree_30_50"<-
    list(model_value_fxn   = function(...){
      if (length(agents_30to50) > 0)
        sum(edges_30to50)/length(agents_30to50)
      else NA
    },
    plot_type="line_raw",ymin="data",description="")
  
  popsumm_fxns$"mean_degree_over_50"<-
    list(model_value_fxn   = function(...){
      if (length(agents_over50) > 0)
        sum(edges_over50)/length(agents_over50)
      else NA
    },
    plot_type="line_raw",ymin="data",description="")
  
  num_generic_attrs <- length(generic_nodal_att_values)
  if (num_generic_attrs >= 2) {
    popsumm_fxns$"mean_degree_grp1"<-
      list(model_value_fxn   = function(...){
        if (tot_grp1 > 0)
          sum(edges_grp1)/tot_grp1
        else NA
      },
      plot_type="line_raw",ymin="data",description="")
    
    popsumm_fxns$"mean_degree_grp2"<-
      list(model_value_fxn   = function(...){
        if (tot_grp2 > 0)
          sum(edges_grp2)/tot_grp2
        else NA
      },
      plot_type="line_raw",ymin="data",description="")
  }
  if (num_generic_attrs >= 3) {
    popsumm_fxns$"mean_degree_grp3"<-
      list(model_value_fxn   = function(...){
        if (tot_grp3 > 0)
          sum(edges_grp3)/tot_grp3
        else NA
      },
      plot_type="line_raw",ymin="data",description="")
  }
  
  if (num_generic_attrs >= 4) {
    popsumm_fxns$"mean_degree_grp4"<-
      list(model_value_fxn   = function(...){
        if (tot_grp4 > 0)
          sum(edges_grp4)/tot_grp4
        else NA
      },
      plot_type="line_raw",ymin="data",description="")
  }
  
  if (num_generic_attrs >= 5) {
    popsumm_fxns$"mean_degree_grp5"<-
      list(model_value_fxn   = function(...){
        if (tot_grp5 > 0)
          sum(edges_grp5)/tot_grp5
        else NA
      },
      plot_type="line_raw",ymin="data",description="")
  }
  
  if (params$start_prep_campaign < 5e5) {   
    popsumm_fxns$"prop_on_prep" <-
      list(model_value_fxn = function(...) {prop_on_prep},
           plot_type = "line_raw", description = "Proportion on PrEP")}
  
  if(aim3){
    
    popsumm_fxns$"total_new_infections"<-
      list(model_value_fxn   = function(...){
        sum(c(0,dat$popsumm$new_infections[1:popsumm_index]))},
        plot_type="line_raw",overlay="total_1+_drug_muts",
        overlay2="total_3+_drug_muts",description="")
    
    popsumm_fxns$"new_infections_drug_sens_virus"<-
      list(model_value_fxn   = function(...){ new_infections_virus_drug_sens_count},
           plot_type="line_cumul",description="")
    
    popsumm_fxns$"new_infections_drug_part_res_virus"<-
      list(model_value_fxn   = function(...){ new_infections_virus_drug_part_res_count},
           plot_type="line_cumul",description="")
    
    popsumm_fxns$"new_infections_drug_3_plus_res_virus"<-
      list(model_value_fxn   = function(...){ new_infections_virus_drug_3_plus_res_count},
           plot_type="line_cumul",description="")
    
    popsumm_fxns$"mean_PPP_incident"<-
      list(model_value_fxn   = function(...){mean(dat$pop$PPP[which(new_infections)])},
           plot_type="points",loess=T,description="")
    
    popsumm_fxns$"mean_PPP_infected"<-
      list(model_value_fxn   = function(...){mean(dat$pop$PPP[which(inf_index)])},
           plot_type="line_raw",ymin="data",description="")
    
    popsumm_fxns$"drug_muts_1+"<-
      list(model_value_fxn   = function(...){mutations1},
           plot_type="line_raw",ymin="data",description="")
    
    
    popsumm_fxns$"drug_muts_3+"<-
      list(model_value_fxn   = function(...){mutations3},
           plot_type="line_raw",ymin="data",description="")
    
    popsumm_fxns$"total_1+_drug_muts"<-
      list(model_value_fxn   = function(...){
        sum(c(0,dat$popsumm[["new_infections_virus_1_drug_muts"]][1:popsumm_index]))},
        plot_type="line_raw",ymin="data",description="")
    
    
    popsumm_fxns$"total_3+_drug_muts"<-
      list(model_value_fxn   = function(...){
        sum(c(0,dat$popsumm[["new_infections_virus_drug_3_plus_res_count"]][1:popsumm_index]))},
        plot_type="line_raw",ymin="data",description="")
    
    popsumm_fxns$"Perc_0_drug_muts"<-
      list(model_value_fxn   = function(...){mutations0/no_inf_undect},
           plot_type="line_raw",ymin="data",description="")
    
    popsumm_fxns$"Perc_1+_drug_muts"<-
      list(model_value_fxn   = function(...){mutations1/no_inf_undect},
           plot_type="line_raw",ymin="data",overlay="Perc_1_drug_muts")
    
    popsumm_fxns$"Perc_2+_drug_muts"<-
      list(model_value_fxn   = function(...){mutations2/no_inf_undect},
           plot_type="line_raw",ymin="data",overlay="Perc_2_drug_muts",description="")
    
    popsumm_fxns$"Perc_3+_drug_muts"<-
      list(model_value_fxn   = function(...){mutations3/no_inf_undect},
           plot_type="line_raw",ymin="data",overlay="Perc_3_drug_muts",description="")
    
    popsumm_fxns$"Perc_4+_drug_muts"<-
      list(model_value_fxn   = function(...){mutations4/no_inf_undect},
           plot_type="line_raw",ymin="data",overlay="Perc_4_drug_muts",description="")
    
    popsumm_fxns$"Perc_All_5_drug_muts"<-
      list(model_value_fxn   = function(...){mutations5/no_inf_undect},
           plot_type="line_raw",ymin="data",description="")
    
    
    popsumm_fxns$"Perc_1_drug_muts"<-
      list(model_value_fxn   = function(...){mutations1exact/no_inf_undect},
           plot_type="line_raw",ymin="data",description="")
    
    
    popsumm_fxns$"Perc_2_drug_muts"<-
      list(model_value_fxn   = function(...){mutations2exact/no_inf_undect},
           plot_type="line_raw",ymin="data",description="")
    
    popsumm_fxns$"Perc_3_drug_muts"<-
      list(model_value_fxn   = function(...){mutations3exact/no_inf_undect},
           plot_type="line_raw",ymin="data",description="")
    
    popsumm_fxns$"Perc_4_drug_muts"<-
      list(model_value_fxn   = function(...){mutations4exact/no_inf_undect},
           plot_type="line_raw",ymin="data",description="")
    
    
    #not graphed/overlay  only
    popsumm_fxns$"Perc_1_drug_muts_total_pop"<-
      list(model_value_fxn   = function(...){mutations1all/total_alive},
           plot_type="line_raw",ymin="data",description="",description2="")
    
    #not graphed/overlay  only
    popsumm_fxns$"Perc_2_drug_muts_total_pop"<-
      list(model_value_fxn   = function(...){mutations2all/total_alive},
           plot_type="line_raw",ymin="data",description="",description2="")
    
    #not graphed/overlay  only
    popsumm_fxns$"Perc_3_drug_muts_total_pop"<-
      list(model_value_fxn   = function(...){mutations3all/total_alive},
           plot_type="line_raw",ymin="data",description="",description2="")
    #not graphed/overlay  only
    popsumm_fxns$"Perc_4_drug_muts_total_pop"<-
      list(model_value_fxn   = function(...){mutations4all/total_alive},
           plot_type="line_raw",ymin="data",description="",description2="")
    
    popsumm_fxns$"Perc_0_drug_muts_total_pop"<-
      list(model_value_fxn   = function(...){mutations0all/total_alive},
           plot_type="line_raw",ymin="data",description="",
           description2="percent infected with zero mutations \n(denom.= total population)")
    
    popsumm_fxns$"Perc_1+_drug_muts_total_pop"<-
      list(model_value_fxn   = function(...){mutations1plusall/total_alive},
           plot_type="line_raw",ymin="data",overlay="Perc_1_drug_muts_total_pop",
           description="",
           description2="percent infected with 1 and 1+ mutations \n(denom.= total population)")
    
    popsumm_fxns$"Perc_2+_drug_muts_total_pop"<-
      list(model_value_fxn   = function(...){mutations2plusall/total_alive},
           plot_type="line_raw",ymin="data",overlay="Perc_2_drug_muts_total_pop",description="",
           description2="")
    
    popsumm_fxns$"Perc_3+_drug_muts_total_pop"<-
      list(model_value_fxn   = function(...){mutations3plusall/total_alive},
           plot_type="line_raw",ymin="data",overlay="Perc_3_drug_muts_total_pop",description="",
           description2="")
    
    popsumm_fxns$"Perc_4+_drug_muts_total_pop"<-
      list(model_value_fxn   = function(...){mutations4plusall/total_alive},
           plot_type="line_raw",ymin="data",overlay="Perc_4_drug_muts_total_pop",description="",
           description2="")
    
    popsumm_fxns$"Perc_All_5_drug_muts_total_pop"<-
      list(model_value_fxn   = function(...){mutations5all/total_alive},
           plot_type="line_raw",ymin="data",description="",
           description2="")
    
    
    
    popsumm_fxns$"Perc_3+_drug_muts_long"<-
      list(model_value_fxn   = function(...){mutations3plus_long/total_inf},
           plot_type="line_raw",ymin="data",description="")
    
    popsumm_fxns$"Perc_4+_drug_muts_long"<-
      list(model_value_fxn   = function(...){mutations4plus_long/total_inf},
           plot_type="line_raw",ymin="data",description="")
    
    popsumm_fxns$"Perc_5_drug_muts_long"<-
      list(model_value_fxn   = function(...){mutations5_long/total_inf},
           plot_type="line_raw",ymin="data",description="")
    
  }#end of aim3 summary stats
  
  if(length(generic_nodal_att_values)>1){
    for(zz in 1:length(generic_nodal_att_values))
    {
      
      namevec=paste("generic_att_percent_cat_",zz,sep="")
      popsumm_fxns[[namevec ]]=list(model_value_fxn=function(){NA_real_},
                                    plot_type="line_raw",description="")
      namevec=paste("generic_att_percent_inf_cat_",zz,sep="")
      popsumm_fxns[[namevec ]]=list(model_value_fxn=function(){NA_real_},
                                    plot_type="line_raw",description="")
      namevec=paste("generic_att_percent_vacc_cat_",zz,sep="")
      popsumm_fxns[[namevec]]=list(model_value_fxn=function(){NA_real_},
                                   plot_type="line_raw",description="")
      
    }  
  }
  
  #template
  #popsumm_fxns$""<-
  #  list(initial_value_fxn = function(...){},
  #       model_value_fxn   = function(...){})
  
  return(popsumm_fxns)
}

#-------------------------------------------------
#' @export
summary_popsumm_generic_att_fxns<-function(generic_att_values,fxn_list)
{
  
  namevec<- paste("percent_popn_att_",1:length(generic_nodal_att_values),sep="")
  
  for(zz in 1:length(generic_nodal_att_values))
  {
    ww=zz
    temp_fxn=function(ww){ 
      force(ww) 
      return(function(...){length(which(dat$pop$att1==ww))/
          length(which(alive_index))} )
    }
    
    fxn_list[[namevec[zz] ]]=list(model_value_fxn=temp_fxn(zz),plot_type="line_raw")
  }
  return(fxn_list)
}
