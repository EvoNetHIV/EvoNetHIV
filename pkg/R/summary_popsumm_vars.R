summary_popsumm_vars <- function(dat){

  default_vars <- c("timestep", "prevalence", "new_infections", "susceptibles",
  "total_infections_alive", "births", "aids_deaths", "natural_deaths",
  "aged_out", "natural_deaths_infecteds", "natural_deaths_susceptibles",
  "alive", "no_in_aids_gamma", "no_in_aids_cd4", 
  "new_diagnoses", "percent_donor_acute",
  "mean_time_donor_infected_incident", "mean_age_incident", "mean_age_died_AIDS",
  "mean_spvl_pop_all", "mean_vl_pop_all", "mean_spvl_incident", "mean_spvl_pop_untreated",
  "total_infections_not_treated",
  "mean_age_infecteds", "mean_age_susceptibles", "mean_trans_prob",
  "no_edges", "mean_degree", "mean_degree_inf_untreated", "prop_nodes_degree_0",
  "prop_nodes_degree_1", "prop_nodes_concurrent",
  "cd4_gt_350", "cd4_200_350","cd4_0_200")


 hetero_vars <-  c("alive_female", "alive_male",
  "prev_15to24", "prev_15to49", "prev_f_15to24", "prev_f_15to49",
  "prev_m_15to24", "prev_m_15to49", "inf_men", "inf_women", "inf_under30",
  "inf_30to50", "inf_over50", "mean_degree_under_30",
  "mean_degree_30_50", "mean_degree_over_50","mean_degree_female","mean_degree_male")

  treatment_vars <- c( "no_treated",
  "percent_suppressed")

  hetero_and_treatment_vars <- c("treated_inf_men", "treated_inf_women",
  "treated_inf_under30", "treated_inf_30to50", "treated_inf_over50",
  "no_treated_undetectable", "mean_vl_pop_untreated",
  "percent_treated_undetectable", "total_pills_taken", "mean_degree_inf_treated")

  prep_vars= c("prop_on_prep","prop_eligible_prep")

  circumcision_vars=c("circum_prev")

  vaccine_vars = c("new_infections_vacc_sens_virus",
                     "new_infections_vacc_resist_virus", "percent_virus_sensitive_vacc",
                     "percentAliveVaccinated")
  
  dmv_vaccine_vars = c("mean_spvl_genotype","mean_spvl_nonvacc","mean_spvl_incident_vacc","percentAliveVaccinated",
                       "perc_eligible_vacc")
  
  aim3_vars <-  c("total_new_infections", "new_infections_drug_sens_virus",
  "new_infections_drug_part_res_virus", "new_infections_drug_3_plus_res_virus",
  "mean_PPP_incident", "mean_PPP_infected", "drug_muts_1+", "drug_muts_3+",
  "total_1+_drug_muts", "total_3+_drug_muts", "Perc_0_drug_muts",
  "Perc_1+_drug_muts", "Perc_2+_drug_muts", "Perc_3+_drug_muts",
  "Perc_4+_drug_muts", "Perc_All_5_drug_muts", "Perc_1_drug_muts",
  "Perc_2_drug_muts", "Perc_3_drug_muts", "Perc_4_drug_muts", "Perc_1_drug_muts_total_pop",
  "Perc_2_drug_muts_total_pop", "Perc_3_drug_muts_total_pop", "Perc_4_drug_muts_total_pop",
  "Perc_0_drug_muts_total_pop", "Perc_1+_drug_muts_total_pop",
  "Perc_2+_drug_muts_total_pop", "Perc_3+_drug_muts_total_pop",
  "Perc_4+_drug_muts_total_pop", "Perc_All_5_drug_muts_total_pop",
  "Perc_3+_drug_muts_long", "Perc_4+_drug_muts_long", "Perc_5_drug_muts_long")
  
  genatt_vars <- NULL
  temp_length <- length(dat$param$generic_nodal_att_values)
  if(temp_length>1){
    genatt_var1 <- paste("generic_att_percent_cat_",dat$param$generic_nodal_att_values,sep="")
    genatt_var2 <- paste("generic_att_percent_inf_cat_",dat$param$generic_nodal_att_values,sep="")
    genatt_var2b <- paste("generic_att_mean_degree_cat_",dat$param$generic_nodal_att_values,sep="")
    genatt_var3=NULL
    if (dat$param$perc_vaccinated != 0.5){
      genatt_var3 <- paste("generic_att_percent_vacc_cat_",dat$param$generic_nodal_att_values,sep="")
    }
    genatt_var4 <- paste("prevalence_attr_",dat$param$generic_nodal_att_values,sep="")
    genatt_var5 <- paste("new_infections_attr_",dat$param$generic_nodal_att_values,sep="")
    genatt_var6 <- paste("susceptibles_attr_",dat$param$generic_nodal_att_values,sep="")
    genatt_var7 <- paste("mean_spvl_incident_attr_",dat$param$generic_nodal_att_values,sep="")
    genatt_var8 <- paste("prop_on_prep_attr_",dat$param$generic_nodal_att_values,sep="")
    genatt_var9 <- paste("prop_eligible_prep_attr_",dat$param$generic_nodal_att_values,sep="")
    
    genatt_vars <- c(genatt_var1,genatt_var2,genatt_var2b,genatt_var3,genatt_var4,genatt_var5,genatt_var6,genatt_var7,genatt_var8,genatt_var9)
  }  
  
  
  
  popsumm_vars <- c(default_vars,genatt_vars)
  
  #hetero models
  if(dat$param$model_sex=="hetero"){popsumm_vars <- c(popsumm_vars,hetero_vars)}
  #models with treatment
  if(dat$param$start_treatment_campaign[1] < 5e5){popsumm_vars <- c(popsumm_vars,treatment_vars)}
  #hetero models with treatment
  if (dat$param$model_sex=="hetero" &
      dat$param$start_treatment_campaign[1] < 5e5){
         popsumm_vars <- c(popsumm_vars,hetero_and_treatment_vars)
  }
  #circumcision model
  if(dat$param$circum_prob != 0.85){popsumm_vars <- c(popsumm_vars,circumcision_vars)}
  #prep model
  if(dat$param$start_prep_campaign[1] < 5e5){popsumm_vars <- c(popsumm_vars,prep_vars)}
  #aim 3 model
  if(dat$param$VL_Function=="aim3"){popsumm_vars <- c(popsumm_vars,aim3_vars)}
  #vaccine model
  if (dat$param$preventative_campaign){ popsumm_vars <- c(popsumm_vars,vaccine_vars)}
  #dmv vaccine model
  if(dat$param$vacc_therapeutic_campaign){
    popsumm_vars <- c(popsumm_vars,dmv_vaccine_vars)
  }
    
    return(popsumm_vars)
}
