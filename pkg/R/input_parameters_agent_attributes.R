#' @title Title
#'
#' @description Returns vector of individual agent attributes (e.g., 'age', 'sex', 'SPVL') that populate 'pop' data structure. This vector is input to initialize_agents (called in initialize_module) which creates a list with the number of elements equal to the number of agent attributes and the length of each element equal to the size of the initial population.
#' 
#'
#' @return Vector of agent attributes as character string.
#' @details
#' No function arguments.
#' @examples
#' agent_attributes <- input_parameters_agent_attributes()

#' @export
input_parameters_agent_attributes <-function(){
  
  #Description:
  # Vector of the individual agent attributes that populate the “pop” list
  
  list( popVariables= c(
    
    "id",
    "unique_id",
    
#--  Virulence model parameters ------------------------------ #

    "s",                               "Status",           
    "NumRecipients",                    
    "ViralContribToLogSP0" ,
    "EnvirContribToLogSP0",            "LogSetPoint",           
    "SetPoint",                        "d_acute",  
    "Generation",                      "RandomTimeToAIDS",
    "Time_Inf",                        "Time_Inf_Adj",      "V",   
     "r0",                             "age_infection",
    "vl_phase2_trans",                 "rate_phase2",
    "PPP",                              "vl_peak_agent",
    "vl_at_test",                      "cd4_at_test",
    "vl_expected",
    "Donors_V",                        "Donors_treated",  
    "Donors_treated_2nd_line",         "Donors_CD4",
    "Donors_ViralContribToLogSP0",     "Donors_EnvirContribToLogSP0",
    "Donors_Total_Time_Inf_At_Trans",  "Donors_Generation",
    "Donors_Index",                    "Donors_age",                   
    "Donors_LogSetPoint",              "Donors_SetPoint",
    "Donors_d_acute",                  "Time_Death",
    "Donors_diag_status",              
    "K",                               "Imm_Trig", 
    "CD4count",                        "CD4tot",
    "ChronPhase",                      "OnDrug",
    "Adherence1","Adherence2",         "Adherence3","Adherence4",
    "Drug1", "Drug2",                  "Drug3", "Drug4", 
    "aim3_no_muts",                    "adherence_start",
    "adherence_type",                  "CD4",                                             
    "CD4_time",                        "CD4_initial_value",
    "CD4_treatment_delay_index",       "spvl_cat",
    "CD4_time_death",                   "CD4_nadir",
    "CD4_at_trtmnt",                    "num_consec_VL_gt1k",
    "start_aids_cd4",                   "start_max_aids",
    "virus_sens_vacc",                  "virus_sens_drug",
    "virus_part_res_drug",               "virus_1_plus_drug_muts",
    "virus_3_plus_drug_muts",             "Aim3RoundingErrors",               
     "aim3_mutations_long",             "CYP_6_slow",
    
# -- testing for and treatment of drug resistant viruses (aim 3) --- #    
    "eligible_2nd_line_ART",
    "treated_2nd_line",  
    "diag_resist_status",
    "diag_resist_time", 
    "last_neg_resist_test",
    "time_init_2nd_line",

# -- therapeutic vaccine --- #    
   "LogSetPoint_genotype",             "vacc_status_at_inf",
   
#--  Vital dynamics /social/treatment ------------------------------ #
    "individual_condom_prob",          "susceptibility",
    "vaccinated",                      "vacc_init_time",           
    "age","sqrt_age",                   "arrival_time",
    "age_for_age_group_calcs",         "age_group",
    "last_neg_test",                   "diag_status",
    "diag_time",                       "disclosure_status",
    "eligible_ART",                   "eligible_vl_test" ,                                   
    "eligible_care",                   "insert_quotient",
    "att1",                            "ai_prob",
    "treated",                         "tx_init_time",
    "circum",                           "tx_schedule",               
    "sti_status",                       "sex",
    "total_acts",                        "total_disc_acts",                        
     "role",
    "min_time_tx",                      "enhanced_testing",
    "time_hiv_sex_act",                 "time_hiv_sex",
     "last_disc_sex",                   "tx_stop_time",              
    "rand_prob_test",                   "ever_enhanced_testing",
    "rand_prob_test_init",              "tx_dropout",
     "pos_partner_duration",             "no_partners_past_prep",
    "no_partners_now_prep",              "have_diag_partner",
    "have_disclosed_partner",            "on_prep",                      
    "prep_decrease",                     "eligible_for_prep",
    "eligible_for_prep_1",               "eligible_for_prep_2",
    "prep_init_time",                    "prep_discontinue_time",
    "prep_list",                         "known_pos_partner_duration",
    "condom_user",                       "partner_recent_test", 
    "have_suppressed_partner",           "have_unknown_status_partner",
    "agent_condom_user",                 "vacc_eff",
    "vacc_rr",                            "trial_status",
    "last_ts_relationship",              "last_ts_multiple_relationships",
    #vaccination model
   "phi","mu","sigma","theta","m","vaccination.dates.stack")
    ##############################
  ) #end of  list
  
}
