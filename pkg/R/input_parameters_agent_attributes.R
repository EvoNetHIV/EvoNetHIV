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
    
#--  Virulence model parameters ------------------------------ #

    "s",                               "Status",           
    "NumRecipients",                    
    "ViralContribToLogSP0" ,
    "EnvirContribToLogSP0",            "LogSetPoint",           
    "SetPoint",                        "d_acute",  
    "Generation",                      "RandomTimeToAIDS",
    "Time_Inf",                        "Time_Inf_Adj",      "V",   
    "r0",                              "age_infection",
    "vl_phase2_trans",                 "rate_phase2",
    "PPP",                              "vl_peak_agent",
    "vl_expected",
    # "Donors_V",                        "Donors_treated",
    # "Donors_ViralContribToLogSP0",     "Donors_EnvirContribToLogSP0",
    # "Donors_Total_Time_Inf_At_Trans",  "Donors_Generation",
    # "Donors_Index",                    "Donors_age",                   
    # "Donors_LogSetPoint",              "Donors_SetPoint",
    # "Donors_d_acute",                  
    "Time_Death",
    
    "CD4",                                             
    "CD4_time",                        "CD4_initial_value",
    "spvl_cat",
    "CD4_time_death",                   "CD4_nadir",
    "start_aids_cd4",                   "start_max_aids",
    "virus_sens_vacc",                  
    
#--  Vital dynamics /social/treatment ------------------------------ #
    "vaccinated",                      "vacc_init_time",           
    "age", "age_cat",                  "arrival_time",
    "last_neg_test",                   "diag_status",
    "id",                              "eligible_care",
    "att1",                            "ai_prob",
    "treated",                         "tx_init_time",
    "circum",             
    "sti_status",                       "sex")
    ##############################
  ) #end of  list
  
}