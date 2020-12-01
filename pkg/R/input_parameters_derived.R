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
input_parameters_derived  <- function(initial_param)
{
  #Description:
  # Basic parameters calculated from parameter values in input_parameters_primary().  
  # Make changes to parameter values in input_parameters_primary().
 
  #1st, some basic qaqc
  initial_param$n_steps <- round(abs(initial_param$n_steps))
  initial_param$initial_pop <- round(abs(initial_param$initial_pop))
  initial_param$initial_infected  <- round(abs(initial_param$initial_infected))
  
  #change epimodel param 'modes' to 2 if hetero model, default = 1
  if(initial_param$model_sex!="msm"){initial_param$modes=2}   
  
  if(initial_param$VL_Function=="aim3"){
    initial_param$Max_Allowable_Loci <- 5
  }
  if(initial_param$VL_Function=="aim2"){
    initial_param$Max_Allowable_Loci <- 0
  }
  
  # poisson_birth_lambda, parameter for default birth fxn, is a fxn of popn size
    initial_param$poisson_birth_lambda     = (initial_param$initial_pop/100)* initial_param$poisson_birth_base
  
  
  mort_per_timestep_male = input_parameters_asmr(
    data_name=initial_param$asmr_data_male ,
    initial_param$min_age,initial_param$max_age)
  
  mort_per_timestep_female = input_parameters_asmr(
    data_name=initial_param$asmr_data_female,
    initial_param$min_age,initial_param$max_age)
  
  pop_growth_rate_timestep  = utilities_annual_prob_conversion(
    initial_param$pop_growth_rate_annual,
    365)
  
  male_age_dist = input_parameters_age_distribution(initial_param, mort_per_timestep_female,
                                                    pop_growth_rate_timestep, initial_param$baseline_input_exp_growth,
                                                    data_name=initial_param$initial_agedata_male,
                                                    initial_param$min_age,initial_param$max_age)
  female_age_dist = input_parameters_age_distribution(initial_param, mort_per_timestep_female,
                                                      pop_growth_rate_timestep, initial_param$baseline_input_exp_growth,
                                                      data_name=initial_param$initial_agedata_female,
                                                      initial_param$min_age,initial_param$max_age)
  
derived_params <- list( 
  h     = sqrt(initial_param$Heritability),
  r0    = (log(initial_param$vl_peak_acute / initial_param$V0) /
              initial_param$t_peak),
  pop_growth_rate_timestep = pop_growth_rate_timestep,
  mort_per_timestep_male = mort_per_timestep_male, 
  mort_per_timestep_female = mort_per_timestep_female,
  male_age_dist= male_age_dist,
  female_age_dist = female_age_dist)


final_params <- c(initial_param,derived_params)
#add class so it's recognized as epimodel object
class(final_params) <- "param.net"
return(final_params)
}


