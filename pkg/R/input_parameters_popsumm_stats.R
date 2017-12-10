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
input_parameters_popsumm_stats <- function()
{
 #description:
 #vector of field names for population/network/epidemic statistics calculated each timestep and
 #stored in "popsumm" list
 #this vector processed in "initialize_popsumm_dynamics
 #to view popsumm list after model run for simulation 1: evomodel$popsumm[[1]]
  
  c(  
    "aids_deaths", 
    "natural_deaths",
    "natural_deaths_infecteds",
    "natural_deaths_susceptibles",
    "aged_out",
    "births",
    "new_infections",
    "total_infections",
    "total_infections_not_treated",
    "susceptibles",
    "alive",
    "no_in_aids_gamma",
    "no_in_aids_cd4",
    
    "mean_spvl_pop_untreated",
    "median_spvl_pop_untreated",
    "variance_spvl_pop_untreated",
    
    "mean_spvl_pop_all",
    "median_spvl_pop_all",
    "variance_spvl_pop_all",
    
    "mean_spvl_incident",
    "median_spvl_incident",
    "variance_spvl_incident",

    "mean_vl_pop_untreated",
    "median_vl_pop_untreated",
    "variance_vl_pop_untreated",
    
    "mean_vl_pop_all",
    "median_vl_pop_all",
    "variance_vl_pop_all",
    
    "total_pills_taken",
    
    "mean_age_incident",
    "mean_age_susceptibles",
    "mean_age_infecteds",
    "mean_age_died_AIDS",
    "mean_age_infected_died_natural",
    "mean_age_susceptibles_died_natural",

    "diagnosed",
    "no_treated",

    "no_edges",
    "mean_degree",
    "no_nodes_degree_0",
    "no_nodes_degree_1",
    "no_nodes_concurrent" )
}
