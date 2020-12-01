#' @title Initialize CD4 values
#'
#'
#' @param dat master data object
#' @param at timestep
#' @return 'dat' object with agent attributes for CD4 values calculated
#' @details
#' Subfunction in 'initialize_module'. Fills in initial CD4 and associated values for initial infecteds; must be run after initial_infecteds_cd4 as SPVL values are required.
#' @examples
#' dat <- initialize_infecteds_cd4(dat,at=1)

#' @export
initialize_infecteds_cd4 <- function(dat,at)
{
 ####################################
  # called in "initialize_module" 
  # fills in initial CD4 and associated values for initial infecteds
  # must be run after initial_infecteds_vl as needs VL values to run
  # assigns initial cd4 values based on initial spvl (probabilistically)
  # helper fxns called:  viral_spvl_cat_fxn, viral_initialCD4
  #input parameters: pop$LogSetPoint, pop$cd4_time_aids_matrix, pop$Time_Inf
  #output: CD4,CD4_initial_value,CD4_time_to_aids,pop$CD4_time,CD4_treatment_delay_index
  #         CD4_TimeToAIDS_exp_cat1,CD4_TimeToAIDS_exp_cat2,CD4_TimeToAIDS_exp_cat3,CD4_TimeToAIDS_exp_cat4
##########################################

  param <-  dat$param 
  pop   <- dat$attr
  timeIndex  <- at
  
  #EpiModel deteremines infected status in init_status.net and puts it in "dat$attr$status
  #ind represents infected individuals of initial population
  ind <- which(dat$attr$status=="i")

  #assigns categorical spvl value
  pop$spvl_cat[ind] <- viral_spvl_cat_fxn(pop$LogSetPoint[ind])
  #assings cd4 value based on spvl category
  pop$CD4[ind] <-  viral_initialCD4(pop$spvl_cat[ind], plist=param );

  pop$CD4count[ind] <- 650 - 120*(pop$CD4[ind]-1) # Rough: 1->530, 2->410, 3-->290, 4->170, 5->50  
  pop$CD4tot[ind] <- pop$CD4count[ind] 
  pop$CD4_initial_value[ind] <- pop$CD4[ind]
  pop$CD4_nadir[ind] <- pop$CD4[ind]
  pop$CD4_time[ind] <- (-pop$Time_Inf[ind])
  pop$CD4_treatment_delay_index[ind] <- (-pop$Time_Inf[ind])

 dat$attr <- pop
  return(dat)
  }
###################################################
