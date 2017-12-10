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
viral_update_cd4_diff_eqn <- function(dat,at)
{  
  # Description:  SKYE IS EDITING THIS TO CREATE ANOTHER CONFLICT EXAMPLE
  #   Uses a difference equation to determine CD4 counts 
  #   Dynamics of CD4 is a balance between homostatic additions and virological killing
  #   CD4-count (observed in patient) is a function of CD4tot (total CD4 in blood and lymph)
  #   CD4 counts are converted to CD4 categories at the end for compatibility with other routines
  #   CD4 category 5 (death) is defined by a new parameter: CD4count_end_stage 

  # Inputs:
  #    dat$pop: V
  #    dat$param: cd4_homo_input, k_cd4, vl_kill_cd4, min_prop_blood, V_half_redist, CD4count_end_stage
  # Outputs: 
  #   dat$pop: CD4count, CD4tot, CD4, RandomTimeToAIDS, CD4_time_death
  
  #index of alive infectees
    infectees <- which(dat$pop$Status==1)
      
  # Homostatic term: Body makes additional CD4 T-cells if count is less than 1000
  # In later versions, we can make this input term a (decreasing) function of age
    dat$pop$CD4tot[which(dat$pop$CD4tot[infectees] < 1000)] <- dat$pop$CD4tot[which(dat$pop$CD4tot[infectees] < 1000)] 
           + dat$param$cd4_homo_input
  
    inf_min_vl <- which(dat$param$k_cd4*log10(dat$pop$V) > dat$param$vl_kill_cd4 & dat$pop$Status ==1)
  # Virological killing of CD4 T-cells (for agents with a VL greater than the minimum for CD4 killing)
  #   includes offset for each viruses per-pathogen pathogenecity (PPP) PPPs greater than 1.0 kill CD4 T-cells at an enhanced rate.
 
    dat$pop$CD4tot[inf_min_vl] <- dat$pop$CD4tot[inf_min_vl] -
                dat$param$k_cd4 * dat$pop$PPP[inf_min_vl] * log10(dat$pop$V[inf_min_vl]) + dat$param$vl_kill_cd4
                # Last term defines the floor (so we don't get negative killing for very low VLs)
  
  # Dissallow zero or negative numbers for CD4tot (possible due to roundoff errors)
    dat$pop$CD4tot[which(dat$pop$CD4tot <= 0.0)] = 0.1

  # CD4 that is visible in blood (CD4count) is affected by redistribution btw blood and lymph
    dat$pop$CD4count[infectees] <- dat$pop$CD4tot[infectees]  *
                                   (dat$param$min_prop_blood +
                                    (1-dat$param$min_prop_blood) * exp(-dat$pop$V[infectees]/dat$param$V_half_redist))

  # Dissallow zero or negative numbers for CD4count (ossible due to roundoff errors)
    dat$pop$CD4count[which(dat$pop$CD4count <= 0.0)] = 0.1
 
  # Derive categorical equivalents for compability with other evonet functions
    oldCD4 <- dat$pop$CD4
    dat$pop$CD4[which(dat$pop$CD4count >= 500)] = 1
    dat$pop$CD4[which(dat$pop$CD4count >= 350 & dat$pop$CD4count < 500)] = 2
    dat$pop$CD4[which(dat$pop$CD4count >= 200 & dat$pop$CD4count < 350)] = 3
    dat$pop$CD4[which(dat$pop$CD4count >= dat$param$CD4count_end_stage & dat$pop$CD4count < 200)] = 4
    dat$pop$CD4[which(dat$pop$CD4count <  dat$param$CD4count_end_stage)]  = 5
   
  # Record times that patients progress to AIDS (for use in other routines)
    new_aids_index <- which(dat$pop$CD4==4 & oldCD4 != 4)
    dat$pop$RandomTimeToAIDS[new_aids_index] <- at
     
  # Record times that patients progress to end-stage AIDS (for use in other routines)
    cd4_dead_final_index <- which(dat$pop$CD4==5 & oldCD4 !=5)
    dat$pop$CD4_time_death[cd4_dead_final_index] <- at      
    if (at == 700) {
      cat("Toggling breakpoint here \n")
    }
    
  return(dat)
}
  
