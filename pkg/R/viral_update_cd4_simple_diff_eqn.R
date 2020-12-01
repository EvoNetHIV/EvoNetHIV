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
viral_update_cd4_simple_diff_eqn <- function(dat,at)
{  
  # Description:
  #   Uses a difference equation to determine CD4 counts 
  #   CD4 counts drop as the square root of log VL
  #
  # Inputs:
  #    dat$pop: V
  #    dat$param: CD4count_end_stage
  # Outputs: 
  #   dat$pop: CD4count, CD4, RandomTimeToAIDS, CD4_time_death
  #   
  #  ** The following constants have been hard-coded into the function for the moment **
  #      - Rate at which CD4 declines with log VL (hard-coded to be 0.045)
  #      - Minimal VL below which CD4 stops declining (hard-coded to be 1.0)
  #
  #  ** This version is NOT designed to work with treatment **  
  
   
   #   Define population with VL greater than a pre-specified mininum (1.) for CD4 killing
   #   Added b/c logV will be negative for VL's < 1 ==> VL-induced increases in CD4's!
   #   Fortunately, a VL < 1 is so low that we hardly have to worry about time to death anyway
    inf_min_vl <- which(dat$attr$V > 1.0 & dat$attr$Status ==1)
    
   # Virological killing of CD4 T-cells (for agents with a VL greater than the minimum for CD4 killing)
   #   PPP is a multiplier (default = 1) that allows some viral genotypes to kill their hosts faster than expected from the VL.
    dat$attr$CD4count[inf_min_vl] <- dat$attr$CD4count[inf_min_vl] -  0.045*dat$attr$PPP[inf_min_vl]*log10(dat$attr$V[inf_min_vl])
      
    
   # Dissallow zero or negative numbers for CD4count (possible due to roundoff errors)
    dat$attr$CD4count[which(dat$attr$CD4count <= 0.0)] = 0.1
  
   # Derive categorical equivalents for compability with other evonet functions
    oldCD4 <- dat$attr$CD4
    dat$attr$CD4[which(dat$attr$CD4count >= 500)] = 1
    dat$attr$CD4[which(dat$attr$CD4count >= 350 & dat$attr$CD4count < 500)] = 2
    dat$attr$CD4[which(dat$attr$CD4count >= 200 & dat$attr$CD4count < 350)] = 3
    dat$attr$CD4[which(dat$attr$CD4count >= dat$param$CD4count_end_stage & dat$attr$CD4count < 200)] = 4
    dat$attr$CD4[which(dat$attr$CD4count <  dat$param$CD4count_end_stage)]  = 5
   
  # Record times that patients progress to AIDS (for use in other routines)
    new_aids_index <- which(dat$attr$CD4==4 & oldCD4 != 4)
    dat$attr$RandomTimeToAIDS[new_aids_index] <- at
     
  # Record times that patients progress to end-stage AIDS (for use in other routines)
    cd4_dead_final_index <- which(dat$attr$CD4==5 & oldCD4 !=5)
    dat$attr$CD4_time_death[cd4_dead_final_index] <- at      
    if (at == 700) {
      cat("Toggling breakpoint here \n")
    }
    
  return(dat)
}
  
