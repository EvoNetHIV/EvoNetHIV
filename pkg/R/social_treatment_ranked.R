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
social_treatment_ranked <- function(dat, at)
{
  #Description: 
  #  Determines which infected, diagnosed, eligible-for-care agents who aren't already being treated get treatment
  #  following the start of the treatment campaign
  
  # Algorithm:
  #   Makes "drug availability" a fixed proportion of those eligible for therapy the start of the treatment campaign     
  #   If the number of eligible patients exceeds "drug availability", use one of several allocation strategies        
  #   Current strategies are called: "VL3","VL4","fifo","CD42","generic_attr","random", "MS1", "MS2"
  
  #Inputs: 
    #param$start_treatment_campaign
    #pop$treated
    #pop$Status
    #pop$diag_status
    #param$tx_type
    #param$max_treated
    #param$tx_in_acute_phase
 #Output:
    #pop$treated
    #pop$tx_init_time
  
  ############################################################################################################
  # Determine agents who are newly eligible for treatment                                                    #
  # Criteria: HIV+, diagnosed, eligible for care, and not already treated                                    #
  # Note: This algorithm assumes that those already being treated stay on treatment for life                 #
  ############################################################################################################
  
    newly_eligible <- which(dat$pop$Status == 1 & dat$pop$treated == 0 &
                            dat$pop$diag_status == 1 & dat$pop$eligible_care == 1 ) 
    # If tx_in_acute_phase == FALSE, restrict eligibility to chronically infected  
    if (dat$param$tx_in_acute_phase == FALSE) { 
      newly_eligible <- newly_eligible[which((at - dat$pop$Time_Inf[newly_eligible]) > dat$param$t_acute)]
    }
    if ( length(newly_eligible)==0 ) { return(dat) } # Exit if no eligible agents
  
  ############################################################################################################
  # Calculate the number currently on therapy                                                                #
  ############################################################################################################
    no_on_tx <- length(which(dat$pop$treated==1 & dat$pop$Status==1))   # Figure the number currently on therapy
    total_drugs_available = dat$param$max_treated # Internal representation for clarity
    if ( no_on_tx >= total_drugs_available ) { return(dat) } # Exit (no new tx) if treatment limit is exceeded

  ############################################################################################################
  # Determine if the current timestep represents the start of the treatment campaign                         #
  # If so, make "availability" proportional to those eligible for therapy the start of the campaign           #
  # The nummber starting tx will then be the difference between availability and the no currently on therapy  #
  ############################################################################################################
  
    if (at>=dat$param$start_treatment_campaign & is.na(dat$param$total_drugs_available)) { # is.na part flags campaign start
       total_drugs_available <- trunc(dat$param$proportion_treated*length(newly_eligible))
       param$max_treated = total_drugs_available # External representation for compatibility with other function
    }
    if ( dat$param$total_drugs_available==0 ) {return(dat)}
    if ( at < dat$param$start_treatment_campaign ) { return(dat) }
    new_drugs_available <- total_drugs_available - no_on_tx 
  
  ############################################################################################################
  # Exit if unrecognized treatment option                                                                    #
  ############################################################################################################
    if ( (dat$param$tx_type %in% c("VL3","VL4","fifo","CD42","generic_attr","random","MS1","MS2")) == FALSE) {
      cat("Warning: Unrecongized tx_type in social_treatment_john (",dat$param$tx_type,") -- Exiting program\n")
      at = dat$param$n_steps # Cause program to halt
      return(dat)
    }

  ############################################################################################################
  # Case 1 -- Enough drugs to treat all newly eligible agents                                                #
  ############################################################################################################
    if (length(newly_eligible) <= new_drugs_available) { 
      newly_eligible_receive_tx <- newly_eligible
    }
  
  ############################################################################################################
  # Case 2 -- Not enough drugs to treat all newly eligible agents                                            #
  #           We need to rank agents using one of several drug allocation strategies                         #
  ############################################################################################################
  
   if (length(newly_eligible) > new_drugs_available) { # 
 
     # Allocate drug to patients with the highest viral loads
     if (dat$param$tx_type=="VL3" ){ 
        rank_newly_eligible <- rank(-dat$pop$V[newly_eligible],ties.method="random")
                              # Note: The rank function ranks by the lowest value.
                              #       Ranking by negative viral load prioritizes those with the highest VLs for therapy
        newly_eligible_receive_tx <- newly_eligible[which(rank_newly_eligible<= new_drugs_available)]
      }
     
      # Allocate drug to patients with the lowest viral loads (probably a bad strategy)
      if (dat$param$tx_type=="VL4" ) {
        rank_newly_eligible <- rank(dat$pop$V[newly_eligible],ties.method="random")
        newly_eligible_receive_tx <- newly_eligible[which(rank_newly_eligible<= new_drugs_available)]
      }
      
      # First-in, first-out.  Allocate drug those who were diagnosed the longest time ago
      if (dat$param$tx_type=="fifo" ) { 
        Time_Infected <- at - dat$pop$diag_time[newly_eligible]
        rank_newly_eligible <- rank(-Time_Infected,ties.method="random")
                              # Note: The rank function ranks by the lowest value.
                              #       Ranking by negative Time_Infected prioritizes those who were infected the longest for therapy
        newly_eligible_receive_tx <- newly_eligible[which(rank_newly_eligible<= new_drugs_available)]    
      }
      
      # Allocate drug to agents with the highest CD4 category (= lowest CD4 T-cell count)
      if (dat$param$tx_type=="CD42" ) { 
        rank_newly_eligible <- rank(-dat$pop$CD4[newly_eligible],ties.method="random")
                              # Note: The rank function ranks by the lowest value.
                              #       Ranking by negative CD4 prioritizes agents in CD4 category 4 (i.e., those in AIDS) for therapy
        newly_eligible_receive_tx <- newly_eligible[which(rank_newly_eligible<= new_drugs_available)]
      }
     
      # Allocate drug to the most connected agents (note: lower number means more connected)
      if (dat$param$tx_type=="generic_attr" ) { 
        rank_newly_eligible <- rank(dat$pop$att1[newly_eligible],ties.method="random")
                               # Note: This new version prioritizes those in group 1 for therapy 
        newly_eligible_receive_tx <- newly_eligible[which(rank_newly_eligible<= new_drugs_available)]    
      }
      
     # Allocate drug to highly connected agents with high viral loads
      if (dat$param$tx_type=="MS1" ) {  
        rank_newly_eligible <- rank(dat$pop$att1[newly_eligible] - log10(dat$pop$V[newly_eligible]), ties.method="random")
                               # Notes: Because the rank function ranks by the lowest value, this prioritizes those in risk
                               #        group 1 (the most connected in John's newest scripts) and those with high VLs for therapy
        newly_eligible_receive_tx <- newly_eligible[which(rank_newly_eligible<= new_drugs_available)]    
      }
     
      # Allocate drug to highly connected agents with high viral loads (using a slightly different formula)
      if (dat$param$tx_type=="MS2" ) {
        rank_newly_eligible <- rank(dat$pop$att1[newly_eligible] - 0.3333*log10(dat$pop$V[newly_eligible]), ties.method="random")
        newly_eligible_receive_tx <- newly_eligible[which(rank_newly_eligible<= new_drugs_available)]    
      }
    
      # Random sampling
      if (dat$param$tx_type=="random") {
        newly_eligible_receive_tx <- sample(newly_eligible,new_drugs_available)
      }
   
   } # end sub-sampling
  
  dat$pop$treated[newly_eligible_receive_tx] <- 1
  dat$pop$tx_init_time[newly_eligible_receive_tx] <- at
     
  return(dat)
}
