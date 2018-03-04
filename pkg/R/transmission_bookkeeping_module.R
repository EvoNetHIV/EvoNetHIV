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
########################################################
#' @export
transmission_bookkeeping_module <- function(dat,timeIndex)
{
  
  #Description:
  # for agents newly infecteds in transmission_main_module, fills in all viral related 
  # variables, e.g., initial VL, SPVL, donor's SPVL
  # Input: discord_coital_df, timeIndex, param$V0, pop$ViralContribToLogSP0,
  # param$MutationVariance, param$VarianceLogSP0, param$start_treatment_campaign,
  # param$zero_heritability_tx, popsumm, param$h, pop$Generation, param$disclosure_prob
  # Output:pop$Time_Inf, pop$V, pop$treated, pop$ViralContribToLogSP0, 
  # pop$EnvirContribToLogSP0, pop$LogSetPoint, pop$SetPoint, pop$d_acute, 
  # pop$RandomTimeToAIDS, pop$Generation, pop$Status, pop$Donors_ViralContribToLogSP0,
  # pop$Donors_EnvirContribToLogSP0, pop$Donors_d_acute, pop$Donors_Total_Time_Inf_At_Trans,
  # pop$Donors_V, pop$Donors_Generation, pop$Donors_SetPoint, pop$Donors_LogSetPoint,
  # pop$Donors_Index, pop$Donors_age, pop$Donors_diag_status, pop$Donors_treated,
  # pop$NumRecipients, pop$disclosure_status, attr$status_evo, attr$status,
  # popsumm$mean_spvl_incident, popsumm$median_spvl_incident, popsumm$variance_spvl_incident
  
  #####################################
  #if no new infections occur, stop fxn
  if(is.null(dat$discord_coital_df)){return(dat)}
  index <- which(dat$discord_coital_df$infection==1)
  if(length(index)==0){return(dat)}
  ######################################
  
  #agent ids of newly infected agents and their infector
  recipient <- dat$discord_coital_df$sus_id[index]
  infector <- dat$discord_coital_df$inf_id[index]
  
  dat$pop$Time_Inf[recipient] <- timeIndex
  dat$pop$Time_Inf_Adj[recipient] <- timeIndex
  dat$pop$V[recipient] <- dat$param$V0
  
  # If treatment provision is based on "time_dist", assign agent-specific min time to trtmnt
  if(!is.logical(dat$param$tx_type)){
   if(dat$param$tx_type == "time_dist" | dat$param$tx_type == "cd4_and_time_dist") {
    dat$pop$min_time_tx[recipient] <- rnorm(length(index), dat$param$mean_time_tx, dat$param$sd_time_tx)
  }
  }
  

  #Patient inherits previous patient's virus + mutational deviation
  variance_vals<-rnorm(length(recipient),
                mean=0,sd=(dat$param$MutationVariance))
  
  dat$pop$ViralContribToLogSP0[recipient] <- (
        dat$pop$ViralContribToLogSP0[infector] + variance_vals)

  #Environmental component is independent
  dat$pop$EnvirContribToLogSP0[recipient] <- rnorm(length(recipient),
      mean=0, sd=sqrt((1-dat$param$h^2)*dat$param$VarianceLogSP0))
  
  #calculate spvl and constrain it to allowable limits
  temp_spvl <-  (dat$pop$ViralContribToLogSP0[recipient] +
                dat$pop$EnvirContribToLogSP0[recipient])
  if(any(temp_spvl< dat$param$min_spvl_allowed))
    temp_spvl[temp_spvl< dat$param$min_spvl_allowed] <- dat$param$min_spvl_allowed
  if(any(temp_spvl> dat$param$max_spvl_allowed))
    temp_spvl[temp_spvl> dat$param$max_spvl_allowed] <- dat$param$max_spvl_allowed
  
  
  dat$pop$LogSetPoint[recipient] <- temp_spvl
  
  dat$pop$SetPoint[recipient] <- "^"(10.0,dat$pop$LogSetPoint[recipient])

  dat$pop$vl_peak_agent[recipient] <-  "^"(10.0,4.639 + 0.495*dat$pop$LogSetPoint[recipient])
  
  if(dat$param$vl_peak_agent_flag){
    vl_peak <-dat$pop$vl_peak_agent[recipient] 
    dat$pop$r0[recipient] <- (log(vl_peak / dat$param$V0) /dat$param$t_peak)
  }else{
    vl_peak <- dat$param$vl_peak_acute
  }
  
  
   dat$pop$vl_phase2_trans[recipient] = exp((2.5*log(dat$pop$SetPoint[recipient])+
                                              log(vl_peak ))/3.5)
  
   dat$pop$rate_phase2[recipient] = -1*(log(dat$pop$vl_phase2_trans[recipient]/dat$pop$SetPoint[recipient])/
                                 (dat$param$t_acute - dat$param$t_acute_phase2))
  
  dat$pop$d_acute[recipient] <- (log(vl_peak /
                                     dat$pop$vl_phase2_trans[recipient]) /
                                 (dat$param$t_acute_phase2- dat$param$t_peak))                      
  
  # Recipients inherit their donor's viruses per pathogen pathogenecity (assume 100% heritable for now)
  dat$pop$PPP[recipient] <- dat$pop$PPP[infector]
  
  ExpectedDelayTime <- ( dat$param$Dmax*"^"(dat$param$D50,dat$param$Dk)/
                        ("^"(dat$pop$SetPoint[recipient],dat$param$Dk) +
                         "^"(dat$param$D50,dat$param$Dk)) )
  
  theta <- ExpectedDelayTime/dat$param$shape_parameter
  
  gamma_vec <- rgamma( length(recipient), dat$param$shape_parameter, 1/theta)
  
  dat$pop$RandomTimeToAIDS[recipient] <- round(dat$param$t_acute + gamma_vec)
  
  dat$pop$Generation[recipient] <- dat$pop$Generation[infector] + 1
  dat$pop$Status[recipient] <- 1
  
  # dat$pop$Donors_ViralContribToLogSP0[recipient] <- dat$pop$ViralContribToLogSP0[infector]
  # dat$pop$Donors_EnvirContribToLogSP0[recipient] <- dat$pop$EnvirContribToLogSP0[infector]
  # dat$pop$Donors_d_acute[recipient] <- dat$pop$d_acute[infector]
  # dat$pop$Donors_Total_Time_Inf_At_Trans[recipient] <- timeIndex - dat$pop$Time_Inf[infector]
  # dat$pop$Donors_V[recipient] <- dat$pop$V[infector]
  # dat$pop$Donors_Generation[recipient] <- dat$pop$Generation[infector]
  # dat$pop$Donors_SetPoint[recipient] <- dat$pop$SetPoint[infector]
  # dat$pop$Donors_LogSetPoint[recipient] <- dat$pop$LogSetPoint[infector]
  # dat$pop$Donors_Index[recipient] <-   infector
  # dat$pop$Donors_age[recipient] <-   dat$pop$age[infector]
  dat$pop$NumRecipients[infector] <- dat$pop$NumRecipients[infector] + 1
  dat$pop$virus_sens_vacc[recipient] <- dat$pop$virus_sens_vacc[infector]
  # dat$pop$Donors_age[recipient] <- dat$pop$age[infector]
  dat$pop$age_infection[recipient] <- dat$pop$age[recipient]
  
  
  #############################################
  
  #save recipient / infector info if desired
  if(dat$param$save_infection_matrix){
    dat$InfMat[[timeIndex]] <- cbind(timeIndex,recipient,  infector)
  }
  
  #fill in EpiModel's "status" vector
  temp_match <- match(dat$attr$id,1:length(dat$pop$Status))
  dat$attr$status_evo <- dat$pop$Status[temp_match]
  temp_which <- which(dat$pop$Status==1)
  temp_which2 <- which(is.element(dat$attr$id,temp_which))
  dat$attr$status[temp_which2] <- "i"
  
  #summary stats of spvl for newly infecteds for timestep 
  #dat$popsumm$mean_spvl_incident[timeIndex]  <- mean(dat$pop$LogSetPoint[recipient])
  #dat$popsumm$median_spvl_incident[timeIndex] <- median(dat$pop$LogSetPoint[recipient])
  #dat$popsumm$variance_spvl_incident[timeIndex] <- var(dat$pop$LogSetPoint[recipient])
  
  return(dat)
}
##########################################
