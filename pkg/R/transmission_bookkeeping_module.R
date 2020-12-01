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
  
  dat$attr$Time_Inf[recipient] <- timeIndex
  dat$attr$Time_Inf_Adj[recipient] <- timeIndex
  dat$attr$V[recipient] <- dat$param$V0
  
  # If treatment provision is based on "time_dist", assign agent-specific min time to trtmnt
  if(!is.logical(dat$param$tx_type)){
   if(dat$param$tx_type == "time_dist" | dat$param$tx_type == "cd4_and_time_dist") {
    dat$attr$min_time_tx[recipient] <- rnorm(length(index), dat$param$mean_time_tx, dat$param$sd_time_tx)
  }
  }
  

  #Patient inherits previous patient's virus
  dat$attr$ViralContribToLogSP0[recipient] <- dat$attr$ViralContribToLogSP0[infector] 

  #Environmental component is independent
  dat$attr$EnvirContribToLogSP0[recipient] <- rnorm(length(recipient),
      mean=0, sd=sqrt((1-dat$param$h^2)*dat$param$VarianceLogSP0))
  
  #calculate spvl and constrain it to allowable limits
  temp_spvl <-  (dat$attr$ViralContribToLogSP0[recipient] +
                dat$attr$EnvirContribToLogSP0[recipient])
    temp_spvl[temp_spvl< dat$param$min_spvl_allowed] <- dat$param$min_spvl_allowed
    temp_spvl[temp_spvl> dat$param$max_spvl_allowed] <- dat$param$max_spvl_allowed
  
  
  
    dat$attr$LogSetPoint[recipient] <- temp_spvl
    dat$attr$SetPoint[recipient] <- "^"(10.0,dat$attr$LogSetPoint[recipient])

    
    #therapeutic vaccine dynamics: 
    #NOTE: this section must go after initial calcs for agent's LogSetPoint and SetPoint
    #"actual" spvl is now "LogSetPoint_genotype",
    # while phenotypic spvl is standard "LogSetPoint"
    if(timeIndex >=  dat$param$start_vacc_campaign[1] & dat$param$vacc_therapeutic_campaign==T){
      #agents just got infected and are vaccinated (indexing recipient vector)
      vaccinated <- which(dat$attr$vaccinated[recipient]==1)
      if(length(vaccinated)>0){
        vacc_ix <-  recipient[vaccinated] 
        dat$attr$LogSetPoint_genotype[vacc_ix] <- temp_spvl[vaccinated] 
        
        #temp qaqc check
        if(any(is.na(dat$attr$LogSetPoint_genotype[vacc_ix]))){browser()}
        
        tempv_vacc_spvl <- temp_spvl[vaccinated] - dat$param$spvl_decrement_vaccine
        tempv_vacc_spvl[tempv_vacc_spvl < dat$param$min_spvl_allowed] <- dat$param$min_spvl_allowed
        dat$attr$LogSetPoint[vacc_ix] <- tempv_vacc_spvl
        dat$attr$SetPoint[vacc_ix] <- "^"(10.0,dat$attr$LogSetPoint[vacc_ix])
      }
    }
    
    
    
  dat$attr$vl_peak_agent[recipient] <-  "^"(10.0,4.639 + 0.495*dat$attr$LogSetPoint[recipient])
  
  if(dat$param$vl_peak_agent_flag){
    vl_peak <-dat$attr$vl_peak_agent[recipient] 
    dat$attr$r0[recipient] <- (log(vl_peak / dat$param$V0) /dat$param$t_peak)
  }else{
    vl_peak <- dat$param$vl_peak_acute
  }
  
  
   dat$attr$vl_phase2_trans[recipient] = exp((2.5*log(dat$attr$SetPoint[recipient])+
                                              log(vl_peak ))/3.5)
  
   dat$attr$rate_phase2[recipient] = -1*(log(dat$attr$vl_phase2_trans[recipient]/dat$attr$SetPoint[recipient])/
                                 (dat$param$t_acute - dat$param$t_acute_phase2))
  
  dat$attr$d_acute[recipient] <- (log(vl_peak /
                                     dat$attr$vl_phase2_trans[recipient]) /
                                 (dat$param$t_acute_phase2- dat$param$t_peak))                      
  
  # Recipients inherit their donor's viruses per pathogen pathogenecity (assume 100% heritable for now)
  dat$attr$PPP[recipient] <- dat$attr$PPP[infector]
  
  ExpectedDelayTime <- ( dat$param$Dmax*"^"(dat$param$D50,dat$param$Dk)/
                        ("^"(dat$attr$SetPoint[recipient],dat$param$Dk) +
                         "^"(dat$param$D50,dat$param$Dk)) )
  
  theta <- ExpectedDelayTime/dat$param$shape_parameter
  
  gamma_vec <- rgamma( length(recipient),
            dat$param$shape_parameter, 1/theta)
  
  dat$attr$RandomTimeToAIDS[recipient] <- round(dat$param$t_acute + gamma_vec)
  
  dat$attr$Generation[recipient] <- dat$attr$Generation[infector] + 1
  dat$attr$Status[recipient] <- 1
  
  #switch status of those on prep and became infected to "-1"
  prep_infected <- which(dat$attr$on_prep[recipient]==1)
  if(length(prep_infected)>0){dat$attr$on_prep[recipient[prep_infected]] <- -1}
  
  
  #aim 3 bookkeeping (ask john)
  if (dat$param$VL_Function == "aim3") {

    V_vec_length <- 2^dat$param$Max_Allowable_Loci
    inf_ix <- apply(dat$attr$V_vec[infector,,drop=F],1,function(x) sample(1:V_vec_length,1,prob=x))
    dat$attr$V_vec[recipient,inf_ix] <- dat$param$V0
    dat$attr$I_vec[recipient,inf_ix] = (dat$param$c *dat$attr$V_vec[recipient,inf_ix] / 
                                         dat$param$p_inf_cells)
    dat$attr$M_vec[recipient,] <- 0
    dat$attr$L_vec[recipient,] <- 0
    
    dat$attr$K[recipient] <- vl_peak
    dat$attr$CD4tot[recipient] <- 1000
    dat$attr$CD4count[recipient] <- 1000
    dat$attr$Imm_Trig[recipient] <- 0
    dat$attr$ChronPhase[recipient] <- 0
    #dat$attr$Drug1[recipient] <- 0  # Big assumption here!!! (Only infected patients get drug!)
    #dat$attr$Drug2[recipient] <- 0  
    #dat$attr$Drug3[recipient] <- 0 
    #dat$attr$Drug4[recipient] <- 0 
    dat$attr$Aim3RoundingErrors[recipient] <- 0
    ix1<-which(inf_ix==1)
    v0 <- 1
    v12 <- c(2,3,4,5,6,7,9,10,11,13, 17, 18, 19, 21, 25) 
    v15 <-  2:32
    ix1 <- which(inf_ix %in% v0)
    ix2 <- which(inf_ix %in% v12)
    ix3 <- which(!is.element(inf_ix,c(v0,v12)))
    ix15 <- which(inf_ix %in% v15)
    
      dat$attr$virus_1_plus_drug_muts[recipient[ix1]] <- 0
    
      dat$attr$virus_sens_drug[recipient[ix1]] <- 1 
      dat$attr$virus_part_res_drug[recipient[ix1]] <- 0
      dat$attr$virus_3_plus_drug_muts[recipient[ix1]] <- 0
  
      dat$attr$virus_sens_drug[recipient[ix2]] <- 0 
      dat$attr$virus_part_res_drug[recipient[ix2]] <- 1
      dat$attr$virus_3_plus_drug_muts[recipient[ix2]] <- 0
      
      dat$attr$virus_sens_drug[recipient[ix3]] <- 0 
      dat$attr$virus_part_res_drug[recipient[ix3]] <- 0
      dat$attr$virus_3_plus_drug_muts[recipient[ix3]] <- 1
  }
  
  
  dat$attr$Donors_ViralContribToLogSP0[recipient] <- dat$attr$ViralContribToLogSP0[infector]
  dat$attr$Donors_EnvirContribToLogSP0[recipient] <- dat$attr$EnvirContribToLogSP0[infector]
  dat$attr$Donors_d_acute[recipient] <- dat$attr$d_acute[infector]
  dat$attr$Donors_Total_Time_Inf_At_Trans[recipient] <- timeIndex - dat$attr$Time_Inf[infector]
  dat$attr$Donors_V[recipient] <- dat$attr$V[infector]
  dat$attr$Donors_Generation[recipient] <- dat$attr$Generation[infector]
  dat$attr$Donors_SetPoint[recipient] <- dat$attr$SetPoint[infector]
  dat$attr$Donors_LogSetPoint[recipient] <- dat$attr$LogSetPoint[infector]
  dat$attr$Donors_Index[recipient] <-   infector
  dat$attr$Donors_age[recipient] <-   dat$attr$age[infector]
  dat$attr$Donors_diag_status[recipient] <-   dat$attr$diag_status[infector]
  dat$attr$Donors_treated[recipient] <-   dat$attr$treated[infector]
  dat$attr$Donors_treated_2nd_line[recipient] <-   dat$attr$treated_2nd_line[infector]
  dat$attr$Donors_CD4[recipient] <- dat$attr$CD4[infector] 
  dat$attr$NumRecipients[infector] <- dat$attr$NumRecipients[infector] + 1
  dat$attr$virus_sens_vacc[recipient] <- dat$attr$virus_sens_vacc[infector]
  dat$attr$vacc_eff[recipient] <- dat$attr$vacc_eff[infector]
    dat$attr$Donors_age[recipient] <- dat$attr$age[infector]
  dat$attr$age_infection[recipient] <- dat$attr$age[recipient]
  dat$attr$vacc_status_at_inf[recipient] <- dat$attr$vaccinated[recipient]
  
  #############################################
  
  #save recipient / infector info if desired
  if(dat$param$save_infection_matrix){
    dat$InfMat[[timeIndex]] <- cbind(timeIndex,recipient,  infector)
  }
  
  #assign disclosure status of newly infected (do they tell partner hiv status)
  dat$attr$disclosure_status[recipient][which(runif(length(recipient)) < dat$param$disclosure_prob )] <- 1 #default is zero
  
  #fill in EpiModel's "status" vector
  temp_which <- which(dat$attr$Status==1)
  dat$attr$status[temp_which] <- "i"

  return(dat)
}
##########################################
