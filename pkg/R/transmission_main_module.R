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
transmission_main_module <- function(dat,at)
{
  
  #Description: 
  #calculates probaility of infection for susceptible agent in disc. partnership
  #and then draws random uniform number to see if infection occurs
  
  # Processes main output of social_coital_acts_module: dat$discord_coital_df 
  # Calculates infection probability of susceptible partner based on who is receptive/insertive, 
  # and condom use, STI status (randomly assigned 0/1 to agents), age, VL, and transmission model 
  # (e.g., “hughes”,”exponential”); after infection probability assigned to susceptible, 
  # infection determined by random draw.
  
  #input: dat$discord_coital_df (sex/transmission covariates) and relative risk
  #for covariates
  #output: dat$discord_coital_df$infection (0/1)
  
  #either no discordant couples or discordant couples but no sex
  #if(is.null(  dat$discord_coital_df)){
  #  return(dat)}
  
  condom_use <- dat$discord_coital_df$condom
  insert_id  <- dat$discord_coital_df$insert_id 
  recept_id  <- dat$discord_coital_df$recept_id 
  sus_id     <- dat$discord_coital_df$sus_id 
  inf_id     <- dat$discord_coital_df$inf_id 
  sus_sex    <- dat$discord_coital_df$sus_sex
  inf_sex    <- dat$discord_coital_df$inf_sex
  logV_inf <-  log10(dat$attr$V[inf_id])
  V_inf <-   dat$attr$V[inf_id]
  on_prep <- dat$attr$on_prep[sus_id]
  susceptibility <- dat$attr$susceptibility[sus_id]
  
  ###############################################################
  ##### identify: 1) msm susceptible and insertive 
  #               2) msm susceptible and insertive and circumcised
  #               3) msm susceptible and receptive
  #               4) hetero infected female
  #               5) hetero infected male
  #               6) sti status of susceptible 
  
  
  #vectors of risk factors,inf status; filled in with 0 or 1 (0 initially)
  
  #------- msm variables -------------------------------
  xB_msm                        <-  rep(0, nrow(dat$discord_coital_df))
  #this should be set to "1" only where sus agent is insertive
  msm_sus_insert_status        <-  rep(0, nrow(dat$discord_coital_df))
  #this should be set to "1" only where sus agent is insertive and circum
  msm_circum_status_insert_sus <-  rep(0, nrow(dat$discord_coital_df))
  #this should be set to "1" only when sus agent is receptive
  msm_sus_receptive_status <- rep(0, nrow(dat$discord_coital_df))
  
  #---hetero variables--------------------
  xB_hetero            <- rep(0, nrow(dat$discord_coital_df))
  hetero_inf_female    <- rep(0, nrow(dat$discord_coital_df))
  hetero_inf_male      <- rep(0, nrow(dat$discord_coital_df))
  hetero_sus_female    <- rep(0, nrow(dat$discord_coital_df))
  hetero_sus_male      <- rep(0, nrow(dat$discord_coital_df))
  hetero_sus_female_vi <- rep(0, nrow(dat$discord_coital_df))
  hetero_sus_female_ai <- rep(0, nrow(dat$discord_coital_df))
  hetero_sus_male_ai   <- rep(0, nrow(dat$discord_coital_df))
  hetero_sus_male_circum_status <- rep(0, nrow(dat$discord_coital_df))
  age_vec_sus   <- dat$attr$age[sus_id]
  #cap ages to dat$param$max_age_RR_age so all females above "max_age"
  #have same RR due to age effects
  age_vec_sus[age_vec_sus>dat$param$max_age_RR_age]=dat$param$max_age_RR_age
  
  #----- either msm/hetero variables ---------------------
  #acute phase status of infector
  acute_phase_status <- rep(0,nrow(dat$discord_coital_df))
  acute_phase_index <-  which((at - dat$attr$Time_Inf[inf_id]) < dat$param$t_acute)
  if(length(acute_phase_index)>0){ acute_phase_status[acute_phase_index] <- 1 }
  #sti status of susceptible (relevant for both msm and hetero) 
  sti_status_sus <- dat$attr$sti_status[sus_id]
  #on prep
  
  #--------- preventative vaccine dynamics -----------------
   #note: multi-efficacy-dynanmics are after calculation of transmission probs ~ line
  
  #set vaccine effect to zero for all agents, but then fill in if necessary
  vacc_sens_sus <- rep(0,nrow(dat$discord_coital_df))
  
  #if sus is vaccinated and infected with sensitive virus
  if(at >=  dat$param$start_vacc_campaign[1] & dat$param$preventative_campaign==T){
       vacc_ix <- which(dat$attr$vaccinated[sus_id]==1 & 
                   dat$attr$virus_sens_vacc[inf_id]==1)
      if(length(vacc_ix)>0){vacc_sens_sus[vacc_ix] <- 1}  
  }
  
  
  #------end of preventative vaccine--------------------
  
  
  
  if(dat$param$model_sex=="msm"){
    
    #susceptible and insertive
    temp_which <- which(insert_id==sus_id)
    if(length(temp_which)>0){
      msm_sus_insert_status[temp_which] <- 1
    }
    #susceptible, insertive, circumcised
    temp_which <- which(insert_id==sus_id &
                          dat$attr$circum[insert_id]==1)
    if(length(temp_which)>0){
      msm_circum_status_insert_sus[temp_which] <- 1 
    }
    
    # susceptible and receptive
    msm_sus_receptive_status[which(recept_id==sus_id)] <- 1
    
  }#end of msm variables
  
  
  if(dat$param$model_sex!="msm"){
    ai <- dat$discord_coital_df$ai
    hetero_inf_female[which(inf_sex==0)] <- 1
    hetero_inf_male[which(inf_sex==1)]   <- 1
    hetero_sus_female[which(sus_sex==0)] <- 1
    hetero_sus_male[which(sus_sex==1)] <- 1
    hetero_sus_female_vi[which(sus_sex == 'f' & ai == 0)] <- 1
    hetero_sus_female_ai[which(sus_sex == 'f' & ai == 1)] <- 1
    hetero_sus_male_ai[which(sus_sex == 'm' & ai == 1)] <- 1
    #susceptible, male(insertive), circumcised
    temp_which <- which(hetero_sus_male == 1 & dat$attr$circum[sus_id]==1)
    if(length(temp_which)>0){
      hetero_sus_male_circum_status[temp_which] <- 1 
    } 
  } #end of hetero variables
  
  if(dat$param$transmission_model=="hill"){
    trans_probs <- transmission_hill_fxn(dat, acute_phase_status, sti_status_sus, condom_use, 
                                         msm_sus_receptive_status, msm_sus_insert_status,
                                         msm_circum_status_insert_sus, age_vec_sus,
                                         hetero_sus_female_vi, hetero_sus_female_ai, 
                                         hetero_sus_male_ai, hetero_sus_male_circum_status,
                                         V_inf, vacc_sens_sus, susceptibility)
  }else{
    trans_probs <- transmission_hughes_and_exp(dat, acute_phase_status, sti_status_sus, condom_use,
                                               msm_sus_receptive_status, msm_sus_insert_status,
                                               msm_circum_status_insert_sus, age_vec_sus,
                                               hetero_sus_female_vi, hetero_sus_female_ai,
                                               hetero_sus_male_ai, hetero_sus_male_circum_status,
                                               logV_inf, vacc_sens_sus, susceptibility)
  }
  ###################################
  # multi-efficacy dynamics
  #multi efficacy model
  #find which suscept. agent is vaccinated, get their vaccine efficacy
  # and fill in "vacc_efficacies" vector (all other values of this vector are zero)
  if(at >=  dat$param$start_vacc_campaign[1] & dat$param$vacc_multi_eff==T){
    vacc_efficacies <- rep(0,nrow(dat$discord_coital_df))
    vacc_ix <- which(dat$attr$vaccinated[sus_id]==1) 
    if(length(vacc_ix)>0){
      #for vaccinated and susceptible agents, get partner's VE
      inf_ix= inf_id[vacc_ix]
      vacc_efficacies[vacc_ix] <- dat$attr$vacc_eff[inf_ix]
      trans_probs <- trans_probs*(1-vacc_efficacies[vacc_ix])
    }  
  }
  
  
  ###################################
  # fill in discord_coital_df
  dat$discord_coital_df$logVL_inf     <- logV_inf
  dat$discord_coital_df$sti_status_sus  <- sti_status_sus
  dat$discord_coital_df$msm_sus_insert_status        <- msm_sus_insert_status
  dat$discord_coital_df$msm_circum_status_insert_sus <- msm_circum_status_insert_sus
  dat$discord_coital_df$msm_sus_receptive_status     <- msm_sus_receptive_status
  dat$discord_coital_df$hetero_inf_female             <- hetero_inf_female
  dat$discord_coital_df$hetero_inf_male               <- hetero_inf_male
  dat$discord_coital_df$hetero_sus_male_circum_status <- hetero_sus_male_circum_status
  dat$discord_coital_df$age_vec_sus <- age_vec_sus
  prep_ix <- which(on_prep==1)
  if(length(prep_ix)>0){
    trans_probs[prep_ix] <- trans_probs[prep_ix]*(1- dat$attr$prep_decrease[sus_id[prep_ix]])
  }
  dat$discord_coital_df$trans_probs <- trans_probs
  
  return(dat)
}
 

transmission_inf_calc<-function(dat,at) 
{  
  #####################################################
  # generate uniform random number, infection if less than trans probs 
  dat$discord_coital_df$uniform_prob <- runif(nrow(dat$discord_coital_df))
  index <- which(dat$discord_coital_df$uniform_prob < dat$discord_coital_df$trans_probs)
  dat$discord_coital_df$infection <- rep(0,nrow(dat$discord_coital_df))
  if(length(index)>0){
    dat$discord_coital_df$infection[index] <- 1
  }
  
  ####################################
  #if saving transmission probabilities (as exposure metric)
  if(dat$param$save_trans_probs){
    
    trans_probs_matrix <- cbind(dat$discord_coital_df$timestep,
             dat$discord_coital_df$trans_probs,
             dat$attr$id[dat$discord_coital_df$sus_id]) #use evonet id, not network id (network position)
    dat$trans_probs_list[[at]] <- trans_probs_matrix
  }
  
  return(dat)
  
}
