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
viral_update_gamma_john <- function(dat, at) {
  ###########################################################################################################
  #  Function to update viral loads of all infected individuals; can have flat VL or peak during acute infection                                       #
  #  Input: dat$V for infected agents, "dat" and "at" data structures from epimodel                   #
  #  Output: Makes changes to viral load: dat$pop$V                                                   #
  #  Output is a function of:                                                                         #
  #    dat$pop$Time_Inf_Adj   (time person was infected after adjusting for times on therapy)                                                  #
  ###########################################################################################################
  
  # Notes: treatment related dynamics from original virulence model were removed 
  #     (dealt with in treatment_fxn)
  # James organized all the "if then" statements in the original C file into 6 different scenarios to update viral load (sa)
  
  timestep <- at
  #
  infected          <-  dat$pop$Status==1 & dat$pop$treated!=1
  acute       <-  timestep <= (dat$pop$Time_Inf_Adj+dat$param$t_acute) 
  acute_exp <-  timestep <= (dat$pop$Time_Inf_Adj+dat$param$t_peak)
  acute_ph1 <- (acute & !acute_exp & 
                  ((timestep-dat$pop$Time_Inf_Adj) <= dat$param$t_acute_phase2) )
  
  if(dat$param$aids_death_model=="Gamma_Death"){
    aids_start<-  timestep > (dat$pop$Time_Inf_Adj + dat$pop$RandomTimeToAIDS)
  }else{
    #this syncs up cd4 aids and VL aids when aids_death_model=cd4
    aids_start<-dat$pop$CD4==4
  }
  
  vl_zero     <-  dat$pop$V < 0.0
  aids_max_vl <- dat$pop$V> (dat$param$vl_max_aids)
  
  acute_exp_ix  <- which(infected & acute_exp)
  acute_ph1_ix <- which(acute_ph1)
  acute_ph2_ix <- which(acute & !acute_exp & !acute_ph1)
  
  post_acute_ix  <- which(infected & !acute & !aids_start)
  aids_ix        <- which(infected & !acute & aids_start)
  vl_zero_ix     <- which(infected & vl_zero)
  aids_max_vl_ix <- which(aids_start& aids_max_vl)
  
  # During the earliest phases of acute infection, VL increases rapidly at rate "r0" 
  if(length(acute_exp_ix)>0){
    inf_duration <- (timestep-dat$pop$Time_Inf_Adj[acute_exp_ix])
    dat$pop$V[acute_exp_ix] <- dat$param$V0 * exp(dat$pop$r0[acute_exp_ix]*inf_duration)
  }  
  
  
  if(length(acute_ph1_ix)>0) {
    
    #------
    #new 3/2/16
    inf_duration <- (timestep-dat$pop$Time_Inf_Adj[acute_ph1_ix])
    time_post_acute_peak <- inf_duration - dat$param$t_peak
    ix <- acute_ph1_ix
    if(dat$param$vl_peak_agent_flag){
      vl_peak <-dat$pop$vl_peak_agent[ix] 
    }else{
      vl_peak <- dat$param$vl_peak_acute
    }
    
    dat$pop$V[ix] <- (vl_peak*
                        exp(-dat$pop$d_acute[ix]*time_post_acute_peak))
    #------
    
    #rate <- (log(dat$param$vl_peak_acute/
    #               dat$pop$vl_t_acute_phase2[acute_ph1_ix]) /
    #           (dat$param$t_peak-dat$param$t_acute_phase2) )
    
    #dat$pop$V[acute_ph1_ix] <- 
    #  dat$pop$V[acute_ph1_ix]*exp(rate)
  }
  
  if(length(acute_ph2_ix)>0) {
    #browser()
    #-----------------------
    #new 3/2/16
    ix <- acute_ph2_ix
    inf_duration <- (timestep-dat$pop$Time_Inf_Adj[ix])
    time_left_phase2 <- dat$param$t_acute-inf_duration 
    dat$pop$V[ix] <- dat$pop$SetPoint[ix]*exp(-dat$pop$rate_phase2[ix]*time_left_phase2)
    #V <- SPVL*exp(decay_rate_2nd_phase*(t_acute - time))
    #--------------------
    #dat$pop$V[acute_ph2_ix] <- 
    #  dat$pop$V[acute_ph2_ix]*exp(dat$param$acute_decline_phase2_rate)
  }
  
  
  # After the acute infection phase is over, VL increases slowly at rate "s"
  if(length(post_acute_ix)>0){
    tempVss <- dat$pop$SetPoint[post_acute_ix]
    dat$pop$V[post_acute_ix] <- (tempVss * exp(dat$pop$s[post_acute_ix] *
                                                 (timestep-dat$param$t_acute-dat$pop$Time_Inf_Adj[post_acute_ix])/365.0))
  }
  
  
  if (length(aids_ix) > 0){ 
    v_aids <- pmax(dat$pop$V[aids_ix],dat$param$vl_max_aids)
    new_values <- dat$pop$V[aids_ix]*dat$param$vl_increase_AIDS
    dat$pop$V[aids_ix] <-  new_values
    ix <- which(dat$pop$V[aids_ix]>v_aids)
    if(length(ix)){
      ix2 <- aids_ix[ix]
      dat$pop$V[ix2] <- v_aids[ix]
    }
  }
  
  #record start time for agents post-acute and vl>vl_max_aids
  ix<-which(dat$pop$Status==1 & 
            dat$pop$CD4==4 &
            is.na(dat$pop$start_max_aids) & 
            dat$pop$V>=dat$param$vl_max_aids)
  if(length(ix)>0){
    dat$pop$start_max_aids[ix] <- at
  }
  
  
  #to be deleted 3/6/16
  #if(length(aids_max_vl_ix)>0){
  #  dat$pop$V[aids_max_vl_ix] <- dat$param$vl_max_aids
  #}
  
  #update aim3 V_vec matrix (per john's request)
  #print(dat$pop$V)
  #print(dat$pop$V_vec[,1])
  aa=try(dat$pop$V_vec[,1]<-dat$pop$V)
  if(class(aa)=="try-error"){browser()}
  
  #for agents on treatment
  treatment_ix <- which(dat$pop$treated==1 & dat$pop$V> dat$param$vl_full_supp)
  dat$pop$V[treatment_ix] <- dat$pop$V[treatment_ix]*exp(dat$param$vl_exp_decline_tx) 
  
  
  return(dat)
}
