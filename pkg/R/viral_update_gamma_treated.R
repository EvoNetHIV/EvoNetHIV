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
#' 
#' @export
viral_update_gamma_treated <- function(dat, at) {
  ###########################################################################################################
  #  Function to update viral loads of all infected individuals; can have flat VL or peak during acute infection                                       #
  #  Input: dat$V for infected agents, "dat" and "at" data structures from epimodel                   #
  #  Output: Makes changes to viral load: dat$pop$V                                                   #
  #  Output is a function of:                                                                         #
  #    dat$pop$Time_Inf   (time person was infected)                                                  #
  ###########################################################################################################
  
  # Notes: treatment related dynamics from original virulence model were removed 
  #     (dealt with in treatment_fxn)
  # James organized all the "if then" statements in the original C file into 6 different scenarios to update viral load (sa)
  
  timestep <- at
  #
  infected          <-  dat$pop$Status==1 & dat$pop$tx_dropout==1
  
  acute       <-  timestep <= (dat$pop$Time_Inf+dat$param$t_acute) 
  acute_exp <-  timestep <= (dat$pop$Time_Inf+dat$param$t_peak)
  acute_ph1 <- (acute & !acute_exp & 
                  ((timestep-dat$pop$Time_Inf) <= dat$param$t_acute_phase2) )
  
  if(dat$param$aids_death_model=="Gamma_Death"){
    aids_start<-  timestep > (dat$pop$Time_Inf + dat$pop$RandomTimeToAIDS)
  }else{
    #this syncs up cd4 aids and VL aids when aids_death_model=cd4
    aids_start<-dat$pop$CD4==4
  }
  
  vl_zero     <-  dat$pop$vl_expected < 0.0
  aids_max_vl <- dat$pop$vl_expected> (dat$param$vl_expectedl_max_aids)
  
  acute_exp_ix  <- which(infected & acute_exp)
  acute_ph1_ix <- which(acute_ph1)
  acute_ph2_ix <- which(acute & !acute_exp & !acute_ph1)
  
  post_acute_ix  <- which(infected & !acute & !aids_start)
  aids_ix        <- which(infected & !acute & aids_start)
  vl_zero_ix     <- which(infected & vl_zero)
  aids_max_vl_ix <- which(aids_start& aids_max_vl)
  
  # During the earliest phases of acute infection, VL increases rapidly at rate "r0" 
  if(length(acute_exp_ix)>0){
    inf_duration <- (timestep-dat$pop$Time_Inf[acute_exp_ix])
    dat$pop$vl_expected[acute_exp_ix] <- dat$param$V0 * exp(dat$pop$r0[acute_exp_ix]*inf_duration)
  }  
  
  
  if(length(acute_ph1_ix)>0) {
    
    #------
    #new 3/2/16
    inf_duration <- (timestep-dat$pop$Time_Inf[acute_ph1_ix])
    time_post_acute_peak <- inf_duration - dat$param$t_peak
    ix <- acute_ph1_ix
    if(dat$param$vl_peak_agent_flag){
      vl_peak <-dat$pop$vl_peak_agent[ix] 
    }else{
      vl_peak <- dat$param$vl_peak_acute
    }
    
    dat$pop$vl_expected[ix] <- (vl_peak*
                        exp(-dat$pop$d_acute[ix]*time_post_acute_peak))
    #------
  }
  
  if(length(acute_ph2_ix)>0) {
    #-----------------------
    #new 3/2/16
    ix <- acute_ph2_ix
    inf_duration <- (timestep-dat$pop$Time_Inf[ix])
    time_left_phase2 <- dat$param$t_acute-inf_duration 
    dat$pop$vl_expected[ix] <- dat$pop$SetPoint[ix]*exp(-dat$pop$rate_phase2[ix]*time_left_phase2)
    #--------------------
  }
  
  
  # After the acute infection phase is over, VL increases slowly at rate "s"
  if(length(post_acute_ix)>0){
    tempVss <- dat$pop$SetPoint[post_acute_ix]
    dat$pop$vl_expected[post_acute_ix] <- (tempVss * exp(dat$pop$s[post_acute_ix] *
                                                 (timestep-dat$param$t_acute-dat$pop$Time_Inf[post_acute_ix])/365.0))
  }
  
  
  if (length(aids_ix) > 0){ 
    v_aids <- pmax(dat$pop$vl_expected[aids_ix],dat$param$vl_expectedl_max_aids)
    new_values <- dat$pop$vl_expected[aids_ix]*dat$param$vl_increase_AIDS
    dat$pop$vl_expected[aids_ix] <-  new_values
    ix <- which(dat$pop$vl_expected[aids_ix]>v_aids)
    if(length(ix)){
      ix2 <- aids_ix[ix]
      dat$pop$vl_expected[ix2] <- v_aids[ix]
    }
  }
  
  
  
  
  return(dat)
}
