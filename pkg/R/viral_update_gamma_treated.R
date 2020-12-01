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
  #  Output: Makes changes to viral load: dat$attr$V                                                   #
  #  Output is a function of:                                                                         #
  #    dat$attr$Time_Inf   (time person was infected)                                                  #
  ###########################################################################################################
  
  # Notes: treatment related dynamics from original virulence model were removed 
  #     (dealt with in treatment_fxn)
  # James organized all the "if then" statements in the original C file into 6 different scenarios to update viral load (sa)
  
  timestep <- at
  #
  infected          <-  dat$attr$Status==1 & dat$attr$tx_dropout==1
  
  acute       <-  timestep <= (dat$attr$Time_Inf+dat$param$t_acute) 
  acute_exp <-  timestep <= (dat$attr$Time_Inf+dat$param$t_peak)
  acute_ph1 <- (acute & !acute_exp & 
                  ((timestep-dat$attr$Time_Inf) <= dat$param$t_acute_phase2) )
  
  if(dat$param$aids_death_model=="Gamma_Death"){
    aids_start<-  timestep > (dat$attr$Time_Inf + dat$attr$RandomTimeToAIDS)
  }else{
    #this syncs up cd4 aids and VL aids when aids_death_model=cd4
    aids_start<-dat$attr$CD4==4
  }
  
  vl_zero     <-  dat$attr$vl_expected < 0.0
  aids_max_vl <- dat$attr$vl_expected> (dat$param$vl_expectedl_max_aids)
  
  acute_exp_ix  <- which(infected & acute_exp)
  acute_ph1_ix <- which(acute_ph1)
  acute_ph2_ix <- which(acute & !acute_exp & !acute_ph1)
  
  post_acute_ix  <- which(infected & !acute & !aids_start)
  aids_ix        <- which(infected & !acute & aids_start)
  vl_zero_ix     <- which(infected & vl_zero)
  aids_max_vl_ix <- which(aids_start& aids_max_vl)
  
  # During the earliest phases of acute infection, VL increases rapidly at rate "r0" 
  if(length(acute_exp_ix)>0){
    inf_duration <- (timestep-dat$attr$Time_Inf[acute_exp_ix])
    dat$attr$vl_expected[acute_exp_ix] <- dat$param$V0 * exp(dat$attr$r0[acute_exp_ix]*inf_duration)
  }  
  
  
  if(length(acute_ph1_ix)>0) {
    
    #------
    #new 3/2/16
    inf_duration <- (timestep-dat$attr$Time_Inf[acute_ph1_ix])
    time_post_acute_peak <- inf_duration - dat$param$t_peak
    ix <- acute_ph1_ix
    if(dat$param$vl_peak_agent_flag){
      vl_peak <-dat$attr$vl_peak_agent[ix] 
    }else{
      vl_peak <- dat$param$vl_peak_acute
    }
    
    dat$attr$vl_expected[ix] <- (vl_peak*
                        exp(-dat$attr$d_acute[ix]*time_post_acute_peak))
    #------
  }
  
  if(length(acute_ph2_ix)>0) {
    #-----------------------
    #new 3/2/16
    ix <- acute_ph2_ix
    inf_duration <- (timestep-dat$attr$Time_Inf[ix])
    time_left_phase2 <- dat$param$t_acute-inf_duration 
    dat$attr$vl_expected[ix] <- dat$attr$SetPoint[ix]*exp(-dat$attr$rate_phase2[ix]*time_left_phase2)
    #--------------------
  }
  
  
  # After the acute infection phase is over, VL increases slowly at rate "s"
  if(length(post_acute_ix)>0){
    tempVss <- dat$attr$SetPoint[post_acute_ix]
    dat$attr$vl_expected[post_acute_ix] <- (tempVss * exp(dat$attr$s[post_acute_ix] *
                                                 (timestep-dat$param$t_acute-dat$attr$Time_Inf[post_acute_ix])/365.0))
  }
  
  
  if (length(aids_ix) > 0){ 
    v_aids <- pmax(dat$attr$vl_expected[aids_ix],dat$param$vl_expectedl_max_aids)
    new_values <- dat$attr$vl_expected[aids_ix]*dat$param$vl_increase_AIDS
    dat$attr$vl_expected[aids_ix] <-  new_values
    ix <- which(dat$attr$vl_expected[aids_ix]>v_aids)
    if(length(ix)){
      ix2 <- aids_ix[ix]
      dat$attr$vl_expected[ix2] <- v_aids[ix]
    }
  }
  
  
  
  
  return(dat)
}
