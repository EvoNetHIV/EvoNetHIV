###########################################################
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
viral_update_aim3_args_list_2 <- function(dat,timeIndex,ind)
{  
  #description:
  #organizes variables into list for input into john's/aim3 
  #viral_update_modified_logisitic fxn; differs from
  #"viral_update_modified_logistic_args_list_1" as list components are 
  #derived model quantities, not raw input parameters
  
  list(Agent = ind,
       time_0     =  timeIndex-1,
       time_final =  timeIndex,
       SPVL    =  dat$attr$SetPoint[ind],
       Time_Inf = dat$attr$Time_Inf[ind],
       Adherence1  =  dat$attr$Adherence1[ind],
       Adherence2  =  dat$attr$Adherence2[ind],
       Adherence3  =  dat$attr$Adherence3[ind],
       Adherence4  =  dat$attr$Adherence4[ind],
       Virus_DT   =  data.table::data.table(
         V = dat$attr$V_vec[ind,],
         I = dat$attr$I_vec[ind,],
         M = dat$attr$M_vec[ind,],
         L = dat$attr$L_vec[ind,]
       ),
       K          =  dat$attr$K[ind],
       CD4        =  dat$attr$CD4[ind],
       Drug1      =  dat$attr$Drug1[ind],
       Drug2      =  dat$attr$Drug2[ind],
       Drug3      =  dat$attr$Drug3[ind],
       Drug4      =  dat$attr$Drug4[ind],
       TherapyStarted            =  dat$attr$treated[ind],
       SecondLineTherapyStarted  =  dat$attr$treated_2nd_line[ind],
       ChronicPhaseBegins        =  dat$attr$ChronPhase[ind],
       Immune_Response_Triggered =  dat$attr$Imm_Trig[ind],
       Aim3RoundingErrors        =  dat$attr$Aim3RoundingErrors[ind]) 
}
