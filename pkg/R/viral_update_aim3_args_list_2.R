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
       SPVL    =  dat$pop$SetPoint[ind],
       Time_Inf = dat$pop$Time_Inf[ind],
       Adherence1  =  dat$pop$Adherence1[ind],
       Adherence2  =  dat$pop$Adherence2[ind],
       Adherence3  =  dat$pop$Adherence3[ind],
       Adherence4  =  dat$pop$Adherence4[ind],
       Virus_DT   =  data.table::data.table(
         V = dat$pop$V_vec[ind,],
         I = dat$pop$I_vec[ind,],
         M = dat$pop$M_vec[ind,],
         L = dat$pop$L_vec[ind,]
       ),
       K          =  dat$pop$K[ind],
       CD4        =  dat$pop$CD4[ind],
       Drug1      =  dat$pop$Drug1[ind],
       Drug2      =  dat$pop$Drug2[ind],
       Drug3      =  dat$pop$Drug3[ind],
       Drug4      =  dat$pop$Drug4[ind],
       TherapyStarted            =  dat$pop$treated[ind],
       SecondLineTherapyStarted  =  dat$pop$treated_2nd_line[ind],
       ChronicPhaseBegins        =  dat$pop$ChronPhase[ind],
       Immune_Response_Triggered =  dat$pop$Imm_Trig[ind],
       Aim3RoundingErrors        =  dat$pop$Aim3RoundingErrors[ind]) 
}
