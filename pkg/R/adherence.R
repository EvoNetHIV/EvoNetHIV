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

adherence<-function(dat,at){
  #-----------------------------------
  #note: adherence_type==1, is default (random), set in vital_new_additions
  #type2 adherence: cyclic, asynchronous start
  #-----------------------------------
  
  adh2_ix <- which(dat$attr$adherence_type==2)
  if(length(adh2_ix)>0){

    start <- dat$attr$adherence_start[adh2_ix]
    days_high <- dat$param$adherence_days_high
    days_low <- dat$param$adherence_days_low

    day_within_cycle <- (at + start - 1) %% (days_high + days_low)
    ix1 <- which(day_within_cycle<days_high)
    ix2 <- which(day_within_cycle>=days_high)
    prob<-numeric(length(adh2_ix))
    if(length(ix1)>0){prob[ix1]<- dat$param$aherence_days_high_prob}
    if(length(ix2)>0){prob[ix2]<- dat$param$aherence_days_low_prob}
    if (dat$param$PrEP_Model == TRUE) { # Don't give drugs to PrEP patients if they have been diagnosed as HIV+
      diag_HIV_pos <- which(dat$attr$diag_status[adh2_ix] == 1)
      prob[diag_HIV_pos] <- 0
    }
    
    dat$attr$Adherence1[adh2_ix]=prob
    dat$attr$Adherence2[adh2_ix]=prob
    dat$attr$Adherence3[adh2_ix]=prob
    dat$attr$Adherence4[adh2_ix]=prob
    
  }
  #end type2 adherence
  return(dat)
}
