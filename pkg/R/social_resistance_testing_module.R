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
social_resistance_testing_module <- function(dat, at){   
 
  #Description:
  # Determines whether treated agent has developed drug resistant virus 
  # If so, set diag_resistant_virus is set.  
  #Two types of testing_model: "interval", "memoryless"
  #Inputs: 
    # param$testing_model
    # param$last_neg_test
    # dat$attr$V
    # dat$attr$aim3_no_muts
  #Outputs: 
    # dat$attr$diag_resistance_status
    #pop$diag_resistance_time
   
  if (dat$param$VL_Function != "aim3") return(dat)
  
  if( ! dat$param$resist_testing_model %in% c("interval","memoryless"))
     stop ("argument resistance testing pattern must equal \"memoryless\" or \"interval\".") 
  
  # To be tested for resistance, must be HIV+, diagnosed as HIV+, have a viral
  # not already have been diagnosed as resistant, have been infected a certain amount of
  # time, and have exceeded a VL threshold for resistance testing
  # note: when using NAs for uninfected, !is.element(dat$attr$diag,c(0,NA_real_))    
  ix <- which(dat$attr$Status == 1 & dat$attr$diag_resist_status %in% c(0,NA) &
              dat$attr$V > dat$param$VL_thres_resist_testing &
              (at - dat$attr$tx_init_time > dat$param$time_on_tx_for_resist_testing))
  
  if(dat$param$resist_testing_model=="memoryless") 
    testing <- ix[which(runif(length(ix))< 1/dat$param$mean_resist_test_interval)]
    
  if(dat$param$resist_testing_model=="interval") {
    time_since_last_neg_resist_test <- at - dat$attr$last_neg_resist_test[ix]
    testing <- ix[which(time_since_last_neg_resist_test >= dat$param$mean_resist_test_interval)]
  }
  
  if(length(testing)>0) {
    testing_and_resistant <- which(dat$attr$aim3_no_muts[testing]>=dat$param$no_muts_switch_2nd_line)
    if(length(testing_and_resistant)>0){
          
        testing_positive <- testing[testing_and_resistant]
        testing_negative <- setdiff(testing, testing_positive)
              
          if(length(testing_negative)>0){
                
             dat$attr$last_neg_resist_test[testing_negative] <- at
           }
              
           if(length(testing_positive)>0){    
              dat$attr$diag_resist_status[testing_positive] <- 1
              dat$attr$diag_resist_time[testing_positive] <- at  
            }
        }  
    }
  return(dat)
}
