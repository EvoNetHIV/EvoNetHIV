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
social_testing_diagnosis_eligibles_only_module <- function(dat, at){   
 
  #Description:
  #Determines whether infected agent is tested (if so diagnosis_status changes  
  #and eligibility for care determined)
  #Two types of testing_model: "interval", "memoryless"
  #Inputs: 
    #param$testing_model
    #param$last_neg_test
  #Outputs: 
    #pop$diag_status
    #pop$diag_time
    #popsumm$diagnosed
  # In this version is the same as social_testng_diagnosis_module with two changes
  #   1.  Only people who are eligible for care get tested (this allows a simple method for ramping up testing later)
  #   2.  Changed local variable "ix" to "to_be_tested" for ease of understanding (this should have no effect on the results)
  
   if( ! dat$param$testing_model %in% c("interval","memoryless"))
     stop ("argument testing.pattern must equal \"memoryless\" or \"interval\".") 
  
  #note: when using NAs for uninfected, !is.element(dat$pop$diag,c(0,NA_real_))    
  to_be_tested <- which(dat$pop$eligible_care== 1 & dat$pop$Status %in% c(0,1) & dat$pop$diag_status %in% c(0,NA) )
  
  to_be_tested_male <- which(dat$pop$sex[to_be_tested] == 'm')
  to_be_tested_female <- which(dat$pop$sex[to_be_tested] == 'f')
  
  mean_test_interval <- numeric(length(to_be_tested))
  mean_test_interval[to_be_tested_male] <- dat$param$mean_test_interval_male
  mean_test_interval[to_be_tested_female] <- dat$param$mean_test_interval_female
    
  if(dat$param$testing_model=="memoryless") 
    testing <- to_be_tested[which(runif(length(to_be_tested))< 1/mean_test_interval)]
    
  
  if(dat$param$testing_model=="interval") {
    time_since_last_neg_test <- at - dat$pop$last_neg_test[to_be_tested]
    testing <- to_be_tested[which(time_since_last_neg_test >= mean_test_interval)]
  }
  
  if(length(testing)>0) {
    
    testing_and_infected <- which(dat$pop$Status[testing]==1)
    if(length(testing_and_infected)>0){
          
        testing_positive <- testing[testing_and_infected]
        testing_negative <- setdiff(testing, testing_positive)
              
          if(length(testing_negative)>0){
                
             dat$pop$last_neg_test[testing_negative] <- at
           }
              
           if(length(testing_positive)>0){    
              dat$pop$diag_status[testing_positive] <- 1
              dat$pop$diag_time[testing_positive] <- at  
              dat$pop$vl_at_test[testing_positive] <- dat$pop$V[testing_positive]
              dat$pop$cd4_at_test[testing_positive] <- dat$pop$CD4[testing_positive]
            }
        }  
    }
  return(dat)
}