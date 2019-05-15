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
testing <- function(dat, at){   
 
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
  
  if( ! dat$param$testing_model %in% c("interval","memoryless"))
     stop ("argument testing.pattern must equal \"memoryless\" or \"interval\".") 
  
  #note: when using NAs for uninfected, !is.element(dat$pop$diag,c(0,NA_real_))    
  ix <- which(dat$pop$Status %in% c(0,1) & dat$pop$diag_status %in% c(0,NA))
  
  if(length(ix)==0){return(dat)}
  
  ix_male <- which(dat$pop$sex[ix] == 'm')
  ix_female <- which(dat$pop$sex[ix] == 'f')
  ix_under25 <- which(dat$pop$age[ix] <= 25)
  
  mean_test_interval <- numeric(length(ix))
  mean_test_interval[ix_male] <- dat$param$mean_test_interval_male
  mean_test_interval[ix_female] <- dat$param$mean_test_interval_female
  if(dat$param$under_25_flag){
  mean_test_interval[ix_under25] <- dat$param$mean_test_interval_under25
  }
  
  if(dat$param$testing_model=="memoryless") 
    testing <- ix[which(runif(length(ix))< 1/mean_test_interval)]
    
  
  if(dat$param$testing_model=="interval") {
    time_since_last_neg_test <- (at - dat$pop$last_neg_test[ix])
    testers <- which(time_since_last_neg_test >= mean_test_interval)
    if(length(testers)>0){
      testing <- ix[testers]
    }else{testing <- NULL}
  }
  
  if(length(testing)>0) {
    #testing time for infected agents and associated values
    testing_and_infected <- which(dat$pop$Status[testing]==1 & at- dat$pop$Time_Inf[testing] > dat$param$test_result_delay)
    if(length(testing_and_infected)>0){
          
        testing_positive <- testing[testing_and_infected]
              
           if(length(testing_positive)>0){    
              dat$pop$diag_status[testing_positive] <- 1
              dat$pop$diag_time[testing_positive] <- at  
              dat$pop$vl_at_test[testing_positive] <- dat$pop$V[testing_positive]
              dat$pop$cd4_at_test[testing_positive] <- dat$pop$CD4[testing_positive]
            }
    }
    #testing time for uninfected agens
    testing_and_not_infected <- which(dat$pop$Status[testing]==0)
    if(length(testing_and_not_infected)>0){
      testing_negative <- testing[testing_and_not_infected]
      dat$pop$last_neg_test[testing_negative] <- at
    }
  }
  return(dat)
}