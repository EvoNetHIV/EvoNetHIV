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
cd4_update2 <- function(dat,at)
{  
  
  #Description:
  # Shifts CD4 value of infectees to next category if time in current category 
  # has expired, waiting time per CD4 category is either based on exponential distributions 
  # based on Pickles et al. data. or raw waiting times reported from their data (param$cd4_exp_flag=0/1)
  # otherwise cd4 value remains same
  
  # This functions models faster CD4 decay in older people 
  # The evonet default "viral_update_cd4_daily" doesn't include any age terms
  # This function is similar to "viral_update_cd4_daily_with_age", except that progression rates come from CASCADE rather than Cori-Pickles
  #
  # Cascade data suggest the following time-to-AIDS multipliers (using age 35 as a baseline)
  #   1.20 for people <25			
  #   1.09 for people 25-35			
  #   0.93 for people 35-45			
  #   0.84 for people 45-55		
  #   0.68 for people 55-65			
  #   0.54 for people >65		
  
  #ask john about aim3
  if(dat$param$VL_Function=="aim3"){
    # Aim 3 code allows virus to rebound due to drug resistance.  For aim 3 code, don't assume that treated
    # patients wont progress (i.e., don't restrict the list of infectees to those who aren't treated)
    # Note: It would probably be harmless to apply this to aim 2 code as well, but I am restricting this change
    # to the aim 3 code for the moment out of an abundance of caution
    infectees <- which(dat$attr$CD4 < 5 & dat$attr$Status==1 &
                         dat$attr$V > dat$param$vl_undetectable) # list of infectees with detectable viral loads.
  }else{
    #index of alive infectees not on tx
    infectees <- which(dat$attr$CD4 < 5 & dat$attr$Status==1  & dat$attr$treated!=1)
    # Start of new code specific to viral_cd4_update_daily_with_age
    # End of new code specific to viral_cd4_update_daily_with_age
  }
  
  #increment time in cd4 category
  dat$attr$CD4_time[infectees] <-   dat$attr$CD4_time[infectees]+1
  
  cd4_threshold_time <- rep(NA_real_,length(infectees))
  
  #non-aids agents have probabilistic time in cd4 categories 1,2,and 3  
  for(age_cat in 1:6){ #loop over age cats <25,25-35,30-35,35-45,45-55,55-65,>65
  for(cd4_cat in 1:3){ #loop over cd4 cats (1,2,3)
    if(age_cat==1){
      infectee_subset <- which(dat$attr$CD4[infectees] ==cd4_cat & dat$attr$Status[infectees]==1  & 
                                 dat$attr$treated[infectees]!=1 & dat$attr$age[infectees] < 25)
    }
    if(age_cat==2){
      infectee_subset  <- which(dat$attr$CD4[infectees] ==cd4_cat & dat$attr$Status[infectees]==1  & 
                                 dat$attr$treated[infectees]!=1 & dat$attr$age[infectees] >= 25 & dat$attr$age[infectees] < 35)
    }
    if(age_cat==3){
      infectee_subset  <- which(dat$attr$CD4[infectees] ==cd4_cat & dat$attr$Status[infectees]==1  & 
                                  dat$attr$treated[infectees]!=1 & dat$attr$age[infectees] >= 35 & dat$attr$age[infectees] < 45)
    }
    if(age_cat==4){
      infectee_subset  <- which(dat$attr$CD4[infectees] ==cd4_cat & dat$attr$Status[infectees]==1  & 
                                  dat$attr$treated[infectees]!=1 & dat$attr$age[infectees] >= 45 & dat$attr$age[infectees] < 55)
    }
    if(age_cat==5){
      infectee_subset  <- which(dat$attr$CD4[infectees] ==cd4_cat & dat$attr$Status[infectees]==1  & 
                                  dat$attr$treated[infectees]!=1 & dat$attr$age[infectees] >= 55 & dat$attr$age[infectees] < 65)
    }
    if(age_cat==6){                        
      infectee_subset  <- which(dat$attr$CD4[infectees] ==cd4_cat & dat$attr$Status[infectees]==1  & 
                                 dat$attr$treated[infectees]!=1 & dat$attr$age[infectees] >= 65)
    }
    
    if(length(infectee_subset)>0){
      cd4_times_index <-cbind(dat$attr$spvl_cat[infectees][infectee_subset],rep(cd4_cat,length(infectee_subset)))
      prob_increment <- 1.0/(dat$param$CD4_lookup[cd4_times_index]*365)
      
      #   1.20 above/below the average time-to-AIDS for people <25			
      #   1.07 above/below the average time-to-AIDS for people 25-35			
      #   0.93 above/below the average time-to-AIDS for people 35-45			
      #   0.84 above/below the average time-to-AIDS for people 45-55		
      #   0.68 above/below the average time-to-AIDS for people 55-65			
      #   0.54 above/below the average time-to-AIDS for people >65		
      
      if (age_cat == 1) { prob_increment <- prob_increment/1.25 }
      if (age_cat == 2) { prob_increment <- prob_increment/1.09 }
      if (age_cat == 3) { prob_increment <- prob_increment/0.91 }
      if (age_cat == 4) { prob_increment <- prob_increment/0.79 }
      if (age_cat == 5) { prob_increment <- prob_increment/0.61 }
      if (age_cat == 6) { prob_increment <- prob_increment/0.40 }
      greaterthan1 <- which(prob_increment > 1)
      prob_increment[greaterthan1] <- 1
      
       # Start of new code specific to viral_cd4_update_daily_with_age
       # End of new code specific to viral_cd4_update_daily_with_age
      cd4_threshold_time[infectee_subset] <- rbinom(length(infectee_subset),1,prob_increment)
    } 
  }#loop over cd4 categories (1,2,3)
  }#loop over age categories
 

  
  #agents in aids have deterministic time in aids
  infectee_subset <- which(dat$attr$CD4[infectees]==4)
  if(length(infectee_subset)>0){
    cd4_times_index <- cbind(dat$attr$spvl_cat[infectees][infectee_subset],rep(4,length(infectee_subset)))
    cd4_threshold_time[infectee_subset] <-   dat$attr$CD4_time[infectees][infectee_subset] > (dat$param$CD4_lookup[cd4_times_index]*365)
  } 
  
  
  #identify agents whose cd4 values will increase (1 to 2, 2 to 3, etc.) 
  cd4_increment<- which(cd4_threshold_time==1)
  
  if(length(cd4_increment)>0)
  {    
    infected_and_cd4_increment <- infectees[cd4_increment]
    dat$attr$CD4[infected_and_cd4_increment] <- dat$attr$CD4[infected_and_cd4_increment] + 1
    dat$attr$CD4_time[infected_and_cd4_increment] <- 0
    dat$attr$CD4_nadir[infected_and_cd4_increment] <-dat$attr$CD4[infected_and_cd4_increment]
    
    cd4_dead <- which(dat$attr$CD4[infected_and_cd4_increment] ==5)
    
    #this starts VL progression to aids level when cd4 aids starts
    if(dat$param$aids_death_model=="cd4"){
      if(any(dat$attr$CD4[infected_and_cd4_increment]==4)){
        new_aids_index <- which(dat$attr$CD4[infected_and_cd4_increment]==4)
        final_index <- infected_and_cd4_increment[new_aids_index]
        dat$attr$RandomTimeToAIDS[final_index] <- at
        dat$attr$start_aids_cd4[final_index] <- at
      }}
    
    
    if(length(cd4_dead)>0)
    {
      cd4_dead_final_index <- infected_and_cd4_increment[cd4_dead]
      dat$attr$CD4_time_death[cd4_dead_final_index] <- at      
    }
  }
  
  #cd4_unchanged <-  which(cd4_threshold_time==0)
  #if(length(cd4_unchanged)>0){dat$attr$CD4_time[cd4_unchanged] <- dat$attr$CD4_time[cd4_unchanged] +1   }
  
  #-----------------------------------
  #additional probability of death for agents 1,2,3 category
  # note! either treated or untreated
  #note: this should probably be turned into subfunction 10-14-15
  cd4_cat1<-which(dat$attr$Status==1 & dat$attr$CD4==1)
  if(length(cd4_cat1)>0){
    prob_death <- runif(length(cd4_cat1))
    death_index <- which(prob_death < dat$param$cd4_cat1_death_prob)
    if(length(death_index)>0){
      pop_index<- cd4_cat1[death_index]
      dat$attr$CD4[pop_index] <- 5
    }
  }
  cd4_cat2<-which(dat$attr$Status==1 & dat$attr$CD4==2)
  if(length(cd4_cat2)>0){
    prob_death <- runif(length(cd4_cat2))
    death_index <- which(prob_death < dat$param$cd4_cat2_death_prob)
    if(length(death_index)>0){
      pop_index<- cd4_cat2[death_index]
      dat$attr$CD4[pop_index] <- 5
    }
  }
  cd4_cat3<-which(dat$attr$Status==1 & dat$attr$CD4==3)
  if(length(cd4_cat3)>0){
    prob_death <- runif(length(cd4_cat3))
    death_index <- which(prob_death < dat$param$cd4_cat3_death_prob)
    if(length(death_index)>0){
      pop_index<- cd4_cat3[death_index]
      dat$attr$CD4[pop_index] <- 5
    }
  }
  #-----------------------------------
  #additional death prob for agents with cd4 aids and treated
  cd4_cat4<-which(dat$attr$Status==1 & dat$attr$CD4==4 & dat$attr$treated==1)
  if(length(cd4_cat4)>0){
    prob_death <- runif(length(cd4_cat4))
    death_index <- which(prob_death < dat$param$cd4_cat4_treated_death_prob)
    if(length(death_index)>0){
      pop_index<- cd4_cat4[death_index]
      dat$attr$CD4[pop_index] <- 5
    }
  }
  
  #-----------------------------------  
  #for agents on treatment
  #update for agents at cd4 nadir
  if(dat$param$VL_Function=="aim3"){
    # Aim 3 code allows virus to rebound due to drug resistance.  Don't allow CD4 counts to increase (= a decrease in
    # CD4 category) to jump back up in patients in whom virus is not suppressed.
    # Note: It would probably be harmless to apply this to aim 2 code as well, but I am restricting this change
    # to the aim 3 code for the moment out of an abundance of caution
    treatment_index_nadir <- which(dat$attr$treated==1 & dat$attr$V < dat$param$vl_undetectable &
                                     dat$attr$CD4==dat$attr$CD4_nadir & dat$attr$CD4!=1)
  } else {
    treatment_index_nadir <- which(dat$attr$treated==1 & 
                                     dat$attr$CD4==dat$attr$CD4_nadir & dat$attr$CD4!=1)
  }                              
  if(length(treatment_index_nadir)>0){
    improvement_prob <- runif(length(treatment_index_nadir))
    improvement_index <- which(improvement_prob< dat$param$cd4_prob_incr_nadir)
    if(length(improvement_index)>0){
      pop_index <- treatment_index_nadir[improvement_index]
      dat$attr$CD4[pop_index] <- dat$attr$CD4[pop_index] - 1
      dat$attr$CD4_time[pop_index] <- 0
    }
  }
  
  #update for agents on tx and cd4= nadir - 1  
  if(dat$param$VL_Function=="aim3") {
    # See note above for treatment_index_nadir
    treatment_index_nadir_plus <- which(dat$attr$treated==1 & dat$attr$V < dat$param$vl_undetectable &
                                          (dat$attr$CD4==dat$attr$CD4_nadir-1) & dat$attr$CD4!=1)
  } else {
    treatment_index_nadir_plus <- which(dat$attr$treated==1 & 
                                          (dat$attr$CD4==dat$attr$CD4_nadir-1) & dat$attr$CD4!=1)
  }
  if(length(treatment_index_nadir_plus)>0){
    improvement_prob <- runif(length(treatment_index_nadir_plus))
    improvement_index <- which(improvement_prob< dat$param$cd4_prob_incr_nadir_minus)
    if(length(improvement_index)>0){
      pop_index <- treatment_index_nadir_plus[improvement_index]
      dat$attr$CD4[pop_index] <- dat$attr$CD4[pop_index] - 1
      dat$attr$CD4_time[pop_index] <- 0
    }
  }
  
  
  #-----------------------------------
  return(dat)
}
