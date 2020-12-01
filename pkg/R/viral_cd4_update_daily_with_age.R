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
viral_update_cd4_daily_with_age <- function(dat,at)
{  
  
  #Description:
  # Shifts CD4 value of infectees to next category if time in current category 
  # has expired, waiting time per CD4 category is either based on exponential distributions 
  # based on Pickles et al. data. or raw waiting times reported from their data (param$cd4_exp_flag=0/1)
  # otherwise cd4 value remains same
  
  # This differs from "viral_update_cd4_daily" in that rates of CD4 progression are assumed to be faster older people 
  # Data in Cori-Pickles suggests relative to their averages...
  #   1.23-fold more time in CD4 categories 1, 2, and 3 for people <30			
  #   1.08-fold more time in CD4 categories 1, 2, and 3 for people 30-35			
  #   0.92-fold less time in CD4 categories 1, 2, and 3 for people 35-40			
  #   0.82-fold less time in CD4 categories 1, 2, and 3 for people >40		
  # Corresponding ratios for CD4 < 200 to death
  #    1.83-fold more time in CD4 category 4 people <30	
  #    1.85-fold more time in CD4 category 4 people 30-35	
  #    0.64-fold less time in CD4 category 4 people 35-40		
  #    0.51-fold less time in CD4 category 4 people >40

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
  for(age_cat in 1:4){ #loop over age cats ,30,30-35,35-40,40+
  for(cd4_cat in 1:3){ #loop over cd4 cats (1,2,3)
    if(age_cat==1){
      infectee_subset <- which(dat$attr$CD4[infectees] ==cd4_cat & dat$attr$Status[infectees]==1  & 
                                 dat$attr$treated[infectees]!=1 & dat$attr$age[infectees] < 30)
    }
    if(age_cat==2){
      infectee_subset  <- which(dat$attr$CD4[infectees] ==cd4_cat & dat$attr$Status[infectees]==1  & 
                                 dat$attr$treated[infectees]!=1 & dat$attr$age[infectees] >= 30 & dat$attr$age[infectees] < 35)
    }
    if(age_cat==3){
      infectee_subset  <- which(dat$attr$CD4[infectees] ==cd4_cat & dat$attr$Status[infectees]==1  & 
                     dat$attr$treated[infectees]!=1 & dat$attr$age[infectees] >= 35 & dat$attr$age[infectees] < 40)
    }
    
    if(age_cat==4){                        
      infectee_subset  <- which(dat$attr$CD4[infectees] ==cd4_cat & dat$attr$Status[infectees]==1  & 
                                 dat$attr$treated[infectees]!=1 & dat$attr$age[infectees] >= 40)
    }
    
    if(length(infectee_subset)>0){
      cd4_times_index <-cbind(dat$attr$spvl_cat[infectees][infectee_subset],rep(cd4_cat,length(infectee_subset)))
      prob_increment <- 1.0/(dat$param$CD4_lookup[cd4_times_index]*365)
      if (age_cat == 1) { prob_increment <- prob_increment/1.23 }
      if (age_cat == 2) { prob_increment <- prob_increment/1.08 }
      if (age_cat == 3) { prob_increment <- prob_increment/0.64 }
      if (age_cat == 4) { prob_increment <- prob_increment/0.51 }
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
