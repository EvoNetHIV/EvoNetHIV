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
viral_update_cd4_intial_pop <- function(dat)
{  
  
  #Description:
  # Shifts CD4 value of infectees to next category if time in current category 
  # has expired, waiting time per CD4 category is either based on exponential distributions 
  # based on Pickles et al. data. or raw waiting times reported from their data (param$cd4_exp_flag=0/1)
  # otherwise cd4 value remains same
  #Input: dat$attr$CD4_TimeToAIDS_exp_cat1
  #dat$attr$CD4_time_cat2
  #dat$attr$CD4_time_cat3
  #dat$attr$CD4_time_cat4
  #dat$attr$CD4_time
  #dat$param$CD4_lookup
  #Output: dat$attr$CD4, CD4_time_death
  
  start_time <- min(dat$attr$Time_Inf,na.rm=T)

  for(at in start_time:1){
    
  #index of alive infectees
  infectees <- which(dat$attr$CD4 < 5 & dat$attr$Status==1 &
                       dat$attr$Time_Inf<=at)
  dat$attr$CD4_time[infectees] <- dat$attr$CD4_time[infectees] + 1
  
  cd4_threshold_time <- rep(NA_real_,length(infectees))
  for(ii in 1:3){
    ix <- which(dat$attr$CD4[infectees]==ii)
    if(length(ix)>0){
      cd4_times_index <-cbind(dat$attr$spvl_cat[infectees][ix],rep(ii,length(ix)))
      prob_increment <- 1.0/(dat$param$CD4_lookup[cd4_times_index]*365)
      cd4_threshold_time[ix] <- rbinom(length(ix),1,prob_increment)
    } 
  }
  #identify agents whose cd4 values will increase  
  cd4_increment<- which(cd4_threshold_time==1)
  
  if(length(cd4_increment)>0)
  {    
    infected_and_cd4_increment <- infectees[cd4_increment]
    dat$attr$CD4[infected_and_cd4_increment] <- dat$attr$CD4[infected_and_cd4_increment] + 1
    index_aids <- which(dat$attr$CD4==4 & is.na(dat$attr$start_aids_cd4))
    if(length(index_aids)>0){
      dat$attr$RandomTimeToAIDS[index_aids] <- at
      dat$attr$start_aids_cd4[index_aids] <- at
    }
    dat$attr$CD4_time[infected_and_cd4_increment] <- 0
    dat$attr$CD4_nadir[infected_and_cd4_increment] <-dat$attr$CD4[infected_and_cd4_increment]
    
    cd4_dead <- which(dat$attr$CD4[infected_and_cd4_increment] ==5)
    
    
    if(length(cd4_dead)>0)
    {
      cd4_dead_final_index <- infected_and_cd4_increment[cd4_dead]
      dat$attr$CD4_time_death[cd4_dead_final_index] <- at      
    }
  }
  
  #cd4_unchanged <-  which(cd4_threshold_time==0)
  #if(length(cd4_unchanged)>0){
  #   dat$attr$CD4_time[cd4_unchanged] <- dat$attr$CD4_time[cd4_unchanged]+1}
  
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
  if(length(cd4_cat4)<1){
    prob_death <- runif(length(cd4_cat4))
    death_index <- which(prob_death < dat$param$cd4_cat4_treated_death_prob)
    if(length(death_index)>0){
      pop_index<- cd4_cat4[death_index]
      dat$attr$CD4[pop_index] <- 5
    }
  }
  
  }
  #-----------------------------------
  return(dat)
}
