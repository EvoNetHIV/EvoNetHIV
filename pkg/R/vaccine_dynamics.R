#' @export
vaccine_dynamics <- function(dat,at){
   #note: actual modification of transmission dynamics occurs
   # in "transmission_vaccine" function

   dat <- initialize_vaccine_agents(dat,at)
   dat <- update_mu(dat,at)
   dat <- update_sigma(dat,at)
   dat <- initialize_phi(dat,at)
   dat <- update_phi(dat,at)
 
#if mean degree by age (risk groups) for GF model
if(length(dat$param$age_nw_groups)>1){
      age_cats <- 1:length(dat$param$age_nw_groups)
      for(ii in 1:length(age_cats)){
         age1 <- dat$param$age_nw_groups[[ii]][1]
         age2 <- dat$param$age_nw_groups[[ii]][2]
         ix <- which(dat$attr$age > age1 & dat$attr$age < age2+1)
         dat$attr$att1[ix] <- ii
      }
      
}

#vaccine trial setup----------
  #assign trial status to agents at dat$param$trial_status_time_switch (default=365 days)
  #so network filters out pairings of trial participants by time 
  #vaccine trial starts (dat$param$vaccine.rollout.year[1]*365)   
   if( at== (dat$param$vaccine.rollout.year[1]*365-dat$param$trial_status_time_switch) & dat$param$vaccine_trial ) {
         trial_samples <- round(length(dat$attr$Status)*dat$param$fraction.vaccinated)
         trial_index <-  sample(1:length(dat$attr$Status), trial_samples,replace=F)
         dat$attr$trial_status[trial_index] <- 1
      }
#---------------------------
   
return(dat)
}
