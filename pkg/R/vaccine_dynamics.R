#' @export
vaccine_dynamics <- function(dat,at){
   #note: actual modification of transmission dynamics occurs
   # in "transmission_vaccine" function

   dat <- initialize_vaccine_agents(dat,at)
   dat <- update_mu(dat,at)
   dat <- update_sigma(dat,at)
   dat <- initialize_phi(dat,at)
   dat <- update_phi(dat,at)
  

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
