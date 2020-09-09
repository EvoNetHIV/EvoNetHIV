## Create a set of functions for a vaccine model, in an environment to be attach()ed.

## Basic; to add variations, you can modify the result. For now this doesn't create any mark variation (mu,sigma). TODO: ADD THAT STUFF. FOR NOW THIS IS MODEL 1.
create.basic.vaccination.model <- function (
  num.years = 15, # Could we instead get dat passed in and glean the number of years from it?
  fraction.vaccinated = 0.60,
  vaccine.rollout.year = 5,
  vaccine.rollout.duration.years = 1,
  vaccine.efficacy.years = 3,

  ## TODO: It looks like we can remove this datum. .. maybe the idea is that for each person it stores when they started .. but don't we use phi for that?
  start_vacc_campaign = (vaccine.rollout.year*365):(num.years*365),    # all vaccine models ### (50 is any number larger than NUM.YEARS -- it is NUMBER OF TIME STEPS.).
  max_perc_vaccinated = fraction.vaccinated, #all vacc. models, maximum percent/proportion of population to be vaccinated
  perc_vaccinated_rate = (max_perc_vaccinated /(1-max_perc_vaccinated ))/(years_to_max_coverage*365),
  vacc_eff_duration = 365*vaccine.efficacy.years,

  vaccine.efficacy.by.mark = c( "sensitive" = 0.8 ),  #models 1/1b/2/2b, proportion (percentage) decrease in trans probs due to vaccine for vaccine model "1" (baseline vaccine model),
  mark.distribution = c( "sensitive" = 1 ),

  vacc_trans_prob_decrease = vaccine.efficacy.by.mark[ "sensitive" ]
) {
    update_mu <- function ( dat, at ) {
      if(at==2){
        #initial infecteds at model start
        inf_index <- which(dat$pop$Status==1)
        mu_values <- dat$pop$virus_sens_vacc[inf_index] ## TODO: This is tying us to the underlying / older vaccine model which had hardcoded the concept of "sensitive" and "resistant"
        invisible(lapply(1:length(inf_index),function(x) dat$vacc_model$agents[[inf_index[x]]]$mu <<- mu_values[x]))
        
      }else{
        #secondary infections from previous timestep
        inf_index <- which(dat$pop$Time_Inf ==(at-1) & dat$pop$Status==1)
        if(length(inf_index)>0){
          donor_index <- dat$pop$Donors_Index[inf_index]
          mu_values <- as.numeric(lapply(donor_index,function(x) dat$vacc_model$agents[[x]]$mu))
          invisible(lapply(1:length(inf_index),function(x) dat$vacc_model$agents[[inf_index[x]]]$mu <<- mu_values[x]))
        
        }
      }
      return( dat$vacc_model$agents )
    } # update_mu (..)

    update_sigma <- function ( dat, at ) {
      if(at==2){
        #initial infecteds at model start
        inf_index <- which(dat$pop$Status==1)
        sigma_values <- 0
        invisible(lapply(1:length(inf_index),function(x) dat$vacc_model$agents[[inf_index[x]]]$sigma <<- sigma_values))
        
      }else{
        #secondary infections from previous timestep
        inf_index <- which(dat$pop$Time_Inf ==(at-1) )
        if(length(inf_index)>0){
          sigma_values <- 0
          invisible(lapply(1:length(inf_index),function(x) dat$vacc_model$agents[[inf_index[x]]]$sigma <<- sigma_values))
        }
      }
      return( dat$vacc_model$agents )
    } # update_sigma (..)
    
    initialize_phi <- function ( dat, at ) {
      
      #current phi values
       index <- 1:length(dat$vacc_model$agents)
       phi_values <- as.numeric(lapply(index,function(x) dat$vacc_model$agents[[x]]$phi))
       
    
      #if designated vacc. level already reached (percent of pop vaccianted), don't vacc anymore
      if(length(which(phi_values == 1 & dat$pop$Status>=0))/length(which(dat$pop$Status>=0)) > dat$param$max_perc_vaccinated){return(dat)}
      
      # Identify eligible_patients: eligible for care, not vaccinated, not infected
      # by default, all agents eligible for care, unless specified otw
      #note:dat$pop$phi == 0 is an agent whose vaccine effect ended (waned)
      
      #never been vaccinated
      eligible_index1 <- which(dat$pop$Status == 0 &
                                 is.na(phi_values) &
                                 dat$pop$eligible_care == 1)
      
      #previously vaccinated
      eligible_index2 <- which(dat$pop$Status == 0 &
                                 phi_values == 0 &
                                 (at-dat$pop$vacc_init_time) > dat$param$vacc_eff_duration &
                                 dat$pop$eligible_care == 1)
      
      eligible_index <- c(eligible_index1,eligible_index2)
      
      #if no agents eligible, end fxn
      if(length(eligible_index)==0){return(dat)}
      
      #calculate how many agents can be vaccinated, based on user-specified vaccination rate
      no_vaccinated <- sum(rbinom(length(which(dat$pop$Status>=0)), 1, dat$param$perc_vaccinated_rate)) #denominator is total population alive 
      if(no_vaccinated == 0) {return(dat)}
      
      #if number of eligible agents exceeds number permissible, randomly choose subset
      if(no_vaccinated <length(eligible_index)){
        vaccinated_index <- sample(eligible_index, no_vaccinated)
      }else{
        vaccinated_index <- eligible_index
        #if the %coverage in total population alive exceeds #eligible, vaccinate all eligible
      }
      
      invisible(lapply(1:length(vaccinated_index),function(x) dat$vacc_model$agents[[vaccinated_index[x]]]$phi <<- 1 ))
      dat$pop$vacc_init_time[vaccinated_index] <- at
      
        return(dat)
      
    } # initialize_phi (..)

    ### ?? This seems to be about the vaccine efficacy duration being actually random.
    update_phi <- function ( dat, at ) {
      # off/on for already vaccinated
      if(at > dat$param$start_vacc_campaign[1] ) {
        index <- 1:length(dat$vacc_model$agents)
        phi_values <- as.numeric(lapply(index,function(x) dat$vacc_model$agents[[x]]$phi))
        vacc_index <- which(phi_values == 1 & dat$pop$Status == 0)
        if(length(vacc_index)>0){
          new_values <-  rbinom(length(vacc_index), 1, 1 - (1/dat$param$vacc_eff_duration))
          invisible(lapply(1:length(vacc_index),function(x) dat$vacc_model$agents[[vacc_index[x]]]$phi <<- new_values[x]))
        }
      }
      return(dat)
    } # update_phi (..)

    ## This for now doesn't actually draw anything. For the basic model there's no within-host evolution. m = mu = { 1 for sensitive, 0 for resistant } -- see above.
    draw_m <- function ( dat, at, ... ) {
      index <- dat$infector_id
      mu_values <- as.numeric(lapply(index,function(x) dat$vacc_model$agents[[x]]$mu))
      return(mu_values)
    }
    
    calculate_theta <- function ( dat, m ) {
    
      theta <- rep(0,length(dat$susceptible_id))
      #of susceptibles, which are vaccinated
      phi_values <- as.numeric(lapply(dat$susceptible_id,function(x) dat$vacc_model$agents[[x]]$phi))
      index <- which(phi_values==1 & m==1)
      if(length(index)>0){
        #theta[index]  <- phi_values[index]*m[index]*dat$param$vacc_trans_prob_decrease
        theta[index]  <- dat$param$vacc_trans_prob_decrease
      }
      return(theta)
    } # calculate_theta (..)

    update_mu_and_sigma <- function ( dat, at ) {
      dat$vacc_model$agents <- update_mu(dat,at)
      dat$vacc_model$agents <- update_sigma(dat,at)
      return(dat)
    } # update_mu_and_sigma (..)
    
    initialize_and_update_phi<-function ( dat, at ) {
      
      if(at<dat$param$start_vacc_campaign[1]){return(dat)}
      if(!is.element(at,dat$param$start_vacc_campaign)){return(dat)}
    
      ## PAUL NOTES that this will clobber phi every time (by initializing it again after updating it)!
      dat <- update_phi(dat,at)
      ## TODO: FIX!
      dat <- initialize_phi(dat,at)
      return(dat)
    }
    
    initialize_vaccine_agents <- function ( dat, at ) {
      
      #at=2 is first time step after model setup/initialization
      if(at==2){
        #create agent object (list of lists attached to dat)
        agent_list <- list(phi=NA,mu=NA,mu_orig=NA,sigma=NA)
        no_current_agents <- length(dat$pop$Status)
        dat$vacc_model$agents <- vector('list',length=no_current_agents)
        #add class 'dat$param$vacc_model' to ojects to trigger appropriate model fxn method
        class(dat$vacc_model$agents)<- c("list",dat$param$vacc_model_id)
        class(dat)<- c("list",dat$param$vacc_model_id)
        dat$vacc_model$agents <- lapply(dat$vacc_model$agents, function(x) x <- agent_list)
        #if start of model initialize mu/sigma or initialize for new agents  
        dat$vacc_model$agents <- update_mu(dat,at)
        dat$vacc_model$agents <- update_sigma(dat,at) 
      }
      
      #if start of model initialize mu/sigma or initialize for new agents   
      if(at>2 & length(dat$pop$Status) > length(dat$vacc_model$agents)){
        #if(at>500) browser()
        agent_list <- list(phi=NA,mu=NA,sigma=NA)
        total_new_agents <- length(dat$pop$Status)- length(dat$vacc_model$agents)
        new_agents_index <- (length(dat$vacc_model$agents)+1):(length(dat$vacc_model$agents) + total_new_agents)
        invisible(lapply(new_agents_index,function(x) dat$vacc_model$agents[[x]] <<- agent_list ))
      }
      
      return(dat)
      
    }

    return( environment() );
} # create.basic.vaccination.model (..)

#' @export

# ####
#   #models 1/1b/2/2b, proportion (percentage) decrease in trans probs due to vaccine for vaccine model "1" (baseline vaccine model),
#  
#    #model 1 specific
#    #perc_virus_vaccine_sens = 0.5, #model 1, proportion of initial viruses/marks that are sensitive to vaccine 
#   
#   #model 1b specific
#   mu_mean  = 0.5, #model 1b, equilibrium mu value 
#   vacc_time_to_mu_mean  = 365, # model 1b, days to reach mu_mean for infected agents (equilibrium value)
#   vacc_model_min_mu = 0.01, #model 1b, lower bound of uniform dist. for initial dist. of mu values for model 1b
#   vacc_model_max_mu = 0.99, #model 1b, upper bound of uniform dist. for initial dist. of mu values for model 1b
#   
#   #model 2 specific
#   prob_loci_1               = 0.5, #probability of mu=1 (loci 1) for initially infected agent (model 2) 
#   prob_loci_2               = 0.5,#probability of mu=1 (loci 2) for initially infected agent (model 2)
#   trans_prob_decrease_scalar_model2 = 0.5 #proportional decrease in transmission for infected agents with
#   # 0/1 or 1/0 mu values relative to mu=1/1 at each loci which
#   #is equal to param. "trans_prob_decrease_scalar_model2"
