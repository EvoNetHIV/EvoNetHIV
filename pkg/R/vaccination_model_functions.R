
###########################

#' @export

initialize_vaccine_agents<- function(dat,at){
  
  if (at == dat$param$evonet.initialization.timestep+1) {
  #agent attributes initialized with NA
  attributes_NA <-c("phi","mu","sigma","theta","m","vaccination.dates.stack")
  no_alive<- length(dat$attr$Status)
  for(ii in 1:length(attributes_NA)){
    dat$attr[[attributes_NA[ii]]] <- vector('list',length=no_alive)
    dat$attr[[attributes_NA[ii]]][1:no_alive] = NA
  }
  
  #if no_marks>1 make each element of dat$attr$mu a list with length = dat$param$no_marks
  #and set initial value to NA
  if(is.numeric(dat$param$no_marks) & dat$param$no_marks>1){
    temp_list <- vector("list",length=dat$param$no_marks)
    for(ii in 1:length(dat$param$no_marks)){
      temp_list[[ii]] <- NA 
    }
    dat$attr$mu <- lapply(1:no_alive,function(x) temp_list)
  }
  
  
  
  }else{
    Status_vector_length <- length(dat$attr$Status)
    phi_vector_length <- length( dat$attr$phi )
    if (  Status_vector_length   > phi_vector_length ){
      total_new_agents <- Status_vector_length - phi_vector_length 
      new_agent_indices <- (Status_vector_length+1):(Status_vector_length+total_new_agents )
      attributes_NA <-c("phi","mu","sigma","theta","m","vaccination.dates.stack")
      for(ii in 1:length(attributes_NA)){
        dat$attr[[attributes_NA[ii]]][new_agent_indices] <- NA
      }
      #if no_marks >1, each mark gets separate list 
      if(is.numeric(dat$param$no_marks) & dat$param$no_marks>1){
        temp_list <- vector("list",length=dat$param$no_marks)
        for(ii in 1:length(dat$param$no_marks)){
          temp_list[[ii]] <- NA 
        }
         lapply(new_agent_indices,function(x) dat$attr$mu[[x]] <<- temp_list)
      } 
    }# end  if (  Status_vector_length   > phi_vector_length )
    
  }# end if new agents added to model
  return(dat)
  
}


#' @export
isVaccinatedByAgent <- function(dat){
  #assumes first element of phi attribute describes vaccination status and
  #is either NA (unvaccinated), 1 (vaccinated), 0 (formerly vaccinated)
  
  phi_pos1 <- unlist(lapply(dat$attr$phi,function(x) x[1]))
  (!is.na(phi_pos1) | phi_pos1 != 0)
}

#' @export
isInfectedByAgent <- function(dat) dat$attr$Status == 1

#' @export
isUninfectedByAgent <- function(dat) dat$attr$Status == 0

#' @export
isEligibleByAgent <- function(dat) dat$attr$eligible_care == 1

#' @export
getInfectedAgents <- function(dat) which(dat$attr$Status == 1)

#' @export
getAliveAgents <- function(dat) which(dat$attr$Status >= 0)

#' @export
getAgentsJustInfectedLastTimestep <- function(dat, at) which(dat$attr$Time_Inf == (at - 1) & (dat$attr$Status == 1))

#' @export
getAgentsPotentiallyInfectedThisTimestep <- function(dat, at) dat$discord_coital_df$inf_id

#' @export
getTransmittingPartners <- function(dat, inf_indices) dat$attr$Donors_Index[inf_indices]



## Vacc Model Agents Values getters/setters:
#' @export
getVaccModelAgentsValue <- function(dat, key, indices = NULL,pos=NULL) {
  
  stopifnot(key %in% names(dat$attr));
  if (is.null(indices) || (length(indices) == 0 ) || ((length(indices) == 1) && is.na(indices))) {
    indices <- 1:length( dat$attr$Status );
  }
  
  #this code not able to deal with elements with multiple values?
  #rv <- rep( 0, length(indices) );
  #for (x in seq_along(indices)) {
  #  rv[ x ] <- dat$attr[[key]][[ indices[ x ] ]]
  #}
  #return( rv );
  
  attr_list <- dat$attr[[key]]
  if(is.numeric(pos)){
    lapply(indices,function(x) attr_list[[x]][[pos]])
  }else{
    lapply(indices,function(x) attr_list[x])
  }

}

#' @export
setVaccModelAgentsValue <- function(dat, key, value, indices = NULL,pos=NULL) {
  #if length(value)==1, same value used for all indices
  #otw, if length(value)!=length(indices), error
  
  stopifnot(key %in% names(dat$attr));
  if (is.null(indices) || (length(indices) == 0) || ((length(indices) == 1) && is.na(indices))) {
    indices <- 1:length(dat$attr$Status);
  }
  if(length(value)==1){value <- rep(value,length(indices))}
  #stopifnot(length(value)==length(indices))
  if(length(value)!=length(indices)){browser()}

   if(is.numeric(pos)){
     for (x in seq_along(indices)) { dat$attr[[key]][[indices[x]]][[pos]] <- value[[x]] }
   }else{
     for (x in seq_along(indices)) { dat$attr[[key]][[indices[x]]] <- value[[x]] }
   }
  return(dat);
}


#' @export
getMu <- function(dat,indices = NULL,pos=NULL){
  getVaccModelAgentsValue(dat, "mu", indices,pos)
}

#' @export
setMu <- function(dat, mu_values, indices = NULL,pos=NULL) {
  setVaccModelAgentsValue(dat, "mu", mu_values, indices,pos)
}

#' @export
getSigma <- function(dat, indices = NULL) {
  getVaccModelAgentsValue(dat, "sigma", indices)
}

#' @export
setSigma <- function(dat, sigma_values, indices = NULL) {
  setVaccModelAgentsValue(dat, "sigma", sigma_values, indices)
}

#' @export
getPhi <- function(dat, indices = NULL) {
  getVaccModelAgentsValue(dat, "phi", indices)
}

#' @export
setPhi <- function(dat, phi_values, indices = NULL) {
  setVaccModelAgentsValue(dat, "phi", phi_values, indices)
}

#' @export
getM <- function(dat, indices = NULL) {
  getVaccModelAgentsValue(dat, "m", indices)
}

#' @export
setM <- function(dat, m_values, indices = NULL) {
  setVaccModelAgentsValue(dat, "m", m_values, indices)
}

#' @export
getTheta <- function(dat, indices = NULL) {
  getVaccModelAgentsValue(dat, "theta", indices)
}

#' @export
setTheta <- function(dat, theta_values, indices = NULL) {
  setVaccModelAgentsValue(dat, "theta", theta_values, indices)
}

#' @export
getMostRecentVaccinationDate <- function(dat, indices = seq_along(dat$attr$vaccination.dates.stack)) {
  ## We store vaccination dates as a stack (last in, first out, so the first element is always the most recent one).
  vacc_dates_list <- lapply(indices,function(x) dat$attr$vaccination.dates.stack[[x]][1])
  return(unlist(vacc_dates_list))
}


#' @export
addVaccinationDate <- function(dat, vaccination_date, indices = newly_vacc_agents) {
  ## We store vaccination dates as a stack (last in, first out, so the first element is always the most recent one).
  for (x in seq_along(indices)) {
    vaccination_dates_previous <- unlist(dat$attr[["vaccination.dates.stack"]][[indices[x]]])
    dat$attr[["vaccination.dates.stack"]][[indices[x]]] <- c(vaccination_date, vaccination_dates_previous)
  }
  return( dat );
}

# This is set up to work for our default configuration, which is the one-mark or
# two-mark models, with "sensitive" always present, and maybe a second option (its name doesn't matter here).
#' @export
update_mu <- function(dat, at) {
  if (at == dat$param$evonet.initialization.timestep+1) {
    # initial infecteds at model start
    inf_indices <- getInfectedAgents(dat);
    no_infected <- length(inf_indices)
    mu_list <- vector('list',length=length(inf_indices))
    no_alive <- length(dat$attr$Status)
    
    #if no_marks>1 make each element of dat$attr$mu a list with length = dat$param$no_marks
    #and set initial value to NA
    if(is.numeric(dat$param$no_marks) & dat$param$no_marks>1){
      temp_list <- vector("list",length=dat$param$no_marks)
      for(ii in 1:dat$param$no_marks){
        temp_list[[ii]] <- NA 
      }
      dat$attr$mu <- lapply(1:no_alive,function(x) temp_list)
    }
    
    if(is.numeric(dat$param$no_marks) & length(dat$param$no_marks)==1){
      for(ii in 1:dat$param$no_marks){
        mu_by_strain <-  dat$param$vaccine.efficacy.by.mark[[ii]]
        probs <- dat$param$initial.mark.distribution[[ii]]
        mu_values <- sample(x=mu_by_strain,size=no_infected,prob=probs,replace=T)
        dat <- setMu(dat, mu_values, inf_indices,pos=ii);
    } 
    }
    
  } else {
    # secondary infections from previous timestep
    inf_indices <- getAgentsJustInfectedLastTimestep( dat, at );
    
    if (length( inf_indices) > 0) {
      donor_indices <- getTransmittingPartners( dat, inf_indices );
      mu_values <- getMu(dat, donor_indices);
      dat <- setMu(dat, mu_values, inf_indices);
    }
  }
  return(dat);
}


#' @export
update_sigma <- function(dat, at) {
  if (at == dat$param$evonet.initialization.timestep+1) {
    #initial infecteds at model start
    inf_indices <- getInfectedAgents( dat );
    sigma_values <- rep( 0, length( inf_indices ) );
    dat <- setSigma( dat, sigma_values, inf_indices );
  } else {
    
    # secondary infections from previous timestep
    inf_indices <- getAgentsJustInfectedLastTimestep( dat, at );
    if (length( inf_indices) > 0) {
      sigma_values <- 0 #base model: sigma=0
      dat <- setSigma(dat, sigma_values, inf_indices);
    }
  }
  return(dat);
}

# This and the other functions are presently vectorized.
# This is run daily to vaccinate new people.
#' @export
initialize_phi <- function(dat, at) {
  #vaccine campaign hasn't started
  if(at < dat$param$vaccine.rollout.year*365  ){return(dat)}
  #vaccination all at once when at=dat$param$vaccine.rollout.year*365, mainly for debugging purposes
  if(at > (dat$param$vaccine.rollout.year*365) & dat$param$vaccine.rollout.duration.years==0 ){return(dat)}
  
  phi.values <- unlist(getPhi(dat));
  
  # Each agent is either vaccinated, unvaccinated, or previously vaccinated (and presently unvaccinated)
  is.vaccinated.by.agent <- (!is.na( phi.values ) & phi.values > 0 )
  vaccinated.placebo <- (!is.na(phi.values) & (phi.values == 2))
  is.unvaccinated.by.agent <- is.na( phi.values );
  is.previously.vaccinated.by.agent <- (!is.na( phi.values ) & phi.values == 0)
  temp_sum <- as.numeric(is.vaccinated.by.agent + is.unvaccinated.by.agent +is.previously.vaccinated.by.agent+
                           vaccinated.placebo )
  if( any(temp_sum != 1)){browser()}
  
  num.alive.and.vaccinated <-
    sum( (is.vaccinated.by.agent | vaccinated.placebo) & (isInfectedByAgent(dat) | isUninfectedByAgent(dat)));
  num.alive <-
    sum((isInfectedByAgent(dat) | isUninfectedByAgent(dat)));
  stopifnot( num.alive == length( getAliveAgents( dat ) ) );
  
  #  if designated vacc. level already reached (percent of attr vaccianted), don't vacc anymore
  if ((num.alive.and.vaccinated / num.alive) > dat$param$fraction.vaccinated) { return( dat) };
  
  is.too.recently.vaccinated <- ((at - getMostRecentVaccinationDate(dat)) <
                                   dat$param$revaccination.eligibility.years*365);
  
  #  Identify eligible_patients: eligible for care, not vaccinated, not infected
  #  by default, all agents eligible for care, unless specified otw
  # never been vaccinated
  age_vec <- get_attr(dat,item="age")
  no_age_range <- length(which(age_vec > dat$param$vaccine_age_range[1] & age_vec < dat$param$vaccine_age_range[2]))
  eligible_indices1 <- which( isUninfectedByAgent( dat ) &
                                is.unvaccinated.by.agent &
                                age_vec > dat$param$vaccine_age_range[1] &
                                age_vec < dat$param$vaccine_age_range[2] &
                                isEligibleByAgent( dat ) );
  # previously vaccinated
  eligible_indices2 <- which( isUninfectedByAgent( dat ) &
                                is.previously.vaccinated.by.agent &
                                age_vec > dat$param$vaccine_age_range[1] &
                                age_vec < dat$param$vaccine_age_range[2] &
                                !is.too.recently.vaccinated &
                                isEligibleByAgent( dat ) )
  
  eligible_indices <- c( eligible_indices1, eligible_indices2 );
  
  #if trial in place, remove agents deemed eligible but not in trial
  #note: needs gettr fxn
  if(dat$param$vaccine_trial){
    trial_status = dat$attr$trial_status[ eligible_indices]
    trial_index <- which(trial_status==1)
    if(length(trial_index)>0){
      eligible_indices <- eligible_indices[trial_index]
    }else{
      eligible_indices <- NULL
    }
  }
  
  # if no agents eligible, end fxn
  if (length(eligible_indices) == 0) { return(dat) };
  
  #  calculate how many agents should be newly vaccinated at this time, based on user-specified vaccination rate
  if(dat$param$vaccine.rollout.duration.years==0){
      num.newly.vaccinated.today <- round(dat$param$fraction.vaccinated *  no_age_range)
  }else{
     num.newly.vaccinated.today <- ( (dat$param$fraction.vaccinated * 
                                        no_age_range) / (365*dat$param$vaccine.rollout.duration.years))
  }
  
  if( num.newly.vaccinated.today>1 & dat$param$vaccine.rollout.duration.years!=0){
    num.newly.vaccinated.today <- round(num.newly.vaccinated.today+rbinom(1,1,0.5)/2)
  }
  
  
  if( num.newly.vaccinated.today<1){
    num.newly.vaccinated.today <- rpois(1,num.newly.vaccinated.today)
  }
  
  #if(at==365+30) browser()
  
  if (num.newly.vaccinated.today == 0) return(dat)
  
  

  
  # if number of eligible agents exceeds number permissible, randomly choose subset
  # if the %coverage in total attrulation alive exceeds #eligible, vaccinate all eligible
  
  if(num.newly.vaccinated.today < length(eligible_indices)){
    vaccinated_indices <- sample( eligible_indices, num.newly.vaccinated.today )
  }else{
    vaccinated_indices <- eligible_indices
  }
  
  if(dat$param$vaccine_trial){
    binom_out <- rbinom(length(vaccinated_indices),1,prob=dat$param$prop_vaccinated_placebo)
    placebo_index <- which(as.logical(binom_out))
    if(length(placebo_index)>0){
       vaccinated_placebo <- vaccinated_indices[placebo_index]
       dat <- setPhi( dat, 2, vaccinated_placebo)
       vaccinated_actual <- vaccinated_indices[-placebo_index]
       if(length(vaccinated_actual)>0){
         dat <- setPhi( dat, 1, vaccinated_actual)
       }
    }
  }else{
    
    #simple case of phi equal to single numeric value (assumed (0,1] )
    if(is.numeric(dat$param$initial_phi_value) & length(dat$param$initial_phi_value)==1){
    dat <- setPhi( dat, dat$param$initial_phi_value, vaccinated_indices )
    }else{
      #placeholder for more complicated values of phi (e.g., other than single numberic value)
    }
    
  }
  
  dat <- addVaccinationDate( dat, at, vaccinated_indices );
  
  return( dat );
} # initialize_phi (..)

## This is run daily to check if the vaccine efficacy wanes
## (presently we have efficacy fixed for all vaccinated persons
## for whom it has not waned). Note that right now this should be
## run before initialize_phi, because we do not check that the
## person was vaccinated today; TODO: Add that check so this can
## be run before or after initialize_phi in either order.
#' @export
update_phi <- function(dat, at) {
  if(at <= dat$param$vaccine.rollout.year*365  ){return(dat)}


  # Each agent is either vaccinated, unvaccinated, or previously vaccinated (and presently unvaccinated)
  # get vaccination status of agents 
  phi.values <- unlist(getPhi(dat))
  is.vaccinated.by.agent <- (!is.na(phi.values) & phi.values > 0)
  vaccinated.placebo <- (!is.na(phi.values) & (phi.values == 2))
  is.unvaccinated.by.agent <- is.na( phi.values )
  is.previously.vaccinated.by.agent <- (!is.na(phi.values) & (phi.values == 0))
  test <- (all(as.numeric(is.vaccinated.by.agent + is.unvaccinated.by.agent +  vaccinated.placebo+
                             is.previously.vaccinated.by.agent) == 1))
  if(!test){browser()}
 ###############################################
  #increasing vaccine efficacy
 if(dat$param$vacc_type=="linear"){
   #phi values incremented each timestep by "vacc_phi_daily_increase"
   update_indices <- which( is.vaccinated.by.agent & isUninfectedByAgent(dat) & phi.values< 1 )
   if(length(update_indices)>0){
     new_values <- phi.values[update_indices]+ dat$param$vacc_phi_daily_increase
     #prevent new values > 1
     new_values <- pmin(new_values,1)
     dat <- setPhi( dat, new_values, update_indices )
    }
 }  
  
  ################################################
  #start of end of vaccine protection
  vacc_indices <- which( is.vaccinated.by.agent & isUninfectedByAgent(dat) )
  vacc_dates <- getMostRecentVaccinationDate(dat,indices = vacc_indices) 
  vacc_duration <-  at - vacc_dates
  #index based on vaccinated agents only
  vacc_elapsed_index <- which(vacc_duration > dat$param$vacc_min_efficacy_duration)
  #if not vaccinated agent has reached min vacc. duration, end fxn
  if(length(vacc_elapsed_index )==0){return(dat)}
  # index based on total populaton (eg, poisition in attr list)
  waning_index <-  vacc_indices[vacc_elapsed_index]
  
  
  if(dat$param$vaccine_waning_type=="daily_prob"){
      new_values <- rbinom( length( waning_index ), 1, 1 - dat$param$daily.vaccine.reversion.rate );
      dat <- setPhi( dat, new_values,waning_index )
  }
  
  if(dat$param$vaccine_waning_type=="cliff-edge"){
      new_values <- 0  
      dat <- setPhi( dat, new_values, waning_index[vacc_cliff_edge_index]  )
  }
  
  if(dat$param$vaccine_waning_type=="exponential"){
    
    if(dat$param$vacc_exp_decline_rate>0){
      exp_decline <-  (-1* dat$param$vacc_exp_decline_rate)
    }else{
      exp_decline <- dat$param$vacc_exp_decline_rate
    }
    time_elapsed <- (vacc_duration[ vacc_elapsed_index]-dat$param$vacc_min_efficacy_duration)
    new_values <- exp(exp_decline *time_elapsed )
    new_values[new_values<0.01] <- 0
    dat <- setPhi( dat, new_values, waning_index )
  }
  
  # end of end of vaccine protection
  ################################################  
  return( dat );
}

#' @export
draw_m <- function(dat, at, ...) {
  inf_indices <- getAgentsPotentiallyInfectedThisTimestep(dat, at);
  
  
  #need clarification from Paul on next 7 lines
      #transmitter.mu <- unlist(getMu( dat, inf_indices ));
      ## transmitter.mu for the basic model is an index into
      ## names( vaccine.efficacy.by.mark ) (0-indexed, so add 1).
      ## That is, 0 means sensitive, and 1 means resistant.
      #mark.index <- round( transmitter.mu ) + 1;
      #stopifnot( mark.index %in% seq_along( names( dat$param$vaccine.efficacy.by.mark ) ) );
      #dat$vacc_model$agents[[ "m" ]][ inf_indices ] <- names( dat$param$vaccine.efficacy.by.mark )[ mark.index ];
  
  #simple, baseline case: 1 mark,   m = mu
  if(dat$param$no_marks==1){
    transmitter.mu <- unlist(getMu( dat, inf_indices ))
    if(length(transmitter.mu) != length(inf_indices)) {browser()}
    dat <- setM(dat, transmitter.mu,inf_indices) 
  }
  
  return( dat );
} # draw_m (..)

# theta is the vaccine-induced probability of avoiding an otherwise-infecting exposure
#' @export
calculate_theta <- function(dat, at) {
  
  # For vaccinated folks, it's the vaccine efficacy for the exposing/potentially infecting mark.
  phi.values <- unlist(getPhi( dat,dat$discord_coital_df$sus_id ))
  
  # Set theta for everyone, to ensure that no-longer-vaccinated folks get it set back to 0
  theta <- rep(0, nrow(dat$discord_coital_df))
  
  # get phi values for susceptible (uninfected) agents having sex with infected agent
  
  #index (row) of vaccinated agents in susceptible column in discord_coital_df table
  vacc_index <- which(!is.na(phi.values) & (phi.values == 1))
  if(length(vacc_index)>0){
    #get id of infected agent paired with vaccinated, uninfected agent from table
    transmitter_index <- dat$discord_coital_df$inf_id[vacc_index]
    #index in entire population of uninfected, vaccinated agent having sex with inf. agent
    is.vaccinated.by.agent <- dat$discord_coital_df$sus_id[vacc_index]

    #simple,baseline case: 1 mark, sigma=0 and theta=m=mu
    if(dat$param$no_marks==1){
      m <- unlist(getM( dat, transmitter_index))
      # Note m is a string index into names( vaccine.efficacy.by.mark ).
      theta[vacc_index ] <- m
    }
  }
  
  dat <- setTheta( dat, theta,dat$discord_coital_df$sus_id );
  
  return( dat );
} # calculate_theta (..)

