
#' @export
isInfectedByAgent <- function(dat) dat$pop$Status == 1

#' @export
isUninfectedByAgent <- function(dat) dat$pop$Status == 0

#' @export
isEligibleByAgent <- function(dat) dat$pop$eligible_care == 1

#' @export
getInfectedAgents <- function(dat) which(dat$pop$Status == 1)

#' @export
getAliveAgents <- function(dat) which(dat$pop$Status >= 0)

#' @export
getAgentsJustInfectedLastTimestep <- function(dat, at) which(dat$pop$Time_Inf == (at - 1) & (dat$pop$Status == 1))

#' @export
getAgentsPotentiallyInfectedThisTimestep <- function(dat, at) dat$discord_coital_df$inf_id

#' @export
getTransmittingPartners <- function(dat, inf_indices) dat$pop$Donors_Indices[inf_indices]

## Vacc Model Agents Values.
#' @export
createAgent <- function() {
  the.agent <- list(
    phi = NA,
    mu = NA,
    sigma = NA,
    m = NA, # formerly known as mu_orig
    theta = 0, # By default for unvaccinated folks, we will multiply the infection probability by 1 == (1 - theta)
    vaccination.dates.stack = list()
  );
  return(the.agent);
}

## Vacc Model Agents Values getters/setters:
#' @export
getVaccModelAgentsValue <- function(dat, key, indices = NULL) {
  stopifnot(key %in% names(dat$vacc_model$agents[[1]]));  ## CHECK: index this to agents[[1]], otherwise no names
  if (is.null(indices) || (length(indices) == 0 ) || ((length(indices) == 1) && is.na(indices))) {
    indices <- 1:length( dat$vacc_model$agents );
  }
  rv <- rep( 0, length( dat$vacc_model$agents ) );
  for (x in seq_along(indices)) { rv[ x ] <- dat$vacc_model$agents[[ indices[ x ] ]]$mu }
  return( rv );
}

#' @export
setVaccModelAgentsValue <- function(dat, key, value, indices = NULL) {
  # browser()
  stopifnot(key %in% names(dat$vacc_model$agents[[1]]));     ## CHECK: index this to agents[[1]], otherwise no names
  if (is.null(indices) || (length(indices) == 0) || ((length(indices) == 1) && is.na(indices))) {
    indices <- 1:length(dat$vacc_model$agents);
  }
  for (x in seq_along(indices)) { dat$vacc_model$agents[[indices[x]]][[key]] <- value[ x ]; }
  return(dat);
}

#' @export
getMu <- function(dat, indices = NULL) {
  getVaccModelAgentsValue(dat, "mu", indices)
}

#' @export
setMu <- function(dat, mu_values, indices = NULL) {
  setVaccModelAgentsValue(dat, "mu", mu_values, indices)
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
getMostRecentVaccinationDate <- function(dat, indices = seq_along(dat$vacc_model$agents)) {
  ## We store vaccination dates as a stack (last in, first out, so the first element is always the most recent one).
  vaccination.dates.stack <- getVaccModelAgentsValue(dat, "vaccination.dates.stack", indices);
  return( vaccination.dates.stack[ 1 ] );
}

#' @export
addVaccinationDate <- function(dat, vaccination_dates, indices = seq_along( dat$vacc_model$agents)) {
  ## We store vaccination dates as a stack (last in, first out, so the first element is always the most recent one).
  for (x in seq_along(indices)) {
    vaccination.dates.stack.x <-
      c( vaccination_dates[x], dat$vacc_model$agents[[indices[x]]][["vaccination.dates.stack"]] );
    dat$vacc_model$agents[[indices[x]]][["vaccination.dates.stack"]] <- vaccination.dates.stack.x;
    dat$vacc_model$agents[[indices[x]]][["vaccination.dates.stack"]] <- vaccination.dates.stack.x;
  }
  return( dat );
}

# This is set up to work for our default configuration, which is the one-mark or 
# two-mark models, with "sensitive" always present, and maybe a second option (its name doesn't matter here).
#' @export
update_mu <- function(dat, at) {
  if (at == dat$param$evonet.initialization.timestep) {
    # initial infecteds at model start
    inf_indices <- getInfectedAgents(dat);
    
    first.mark.name <- names( dat$param$vaccine.efficacy.by.mark );
    
    if (dat$param$initial.mark.distribution[first.mark.name] == 1.0) {
      mu_values <- rep( 1, length( inf_indices ) );
    } else if (dat$param$initial.mark.distribution[ first.mark.name ] == 0.0) {
      mu_values <- rep( 0, length( inf_indices ) );
    } else {
      ## TODO: expand to allow more than two marks by modifying this:
      mu_values <- rbinom( length( inf_indices ), 1,
                           prob = dat$param$initial.mark.distribution[ first.mark.name ] );
    }
    dat <- setMu(dat, mu_values, inf_indices);   ## CHECK: this not assigned back to dat
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
  if (at == dat$param$evonet.initialization.timestep) {
    #initial infecteds at model start
    inf_indices <- getInfectedAgents( dat );
    sigma_values <- rep( 0, length( inf_indices ) );
    dat <- setSigma( dat, sigma_values, inf_indices );
  } else {
    # Do nothing.
  }
  return(dat);
}

# This and the other functions are presently vectorized.
# This is run daily to vaccinate new people.
#' @export
initialize_phi <- function(dat, at) {
  phi.values <- getPhi(dat);
  
  # Each agent is either vaccinated, unvaccinated, or previously vaccinated (and presently unvaccinated)
  is.vaccinated.by.agent <- ( !is.na( phi.values ) & ( phi.values == 1 ) );
  is.unvaccinated.by.agent <- is.na( phi.values );
  is.previously.vaccinated.by.agent <- ( !is.na( phi.values ) & ( phi.values == 0 ) );
  stopifnot( all( as.numeric( is.vaccinated.by.agent + is.unvaccinated.by.agent + is.previously.vaccinated.by.agent ) == 1 ) )
  
  num.alive.and.vaccinated <-
    sum( is.vaccinated.by.agent & ( isInfectedByAgent( dat ) | isUninfectedByAgent( dat ) ) );
  num.alive <-
    sum( ( isInfectedByAgent( dat ) | isUninfectedByAgent( dat ) ) );
  stopifnot( num.alive == length( getAliveAgents( dat ) ) );
  
  #  if designated vacc. level already reached (percent of pop vaccianted), don't vacc anymore
  if ((num.alive.and.vaccinated / num.alive) > dat$param$fraction.vaccinated) { return( dat$vacc_model$agents ) };
  
  is.too.recently.vaccinated <- ((at - getMostRecentVaccinationDate(dat)) > 
                                   dat$param$revaccination.eligibility.days);
  
  #  Identify eligible_patients: eligible for care, not vaccinated, not infected
  #  by default, all agents eligible for care, unless specified otw
  # never been vaccinated
  eligible_indices1 <- which( isUninfectedByAgent( dat ) &
                                !is.vaccinated.by.agent &
                                isEligibleByAgent( dat ) );
  # previously vaccinated
  eligible_indices2 <- which( isUninfectedByAgent( dat ) &
                                is.previously.vaccinated.by.agent &
                                !is.too.recently.vaccinated &
                                isEligibleByAgent( dat ) )
  
  eligible_indices <- c( eligible_indices1, eligible_indices2 );
  
  # if no agents eligible, end fxn
  if (length(eligible_indices) == 0) { return(dat) };
  
  #  calculate how many agents should be newly vaccinated at this time, based on user-specified vaccination rate
  
  ## This is using the calculuated per-day rate. [NOTE THERE WAS A BUG: It was using the per-day odds!] -- 
  ## but ok now that this is the per-day rate, then this is a small number of people.. it's the per-day number 
  ##being vaccinated, which is correct.
  num.newly.vaccinated.today <- sum( rbinom( length( getAliveAgents( dat ) ), 1, dat$param$daily.vaccination.rate ) );
  
  if (num.newly.vaccinated.today == 0) { return( dat ) };
  
  # if number of eligible agents exceeds number permissible, randomly choose subset
  # if the %coverage in total population alive exceeds #eligible, vaccinate all eligible
  if (num.newly.vaccinated.today < length( eligible_indices)) {
    vaccinated_indices <- sample( eligible_indices, num.newly.vaccinated.today );
  } else {
    vaccinated_indices <- eligible_indices;
  }
  
  dat <- setPhi( dat, 1, vaccinated_indices );
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
  phi.values <- getPhi(dat)
  
  # Each agent is either vaccinated, unvaccinated, or previously vaccinated (and presently unvaccinated)
  is.vaccinated.by.agent <- (!is.na(phi.values) & (phi.values == 1))
  is.unvaccinated.by.agent <- is.na( phi.values )
  is.previously.vaccinated.by.agent <- (!is.na(phi.values) & (phi.values == 0))
  stopifnot( all( as.numeric( is.vaccinated.by.agent + is.unvaccinated.by.agent + is.previously.vaccinated.by.agent ) == 1 ) )
  
  ## NOTE/THOUGHT:
  ## This builds in that they are not already infected; later we
  ## might consider that there is some state of knowledge of infected
  ## status, which might differ from actual infected status (eg
  ## when enrollment is within the window period of the
  ## diagnostic assay; this is a common enough occurrance it's
  ## important in our downstream work to distinguish sometimes
  ## -- probably not important here, but just noting this for
  ## the record). For preserving existing code it'd probably be better to keep 
  ## Status meaning known-status, and add some delay from exposure to the time at 
  ## which you update to Status==1.
  
  # Here phi is just turning from 1 to 0 with some small probability; I like that, as 
  ## it is Markov and should obviate the need to store the date of vaccination.
  vacc_indices <- which( is.vaccinated.by.agent & isUninfectedByAgent( dat ) );
  if (length( vacc_indices) > 0) {
    # With a small probability each day we might switch a person to being (effectively) 
    # not vaccinated (aka no vaccine-induced protection).
    new_values <- rbinom( length( vacc_indices ), 1, 1 - dat$param$daily.vaccine.reversion.rate );
    dat <- setPhi( dat, new_values, vacc_indices );
  }
  return( dat );
} # update_phi (..)

#' @export
draw_m <- function(dat, at, ...) {
  inf_indices <- getAgentsPotentiallyInfectedThisTimestep( dat, at );
  transmitter.mu <- getMu( dat, inf_indices );
  
  ## transmitter.mu for the basic model is an index into 
  ## names( vaccine.efficacy.by.mark ) (0-indexed, so add 1). 
  ## That is, 0 means sensitive, and 1 means resistant.
  mark.index <- round( transmitter.mu ) + 1;
  stopifnot( mark.index %in% seq_along( names( vaccine.efficacy.by.mark ) ) );
  
  dat$vacc_model$agents[[ "m" ]][ inf_indices ] <- names( dat$param$vaccine.efficacy.by.mark )[ mark.index ];
  
  dat$vacc_model$agents[[ "mu" ]][ inf_indices ] <- transmitter.mu;
  dat$vacc_model$agents[[ "sigma" ]][ inf_indices ] <- 0;
  
  return( dat );
} # draw_m (..)

# theta is the vaccine-induced probability of avoiding an otherwise-infecting exposure
#' @export
calculate_theta <- function(dat, at) {
  # For vaccinated folks, it's the vaccine efficacy for the exposing/potentially infecting mark.
  phi.values <- getPhi( dat );
  
  # Each agent is either vaccinated, unvaccinated, or previously vaccinated (and presently unvaccinated)
  is.vaccinated.by.agent <- (!is.na(phi.values) & (phi.values == 1));
  
  num.alive.and.vaccinated <-
    sum(is.vaccinated.by.agent & (isInfectedByAgent(dat) | isUninfectedByAgent(dat)));
  num.alive <-
    sum((isInfectedByAgent(dat) | isUninfectedByAgent(dat)));
  stopifnot( num.alive == length( getAliveAgents( dat ) ) );
  
  m <- getM( dat, at );
  
  # Set theta for everyone, to ensure that no-longer-vaccinated folks get it set back to 0.
  theta <- rep( 0, length( is.vaccinated.by.agent ) );
  
  # Note m is a string index into names( vaccine.efficacy.by.mark ).
  theta[ is.vaccinated.by.agent ] <- dat$param$vaccine.efficacy.by.mark[ m[ is.vaccinated.by.agent ] ];
  
  dat <- setTheta( dat, at, theta );
  
  return( dat );
} # calculate_theta (..)

#' @export
update_mu_and_sigma <- function(dat, at) {
  dat <- update_mu( dat, at );
  dat <- update_sigma( dat, at );
  
  return( dat );
}

#' @export
initialize_and_update_phi <- function(dat, at) {
  
  if (at < (dat$param$vaccine.rollout.year * 365)) { return(dat) };
  
  # Note this looks odd, but these are vectorized functions and so we first update 
  # those already initialized at a previous time step, then we initialize those newly 
  # vaccinated. We could have done it either way 'round, but these fns are presently 
  # written to not check that the update is not applying to someone just vaccinated. 
  # So for now this has to be done in this order:
  dat <- update_phi(dat, at);
  dat <- initialize_phi(dat, at);
  
  return( dat );
}

#' @export
initialize_vaccine_agents <- function(dat, at) {
  # browser()
  if (at == dat$param$evonet.initialization.timestep) {
    #create agent object (list of lists attached to dat)
    agent_template <- createAgent();
    num.current_agents <- length( dat$pop$Status );
    dat$vacc_model$agents <- vector( 'list', length = num.current_agents );
    
    ## CHECK: why use this instead of dat$attr?
    for (x in seq_along(dat$vacc_model$agents)) { dat$vacc_model$agents[[x]] <- agent_template }
    
    #if start of model initialize mu/sigma or initialize for new agents  
    dat <- update_mu_and_sigma(dat, at);
  } else {
    # Detect that new agents need to be added to the agents list.
    if (length(dat$pop$Status) > length(dat$vacc_model$agents)) {
      #if(at>500) browser()
      agent_template <- createAgent();
      total_new_agents <- length( dat$pop$Status ) - length( dat$vacc_model$agents );
      new_agents_indices <- (length(dat$vacc_model$agents) + 1):(length(dat$vacc_model$agents) + total_new_agents)
      for (x in new_agents_indices) { dat$vacc_model$agents[[ x ]] <- agent_template }
    }
  }
  
  stopifnot(length( dat$pop$Status) == length(dat$vacc_model$agents));
  
  return( dat );
}
