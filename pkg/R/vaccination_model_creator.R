## Create a set of functions for a vaccine model, in an environment to be attach()ed.

## Basic; to add variations, you can modify the result. For now this doesn't create any mark variation (mu,sigma). TODO: ADD THAT STUFF. FOR NOW THIS IS MODEL 1.
create.basic.vaccination.model <- function (
  fraction.vaccinated = 0.60,
  vaccine.rollout.year = 5,
  vaccine.rollout.duration.years = 1,
  vaccine.efficacy.years = 3,
  revaccination.eligibility.years = 3,

  vaccine.efficacy.by.mark = c( "sensitive" = 0.8 ),  #models 1/1b/2/2b, proportion (percentage) decrease in trans probs due to vaccine for vaccine model "1" (baseline vaccine model),
  initial.mark.distribution = c( "sensitive" = 1 ) ## TODO: This is an idea I'm working on, so I can make one constructor for all existing models 1,1a,2,and 2a, and any discrete-mark variant of that.

) {
    ## mark.distribtion is a simplex for the parameters of the categorical distribution, and has the same entries as vacine.efficacy.by.mark, to denote their starting distributions.
    stopifnot( length( vaccine.efficacy.by.mark ) == length( initial.mark.distribution ) );
    stopifnot( all( sort( names( vaccine.efficacy.by.mark ) ) = sort( names( initial.mark.distribution ) ) ) );
    # Distribution of marks must sum to 1.
    stopifnot( all.equal( sum( initial.mark.distribution ), 1, tol = 1E-8 ) ); # TODO: Replace magic # for tolerance, maybe, using machine epsilon.

    ### NOTE I think this was previously a bug; it was dividing by 1-max_perc_vacccinated, making it an odds, not a probability.
    daily.vaccination.rate <- fraction.vaccinated / ( vaccine.rollout.duration.years * 365 );
    vaccine.efficacy.days <- 365*vaccine.efficacy.years;
    # Note that this makes vaccine.efficacy.years the _average_ duration of efficacy; it is negative-binomial/geometric.
    daily.vaccine.reversion.rate <- 1 / ( vaccine.efficacy.years * 365 );
    revaccination.eligibility.days <- 365*revaccination.eligibility.years;

    ## Evonet Values.
    evonet.initialization.timestep <- 2;
    isEvonetInitializationTimestep <- function ( at ) { at == evonet.initialization.timestep }
    isInfectedByAgent <- function ( dat ) { ( dat$pop$Status == 1 ) };
    isUninfectedByAgent <- function ( dat ) { ( dat$pop$Status == 0 ) };

    isEligibleByAgent <- function ( dat ) { ( dat$pop$eligible_care == 1 ) }

    getInfectedAgents <- function ( dat ) { which( dat$pop$Status == 1 ) };
    getAliveAgents <- function ( dat ) { which( dat$pop$Status >= 0 ) };

    getAgentsJustInfectedLastTimestep <- function ( dat, at ) { which( dat$pop$Time_Inf ==(at-1) & ( dat$pop$Status == 1 ) ) }

    getAgentsPotentiallyInfectedThisTimestep <- function ( dat, at ) { dat$discord_coital_df$inf_id }
    getTransmittingPartners <- function ( dat, inf_indices ) { dat$pop$Donors_Indices[ inf_indices ] }

    ## Vacc Model Agents Values.
    createAgent <- function () {
        the.agent <- list(
            phi = NA,
            mu = NA,
            sigma = NA,
            m = NA, # formerly known as mu_orig
            theta = 0, # By default for unvaccinated folks, we will multiply the infection probability by 1 == (1 - theta)
            vaccination.dates.stack = list()
        );
        return( the.agent );
    } # createAgent ()
    ## Vacc Model Agents Values getters/setters:
    getVaccModelAgentsValue <- function ( dat, key, indices = NULL ) {
        stopifnot( key %in% names( dat$vacc_model$agents ) );
        if( is.null( indices ) || ( length( indices ) == 0 ) || ( ( length( indices ) == 1 ) && is.na( indices ) ) ) {
            indices <- 1:length( dat$vacc_model$agents );
        }
        rv <- rep( 0, length( dat$vacc_model$agents ) );
        for( x in seq_along( indices ) ) { rv[ x ] <- dat$vacc_model$agents[[ indices[ x ] ]]$mu }
        return( rv );
    } # getVaccModelAgentsValue(..)
    setVaccModelAgentsValue <- function ( dat, key, value, indices = NULL ) {
        stopifnot( key %in% names( dat$vacc_model$agents ) );
        if( is.null( indices ) || ( length( indices ) == 0 ) || ( ( length( indices ) == 1 ) && is.na( indices ) ) ) {
            indices <- 1:length( dat$vacc_model$agents );
        }
        for( x in seq_along( indices ) ) { dat$vacc_model$agents[[ indices[ x ] ] ][[ key ]] <- value[ x ]; }
        return( dat );
    } # setVaccModelAgentsValue(..)
    getMu <- function ( dat, indices = NULL ) {
        getVaccModelAgentsValue( dat, "mu", indices )
    }
    setMu <- function ( dat, mu_values, indices = NULL ) {
        setVaccModelAgentsValue( dat, "mu", mu_values, indices )
    }
    getSigma <- function ( dat, indices = NULL ) {
        getVaccModelAgentsValue( dat, "sigma", indices )
    }
    setSigma <- function ( dat, sigma_values, indices = NULL ) {
        setVaccModelAgentsValue( dat, "sigma", sigma_values, indices )
    }
    getPhi <- function ( dat, indices = NULL ) {
        getVaccModelAgentsValue( dat, "phi", indices )
    }
    setPhi <- function ( dat, phi_values, indices = NULL ) {
        setVaccModelAgentsValue( dat, "phi", phi_values, indices )
    }
    getM <- function ( dat, indices = NULL ) {
        getVaccModelAgentsValue( dat, "m", indices )
    }
    setM <- function ( dat, m_values, indices = NULL ) {
        setVaccModelAgentsValue( dat, "m", m_values, indices )
    }
    getTheta <- function ( dat, indices = NULL ) {
        getVaccModelAgentsValue( dat, "theta", indices )
    }
    setTheta <- function ( dat, theta_values, indices = NULL ) {
        setVaccModelAgentsValue( dat, "theta", theta_values, indices )
    }
    getMostRecentVaccinationDate <- function ( dat, indices = seq_along( dat$vacc_model$agents ) ) {
        ## We store vaccination dates as a stack (last in, first out, so the first element is always the most recent one).
        vaccination.dates.stack <- getVaccModelAgentsValue( dat, "vaccination.dates.stack", indices );
        return( vaccination.dates.stack[ 1 ] );
    } # getMostRecentVaccinationDate (..)
    addVaccinationDate <- function ( dat, vaccination_dates, indices = seq_along( dat$vacc_model$agents ) ) {
        ## We store vaccination dates as a stack (last in, first out, so the first element is always the most recent one).
        for( x in seq_along( indices ) ) {
            vaccination.dates.stack.x <-
                c( vaccination_dates[ x ], dat$vacc_model$agents[[ indices[ x ] ] ][[ "vaccination.dates.stack" ]] );
            dat$vacc_model$agents[[ indices[ x ] ] ][[ "vaccination.dates.stack" ]] <- vaccination.dates.stack.x;
        }
        return( dat );
    } # addVaccinationDate (..)

    # This is set up to work for our default configuration, which is the one-mark or two-mark models, with "sensitive" always present, and maybe a second option (its name doesn't matter here).
    update_mu <- function ( dat, at ) {
      if( isEvonetInitializationTimestep( at ) ) {
        # initial infecteds at model start
        inf_indices <- getInfectedAgents( dat );

        first.mark.name <- names( vaccine.efficacy.by.mark );

        if( initial.mark.distribution[ first.mark.name ] == 1.0 ) {
            mu_values <- rep( 1, length( inf_indices ) );
        } else if( initial.mark.distribution[ first.mark.name ] == 0.0 ) {
            mu_values <- rep( 0, length( inf_indices ) );
        } else {
            ## TODO: expand to allow more than two marks by modifying this:
            mu_values <- rbinom( length( inf_indices ), 1,
                                prob = initial.mark.distribution[ first.mark.name ] );
        }
        setMu( dat, mu_values, inf_indices );
      } else {
        # secondary infections from previous timestep
        inf_indices <- getAgentsJustInfectedLastTimestep( dat, at );

        if( length( inf_indices ) > 0 ) {
          donor_indices <- getTransmittingPartners( dat, inf_indices );
          mu_values <- getMu( dat, donor_indices );
          dat <- setMu( dat, mu_values, inf_indices );
        }
      }
      return( dat );
    } # update_mu (..)

    update_sigma <- function ( dat, at ) {
      if( isEvonetInitializationTimestep( at ) ){
        #initial infecteds at model start
        inf_indices <- getInfectedAgents( dat );
        sigma_values <- rep( 0, length( inf_indices ) );
        dat <- setSigma( dat, sigma_values, inf_indices );
      } else {
          # Do nothing.
      }
      return( dat );
    } # update_sigma (..)

    # This and the other functions are presently vectorized.
    # This is run daily to vaccinate new people.
    initialize_phi <- function ( dat, at ) {
        phi.values <- getPhi( dat );

        # Each agent is either vaccinated, unvaccinated, or previously vaccinated (and presently unvaccinated)
        is.vaccinated.by.agent <- ( !is.na( phi.values ) & ( phi.values == 1 ) );
        is.unvaccinated.by.agent <- is.na( phi.values );
        is.previously.vaccinated.by.agent <- ( !is.na( phi.values ) & ( phi.values == 0 ) );
        stopifnot( all( as.numeric( is.vaccinated.by.agent + is.unvaccinated.by.agent + is.previously.vaccinated.by.agent ) == 1 ) )

        num.alive.and.vaccinated <
            sum( is.vaccinated.by.agent & ( isInfectedByAgent( dat ) | isUninfectedByAgent( dat ) ) );
        num.alive <-
            sum( ( isInfectedByAgent( dat ) | isUninfectedByAgent( dat ) ) );
        stopifnot( num.alive == length( getAliveAgents( dat ) ) );

        #  if designated vacc. level already reached (percent of pop vaccianted), don't vacc anymore
        if( ( num.alive.and.vaccinated / num.alive ) > fraction.vaccinated ) { return( dat$vacc_model$agents ) };

        is.too.recently.vaccinated <- ( ( at - getMostRecentVaccinationDate( dat ) ) > revaccination.eligibility.days );

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
        if( length( eligible_indices ) == 0 ) { return( dat ) };
        
        #  calculate how many agents should be newly vaccinated at this time, based on user-specified vaccination rate

        ## This is using the calculuated per-day rate. [NOTE THERE WAS A BUG: It was using the per-day odds!] -- but ok now that this is the per-day rate, then this is a small number of people.. it's the per-day number being vaccinated, which is correct.
        num.newly.vaccinated.today <- sum( rbinom( length( getAliveAgents( dat ) ), 1, daily.vaccination.rate ) );

        if( num.newly.vaccinated.today == 0 ) { return( dat ) };
        
        # if number of eligible agents exceeds number permissible, randomly choose subset
        # if the %coverage in total population alive exceeds #eligible, vaccinate all eligible
        if( num.newly.vaccinated.today < length( eligible_indices ) ) {
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
    update_phi <- function ( dat, at ) {
        phi.values <- getPhi( dat );

        # Each agent is either vaccinated, unvaccinated, or previously vaccinated (and presently unvaccinated)
        is.vaccinated.by.agent <- ( !is.na( phi.values ) & ( phi.values == 1 ) );
        is.unvaccinated.by.agent <- is.na( phi.values );
        is.previously.vaccinated.by.agent <- ( !is.na( phi.values ) & ( phi.values == 0 ) );
        stopifnot( all( as.numeric( is.vaccinated.by.agent + is.unvaccinated.by.agent + is.previously.vaccinated.by.agent ) == 1 ) )

        ## NOTE/THOUGHT:
        ## This builds in that they are not already infected; later we
        ## might consider that there is some state of knowledge of infected
        ## status, which might differ from actual infected status (eg
        ## when enrollment is within the window period of the
        ## diagnostic assay; this is a common enough occurrance it's
        ## important in our downstream work to distinguish sometimes
        ## -- probably not important here, but just noting this for
        ## the record). For preserving existing code it'd probably be better to keep Status meaning known-status, and add some delay from exposure to the time at which you update to Status==1.

        # Here phi is just turning from 1 to 0 with some small probability; I like that, as it is Markov and should obviate the need to store the date of vaccination.
        vacc_indices <- which( is.vaccinated.by.agent & isUninfectedByAgent( dat ) );
        if( length( vacc_indices ) > 0 ) {
          # With a small probability each day we might switch a person to being (effectively) not vaccinated (aka no vaccine-induced protection).
          new_values <- rbinom( length( vacc_indices ), 1, 1 - daily.vaccine.reversion.rate );
          dat <- setPhi( dat, new_values, vacc_indices );
        }
      }
      return( dat );
    } # update_phi (..)

    draw_m <- function ( dat, at, ... ) {
        inf_indices <- getAgentsPotentiallyInfectedThisTimestep( dat, at );
        transmitter.mu <- getMu( dat, inf_indices );

        ## transmitter.mu for the basic model is an index into names( vaccine.efficacy.by.mark ) (0-indexed, so add 1). That is, 0 means sensitive, and 1 means resistant.
        mark.index <- round( transmitter.mu ) + 1;
        stopifnot( mark.index %in% seq_along( names( vaccine.efficacy.by.mark ) ) );

        dat$vacc_model$agents[[ "m" ]][ inf_indices ] <- names( vaccine.efficacy.by.mark )[ mark.index ];

        dat$vacc_model$agents[[ "mu" ]][ inf_indices ] <- transmitter.mu;
        dat$vacc_model$agents[[ "sigma" ]][ inf_indices ] <- 0;

        return( dat );
    } # draw_m (..)

    # theta is the vaccine-induced probability of avoiding an otherwise-infecting exposure
    calculate_theta <- function ( dat, at ) {
        # For vaccinated folks, it's the vaccine efficacy for the exposing/potentially infecting mark.
        phi.values <- getPhi( dat );

        # Each agent is either vaccinated, unvaccinated, or previously vaccinated (and presently unvaccinated)
        is.vaccinated.by.agent <- ( !is.na( phi.values ) & ( phi.values == 1 ) );

        num.alive.and.vaccinated <
            sum( is.vaccinated.by.agent & ( isInfectedByAgent( dat ) | isUninfectedByAgent( dat ) ) );
        num.alive <-
            sum( ( isInfectedByAgent( dat ) | isUninfectedByAgent( dat ) ) );
        stopifnot( num.alive == length( getAliveAgents( dat ) ) );

        m <- getM( dat, at );

        # Set theta for everyone, to ensure that no-longer-vaccinated folks get it set back to 0.
        theta <- rep( 0, length( is.vaccinated.by.agent ) );
        theta[ is.vaccinated.by.agent ] <- vaccine.efficacy.by.mark[ m[ is.vaccinated.by.agent ] ];
        dat <- setTheta( dat, at, theta );

        return( dat );
    } # calculate_theta (..)

    update_mu_and_sigma <- function ( dat, at ) {
      dat <- update_mu( dat, at );
      dat <- update_sigma( dat, at );

      return( dat );
    } # update_mu_and_sigma (..)
    
    initialize_and_update_phi <- function ( dat, at ) {
      
      if( at < ( vaccine.rollout.year * 365 ) ) { return( dat ) };

      # Note this looks odd, but these are vectorized functions and so we first update those already initialized at a previous time step, then we initialize those newly vaccinated. We could have done it either way 'round, but these fns are presently written to not check that the update is not applying to someone just vaccinated. So for now this has to be done in this order:
      dat <- update_phi( dat, at );
      dat <- initialize_phi( dat, at );

      return( dat );
    }
    
    initialize_vaccine_agents <- function ( dat, at ) {
      
      if( isEvonetInitializationTimestep( at ) ) {
        #create agent object (list of lists attached to dat)
        agent_template <- createAgent();
        num.current_agents <- length( dat$pop$Status );
        dat$vacc_model$agents <- vector( 'list', length = num.current_agents );
        for( x in seq_along( dat$vacc_model$agents ) ) { dat$vacc_model$agents[[ x ]] <- agent_template }

        #if start of model initialize mu/sigma or initialize for new agents  
        dat <- update_mu_and_sigma( dat, at );
      } else {
          # Detect that new agents need to be added to the agents list.
          if( length( dat$pop$Status ) > length( dat$vacc_model$agents ) ) {
            #if(at>500) browser()
            agent_template <- createAgent();
            total_new_agents <- length( dat$pop$Status ) - length( dat$vacc_model$agents );
            new_agents_indices <- (length(dat$vacc_model$agents)+1):(length(dat$vacc_model$agents) + total_new_agents)
            for( x in seq_along( new_agents_indices ) ) { dat$vacc_model$agents[[ x ]] <- agent_template }
          }
      }

      stopifnot( length( dat$pop$Status ) == length( dat$vacc_model$agents ) );

      return( dat );
    } # initialize_vaccine_agents (..)

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
