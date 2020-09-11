## Create a set of functions for a vaccine model, in an environment to be attach()ed.

## Basic; to add variations, you can modify the result. For now this doesn't create any mark variation (mu,sigma). TODO: ADD THAT STUFF. FOR NOW THIS IS MODEL 1.
create.basic.vaccination.model <- function (
  num.years = 15, # Could we instead get dat passed in and glean the number of years from it?
  fraction.vaccinated = 0.60,
  vaccine.rollout.year = 5,
  vaccine.rollout.duration.years = 1,
  vaccine.efficacy.years = 3,

  ## TODO: It looks like we can remove this datum. .. maybe the idea is that for each person it stores when they started .. but don't we use phi for that? [UPDATE This will be phi[2], or maybe something new about intervention_strategy_parameters, see below.]
  start_vacc_campaign = (vaccine.rollout.year*365):(num.years*365),    # all vaccine models
  max_perc_vaccinated = fraction.vaccinated, #all vacc. models, maximum percent/proportion of population to be vaccinated
  perc_vaccinated_rate = (max_perc_vaccinated /(1-max_perc_vaccinated ))/(years_to_max_coverage*365), ### NOTE I think this is a bug; why is this dividing by 1-max_perc_vacccinated? I think we just remove that to fix this.
  vacc_eff_duration = 365*vaccine.efficacy.years,

  vaccine.efficacy.by.mark = c( "sensitive" = 0.8 ),  #models 1/1b/2/2b, proportion (percentage) decrease in trans probs due to vaccine for vaccine model "1" (baseline vaccine model),
  mark.distribution = c( "sensitive" = 1 ), ## TODO: This is an idea I'm working on, so I can make one constructor for all existing models 1,1a,2,and 2a, and any discrete-mark variant of that.

  vacc_trans_prob_decrease = vaccine.efficacy.by.mark[ "sensitive" ]
) {
    update_mu <- function ( dat, at ) {
      if(at==2){
        #initial infecteds at model start
        inf_index <- which(dat$pop$Status==1)
        ## NOTE: THIS USED TO BE based on dat$pop$virus_sens_vacc, which was the old vaccination model.
        ## OLD
        # mu_values <- dat$pop$virus_sens_vacc[inf_index] ## TODO: This is tying us to the underlying / older vaccine model which had hardcoded the concept of "sensitive" and "resistant"
        if( mark.distribution[ "sensitive" ] == 1.0 ) {
            mu_values <- rep( 1, length( inf_index ) );
        } else if( mark.distribution[ "sensitive" ] == 0.0 ) {
            mu_values <- rep( 0, length( inf_index ) );
        } else {
            mu_values <- rbinom( length( inf_index ), 1,
                                prob=mark.distribution[ "sensitive" ] );
        }
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
          # Do nothing.
      }
      return( dat$vacc_model$agents )
    } # update_sigma (..)

    ## PAUL NOTES: This should be setting phi, but it is accessing it, set elsewhere!
    initialize_phi <- function ( dat, at ) {
      #current phi values. For model 1 these are just "vaccinated" or not.

        ## PAUL ASKS: Is this different from some other length of vectors in dat? If so, why?
       index <- 1:length(dat$vacc_model$agents)

       ## Here phi for each person is just 1 or 0 (1 is vaccinated) so this as.numeric operation is safe. For more complex phi structures, this could lose structural integrity.
       phi_values <-
           as.numeric(lapply(index,function(x) dat$vacc_model$agents[[x]]$phi))
       
       ### "Status" records "infected" status, which seems to be the case, see just above where inf_index is defined as which( Status == 1 ). But also it seems to have a negative value for folks not in the population -- maybe these are dead or not yet living folks?

       ## HERE we are relying on dat$param$max_perc_vaccinated where I think we should not -- that's part of the old model. BUT we apparently are here using phi == 1 to mean "vaccinated".

      #if designated vacc. level already reached (percent of pop vaccianted), don't vacc anymore
      if(length(which(phi_values == 1 & dat$pop$Status>=0))/length(which(dat$pop$Status>=0)) > dat$param$max_perc_vaccinated){return(dat)}
n      
      # Identify eligible_patients: eligible for care, not vaccinated, not infected
      # by default, all agents eligible for care, unless specified otw
      #note:dat$pop$phi == 0 is an agent whose vaccine effect ended (waned)
      
       ## HERE we are relying on dat$param$eligible_care and dat$pop$vacc_init_time and dat$param$vacc_eff_duration where I think we should not -- that's part of the old model.

### YOINKS. It does look as if using eligible_care could keep some other things possible, so we might as well -- but that would go into determining whether someone is vaccinated, only, correct?

       ## WOAH, new info: phi is NA for never been vaccinated, and 0 for previously vaccinated... ok.

      #never been vaccinated
      eligible_index1 <- which(dat$pop$Status == 0 &
                                 is.na(phi_values) &
                                 dat$pop$eligible_care == 1)


       ### NOTE: It's not clear what 
       ### NOTE: vacc_eff_duration seems to be used both as the _mean_ time to efficacy ending, and the time at which a person becomes eligible to become revaccinated. These seem to not be the same thing and I recommend that we have two parameters for this; one for the mean time to the vaccine waning to non-efficacy, and the other is eligibility for revaccination -- for one thing, in a vaccine trial nobody is eligible for revaccination; in a post-rollout setting, it's not clear what the recommendation would be but I don't think we can assume it's the mean duration of efficacy. For instance, it might be the _minimum_ duration of efficacy, or something calculated to minimize population-level risk. I AM STILL DECIDING WHETHER TO STORE vacc_init_time in phi. I THINK NOT. But there's nothing prohibiting it! Eg the model could use calendar time.  But if it is just for keeping track of eligibility for revaccination, we can handle that with a count-down instead of a count-up. Eg phi[2] could store a countter set to the guidelines eg "2 years" for revaccination, and then the update could decrement it.  The diff is that storing the calendar time is more versatile and informative for documentation purposes. So if I'm going to do that, I might as well store it in phi. OK. Hmm. Another notion is that phi should store the immune status... hmm. but I guess it can store everything relevant to agent.  Or .. can we add another named parameter subset like phi, for things like vacc_start_date that could be more relevant to the machinery controlling the vaccination? I guess here they are interdependent - but if I make separate factories for altering vaccination strategies, I'll want this to be not tied too tightly.  So I'll go with something like divvying up the parameters to what the modules will want to work with. phi can be the vaccine response parameters, whereas something else can store the vaccination strategy parameters, and these need not be tied. Let's do that, and name things better. phi can be parameters_modifying_infection_probability_and_post_infection_evolutionary_dynamics. But maybe that's not a distinction worth making? I think that's it. We do need to support modularity. Hmm. mu/sigma is stored separately.  Hmm. ... but still that's the typical "covariate" vs "mark" distinction, which is reasonable, and the type constraints of mu/sigma guided that decision to keep it separate; really it could be a component of phi, it doesn't matter. It's more like "agent" is a list with a bunch of stuff, including a vector of immune status parameters, phi, and a vector of other parameters.

      #previously vaccinated
      eligible_index2 <- which(dat$pop$Status == 0 &
                                 phi_values == 0 &
                                 (at-dat$pop$vacc_init_time) > dat$param$vacc_eff_duration &
                                 dat$pop$eligible_care == 1)
      
      eligible_index <- c(eligible_index1,eligible_index2)
      
      #if no agents eligible, end fxn
      if(length(eligible_index)==0){return(dat)}
      
       ## HERE we are relying on dat$param$perc_vaccinated_rate where I think we should not -- that's part of the old model.

      #calculate how many agents can be vaccinated, based on user-specified vaccination rate

       ## This is using the calculuated per-day rate. [ONLY THERE'S A BUG: It's using the per-day odds!] -- but ok if that were the per-day rate, then this is a small number of people.. it's the per-day number being vaccinated, which is correct.
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
      if(at > dat$param$start_vacc_campaign[1] ) { ## this seems unnecessary but ok, maybe for speed?
        index <- 1:length(dat$vacc_model$agents)
        phi_values <- as.numeric(lapply(index,function(x) dat$vacc_model$agents[[x]]$phi))

        ## This builds in that they are not already infected; later we
        ## might consider that there is some state of knowledge of infected
        ## status, which might differ from actual infected status (eg
        ## when enrollment is within the window period of the
        ## diagnostic assay; this is a common enough occurrance it's
        ## important in our downstream work to distinguish sometimes
        ## -- probably not important here, but just noting this for
        ## the record). For preserving existing code it'd probably be better to keep Status meaning known-status, and add some delay from exposure to the time at which you update to Status==1.

        # Here phi is just turning from 1 to 0 with some small probability; I like that, as it is Markov and should obviate the need to store the date of vaccination.
        vacc_index <- which(phi_values == 1 & dat$pop$Status == 0);
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

    # theta is the vaccine-induced probability of avoiding an otherwise-infecting exposure
    calculate_theta <- function ( dat, m ) { # m is mark, which here is ignored.
    
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
    
    initialize_and_update_phi <- function ( dat, at ) {
      
      if(at<dat$param$start_vacc_campaign[1]){return(dat)}
      if(!is.element(at,dat$param$start_vacc_campaign)){return(dat)}
    
      ## PAUL NOTES that this will clobber phi every time (by initializing it again after updating it)! ACTUALLY not -- this "initialize" (and update) are both vector functions, so you initialize some folks and update other folks -- it's not actually contradictory.
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
      
      ### This is assuming that new agents are always indexed after pre-existing agents, which sounds reasonable.

      #if start of model initialize mu/sigma or initialize for new agents   
      if(at>2 & length(dat$pop$Status) > length(dat$vacc_model$agents)){
        #if(at>500) browser()
        agent_list <- list(phi=NA,mu=NA,sigma=NA) # See there's a problem here -- we need to use a constructor concept I think, so we don't have type mismatch issues downstream. eg make_agent_list_template (since this is actually a template for the list). A different, maybe safer way to do this is just glean the columns (the template) by taking a row from the existing agent_list.  It seems like we should do all this with functions to ensure extensibility, eg add_new_agents_to_agent_list(..)
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
