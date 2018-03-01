#' @export

# This module modifies the network model coefficients following implementation of a vaccine campaign. Note that it is specific to a heterosexual model in which the nw_form_terms are of the order "~ edges + concurrent('sex') + addl terms".

update_nw_formula <- function(dat, at) {
  if(dat$param$risk_comp_degree) {
    if(at != dat$param$start_vacc_campaign[1]) { return(dat) } 
    else {
      ## Create network on which to estimate model
      nw <- network.initialize(10000, directed = F)
      
      ## Assign sex, age, and vaccination values to nodes
      sex  <- sample(c('f', 'm'), size = 10000, replace = T)
      age  <- sample(16:49, size = 10000, replace = T)
      vacc <- rbinom(n = 10000, size = 1, prob = 0.20) # 20% of population is vaccinated. This proportion is arbitrary and irrespective of specified target vaccine coverage; it is used only for network model estimation.
      nw <- set.vertex.attribute(nw, "sex", sex)
      nw <- set.vertex.attribute(nw, "age", age)
      nw <- set.vertex.attribute(nw, "vacc_rc", vacc)
      
      nw_form_terms <- "~ edges + nodefactor('vacc_rc') + concurrent('sex') + absdiffby('age', 'sex', 3, c('f', 'm')) + offset(nodematch('sex', diff = F))"
      
      nEdges         <- (dat$param$target_stats[1]/dat$param$initial_pop) * 10000
      fem_conc_prop  <- dat$param$target_stats[2] * 2/dat$param$initial_pop
      male_conc_prop <- dat$param$target_stats[3] * 2/dat$param$initial_pop
      
      target_stats  <- c(nEdges, (dat$param$risk_comp_degree_rr * (nEdges * 2)/10000) * sum(vacc), fem_conc_prop * sum(sex == 'f'), male_conc_prop * sum(sex == 'm'), nEdges)
      
      
      new_est_nw <- EpiModel::netest(nw = nw,
                                     formation = as.formula(nw_form_terms),
                                     target.stats = target_stats,
                                     coef.diss = dissolution_coefs(dissolution =  as.formula(evoparams$dissolution),
                                                                   duration    =  evoparams$relation_dur,
                                                                   d.rate      =  3e-05),
                                     constraints = as.formula(evoparams$nw_constraints),
                                     coef.form = evoparams$nw_coef_form,
                                     verbose = F,
                                     set.control.ergm = control.ergm(MCMLE.maxit = 100))
      
      browser()
      # Apply edges correction
      new_est_nw$coef.form[1] <- new_est_nw$coef.form[1] - log(sum(dat$pop$Status >= 0)) + log(10000)
      
      dat$nwparam <- list(new_est_nw[-which(names(new_est_nw) == "fit")])
      
      sim_nw <- as.network.matrix(dat$el[[1]], matrix.type='edgelist',
                                  directed = FALSE,
                                  bipartite = FALSE,
                                  loops = FALSE)
      
      p <- tergmLite::stergm_prep(network.collapse(as.networkDynamic(sim_nw), at = 1),
                                  new_est_nw$formation,
                                  new_est_nw$coef.diss$dissolution,
                                  new_est_nw$coef.form,
                                  new_est_nw$coef.diss$coef.adj,
                                  new_est_nw$constraints)
      p$model.form$formula <- NULL
      p$model.diss$formula <- NULL
      dat$p <- list()
      dat$p[[1]] <- p
      
      return(dat)
    }
  } else { return(dat) }
}