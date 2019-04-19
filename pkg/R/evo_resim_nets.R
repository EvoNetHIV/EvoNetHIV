#' @export
evo_resim_nets <- function(dat, at) {
  #originally written by Skye B.
  #https://github.com/statnet/EpiModel/blob/fast_edgelist/R/net.mod.simnet.R
  
  idsActive <- which(dat$attr$active == 1)
  anyActive <- ifelse(length(idsActive) > 0, TRUE, FALSE)
  if (dat$param$modes == 2) {
    nActiveM1 <- length(intersect(modeids(dat$nw, mode = 1), idsActive))
    nActiveM2 <- length(intersect(modeids(dat$nw, mode = 2), idsActive))
    anyActive <- ifelse(nActiveM1 > 0 & nActiveM2 > 0, TRUE, FALSE)
  }
  
  # Pull network model parameters
  nwparam <- get_nwparam(dat)
  
  # Serosorting model check
  statOnNw <- ("status" %in% get_formula_terms(nwparam$formation))
  status <- dat$attr$status
  if (statOnNw == TRUE && length(unique(status)) == 1) {
    stop("Stopping simulation because status in formation formula and ",
         "no longer any discordant nodes",
         call. = TRUE)
  }
  
  # Set up nwstats df
  if (dat$control$save.nwstats == TRUE) {
    if (at == 2) {
      nwstats <- attributes(dat$nw)$stats
      dat$stats$nwstats <- as.data.frame(nwstats)
    }
  }
  
  # Network simulation
  if (anyActive > 0 & dat$control$depend == TRUE) {
    
    if (!is.null(dat[['nw']])) {
      
      
      # in network mode
      suppressWarnings(
        
        dat$nw <- simulate(dat$nw,
                           formation = nwparam$formation,
                           dissolution = nwparam$coef.diss$dissolution,
                           coef.form = nwparam$coef.form,
                           coef.diss = nwparam$coef.diss$coef.adj,
                           constraints = nwparam$constraints,
                           time.start = at,
                           time.slices = 1,
                           time.offset = 0,
                           monitor = dat$control$nwstats.formula,
                           control = dat$control$set.control.stergm))
      
      # Set up nwstats df
      if (dat$control$save.nwstats == TRUE) {
        dat$stats$nwstats <- rbind(dat$stats$nwstats,
                                   tail(attributes(dat$nw)$stats, 1))
      }
      
      if (dat$control$delete.nodes == TRUE) {
        dat$nw <- network.extract(dat$nw, at = at)
        inactive <- which(dat$attr$active == 0)
        dat$attr <- deleteAttr(dat$attr, inactive)
      }
    } else {
      
      # construct the list of model statistics input vectors
      dat <- tergmLite::updateModelTermInputs(dat)
      
      # fast version of tergm::simulate.network
      dat$el[[1]] <- tergmLite::simulate_network(p = dat$p[[1]],
                                                 el = dat$el[[1]],
                                                 coef.form = nwparam$coef.form,
                                                 coef.diss = nwparam$coef.diss$coef.adj,
                                                 save.changes = TRUE)
      
    }
  }
  
  return(dat)
}
