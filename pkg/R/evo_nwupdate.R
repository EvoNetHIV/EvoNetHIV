#very slight modification to EpiModel's "nwupdate.net"

#' @export
evo_nwupdate<-function (dat, at) 
{
  type <- get_control(dat, "type", override.null.error = TRUE)
  tergmLite <- get_control(dat, "tergmLite")
  status <- get_attr(dat, "status")
  infTime <- get_attr(dat, "infTime")
  active <- get_attr(dat, "active")
  entrTime <- get_attr(dat, "entrTime")
  exitTime <- get_attr(dat, "exitTime")
  resimulate.network <- get_control(dat, "resimulate.network")
  arrivals <- which(active == 1 & entrTime == at)
  departures <- which(active == 0 & exitTime == at)
  nArrivals <- length(arrivals)
  
  #note: moved departures section in front of arrivals section
  #due to bookkeeping issues between epimodel/nw indices and evonet indices
  if (tergmLite == TRUE) {
    dat <- delete_attr(dat, departures)
    dat$el[[1]] <- delete_vertices(dat$el[[1]], departures)
  }
  
  if (nArrivals > 0) {
    nwterms <- dat$temp$nwterms
    #if (!is.null(nwterms)) {
    #  curr.tab <- get_attr_prop(dat, nwterms)
    #  dat <- auto_update_attr(dat, arrivals, curr.tab)
    #}
    if (length(unique(sapply(dat$attr, length))) != 1) {
      stop("Attribute list of unequal length. Check arrivals.net module.\n", 
           print(cbind(sapply(get_attr_list(dat), length))))
    }
    if (tergmLite == FALSE) {
      dat$nw[[1]] <- add.vertices(dat$nw[[1]], nv = nArrivals)
      dat$nw[[1]] <- activate.vertices(dat$nw[[1]], onset = at, 
                                       terminus = Inf, v = arrivals)
      dat$nw[[1]] <- activate.vertex.attribute(dat$nw[[1]], 
                                               prefix = "testatus", value = status[arrivals], 
                                               onset = at, terminus = Inf, v = arrivals)
    }
    if (tergmLite == TRUE) {
      dat$el[[1]] <- add_vertices(dat$el[[1]], nv = nArrivals)
    }
  }
  #if (length(departures) > 0) {
  #  if (tergmLite == FALSE) {
  #    dat$nw[[1]] <- deactivate.vertices(dat$nw[[1]], onset = at, 
  #                                       terminus = Inf, v = departures, deactivate.edges = TRUE)
  #  }
  #}

  if (tergmLite == FALSE) {
    idsNewInf <- which(status == "i" & infTime == at)
    if (length(idsNewInf) > 0) {
      dat$nw[[1]] <- activate.vertex.attribute(dat$nw[[1]], 
                                               prefix = "testatus", value = "i", 
                                               onset = at, terminus = Inf, v = idsNewInf)
    }
  }
  if (tergmLite == FALSE) {
    if (type %in% c("SIS", "SIR") && !is.null(type)) {
      recovState <- ifelse(type == "SIR", "r", 
                           "s")
      attr.status <- which(status == recovState)
      nw.status <- which(get_vertex_attribute(dat$nw[[1]], 
                                              "status") == recovState)
      idsRecov <- setdiff(attr.status, nw.status)
      if (length(idsRecov) > 0) {
        dat$nw[[1]] <- activate.vertex.attribute(dat$nw[[1]], 
                                                 prefix = "testatus", value = recovState, 
                                                 onset = at, terminus = Inf, v = idsRecov)
      }
    }
  }
  if (tergmLite == FALSE & resimulate.network == TRUE) {
    dat <- copy_datattr_to_nwattr(dat)
  }
  
  #print(c(at,max(as.numeric(dat$el[[1]]))))
  #if(max(as.numeric(dat$el[[1]]))>length(dat$pop$Status)){browser()}
  
  return(dat)
}