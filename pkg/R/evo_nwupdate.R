#very slight modification to EpiModel's "nwupdate.net"

#' @export
evo_nwupdate<- function(dat, at) {
  
  ## Attributes
  type <- get_control(dat, "type", override.null.error = TRUE)
  tergmLite <- get_control(dat, "tergmLite")
  status <- get_attr(dat, "status")
  infTime <- get_attr(dat, "infTime")
  active <- get_attr(dat, "active")
  entrTime <- get_attr(dat, "entrTime")
  exitTime <- get_attr(dat, "exitTime")
  
  # statOnNw <- "status" %in% dat$temp$nwterms
  resimulate.network <- get_control(dat, "resimulate.network")
  
  ## Vital Dynamics
  arrivals <- which(active == 1 & entrTime == at)
  departures <- which(active == 0 & exitTime == at)
  
  nArrivals <- length(arrivals)
  if (nArrivals > 0) {
    
    ## Arrivals
    nwterms <- dat$temp$nwterms
  #  if (!is.null(nwterms)) {
  #    curr.tab <- get_attr_prop(dat, nwterms)
  #    dat <- auto_update_attr(dat, arrivals, curr.tab)
  #  }
    
    if( (at %% (365*5)) == 0){
       if (length(unique(sapply(dat$attr, length))) != 1) {
       #stop("Attribute list of unequal length. Check arrivals.net module.\n",
           cat("Note: Attribute list of unequal length.\nPossibly due to non-numeric attributes, e.g., matrices")
               # print(cbind(sapply(get_attr_list(dat), length))))
       }
    }
    
    if (tergmLite == FALSE) {
      dat$nw[[1]] <- add.vertices(dat$nw[[1]], nv = nArrivals)
      dat$nw[[1]] <- activate.vertices(dat$nw[[1]], onset = at, terminus = Inf, v = arrivals)
      dat$nw[[1]] <- activate.vertex.attribute(dat$nw[[1]], prefix = "testatus",
                                               value = status[arrivals],
                                               onset = at, terminus = Inf,
                                               v = arrivals)
    }
    if (tergmLite == TRUE) {
      dat$el[[1]] <- add_vertices(dat$el[[1]], nv = nArrivals)
    }
    
  }
  
  
  ## Departures
  if (length(departures) > 0) {
    if (tergmLite == FALSE) {
      dat$nw[[1]] <- deactivate.vertices(dat$nw[[1]], onset = at, terminus = Inf,
                                         v = departures, deactivate.edges = TRUE)
    }
    if (tergmLite == TRUE) {
      
      #replacement code for delete_attr(dat,departures) because
      # base epimodel fxn can't handle non-scalar type values (eg., matices)
      #as agent attributes
      
      #deletes values for dead/aged-out agents except for non-scalar attributes
      vector_flag <- unlist(lapply(dat$attr,function(x) is.vector(x)))
      non_vectors <- which(!vector_flag) #e.g., aim3 matrices etc.
      if(length(non_vectors)>0){
        attrList <- dat$attr[-non_vectors]
      }else{
        attrList <- dat$attr
      }
      
      
      if (class(attrList) != "list") {
        stop("dat object does not contain a valid attribute list", 
             call. = FALSE)
      }
      
      if(dat$param$VL_Function != "aim3"){
      if (length(unique(sapply(attrList, length))) != 1) {
        stop("attribute list must be rectangular (same number of obs per element)")
      }
      }
      
      
      attrList <- lapply(attrList, function(x) x[-departures])
      
      dat$attr <- c(attrList,dat$attr[non_vectors])
      
      #regular epimodel function
      dat$el[[1]] <- delete_vertices(dat$el[[1]], departures)
    }
  }
  
  
  
  ## Copy static attributes to network object
  if (tergmLite == FALSE & resimulate.network == TRUE) {
    dat <- copy_datattr_to_nwattr(dat)
  }
  
  
  ## Output
  return(dat)
}