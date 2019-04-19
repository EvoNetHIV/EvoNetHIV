#' @export
terminate_vertices <- function(dat, at, vids.to.terminate) {
  # if no vids, give back the dat
  if (length(vids.to.terminate) == 0) return(dat)
  # if the network object exists, assume we are in network mode
  if (!is.null(dat[['nw']])) {
    # deactivate the vertices on the network object
    dat$nw <- deactivate.vertices(dat$nw, onset = at, terminus = Inf,
                                  v = vids.to.terminate, deactivate.edges = TRUE)
  } else {
    # assume we are running in fast_edgelist mode
    # and remove from edgelist using tergmLite utils
    dat$el[[1]] <- tergmLite::delete_vertices(el = dat$el[[1]], vid = vids.to.terminate)
    # also need to remove corresponding attribute rows
    dat$attr <- deleteAttr(dat$attr, vids.to.terminate)
  }
  return(dat)
}
