#' @export
initiate_vertices <- function(dat,at,n){
  # if the network object exists, assume we are in network mode
  if (!is.null(dat[['nw']])) {
    # add and activate new vertices
    dat$nw <- add.vertices.active(dat$nw, nv = n, onset = at, terminus = Inf)
  } else {
    # increment the vertex counter on the edgelist
    dat$el[[1]] <- tergmLite::add_vertices(dat$el[[1]], n)
  }
  return(dat)
}
