
#' @export
remove_offset_relationships <- function(dat) {
  
  ## Set up nw object according to fast_edgelist mode.
  if(dat$param$fast_edgelist) {
    nw <- as.network.matrix(dat$el[[1]], matrix.type = "edgelist", directed = F, bipartite = F, loops = F)
  } else {
    nw <- dat$nw
  }
  
  ## Create empty list in which to store relevant attribute of nodes in each partnership
  rel_att <- list()

  if(dat$param$model_sex == "hetero") {
    
    ## Extract sex of each node in each partnership
    for(i in 1:length(nw$mel)) {
      rel_att[[i]] <- dat$attr$sex[unname(unlist(nw$mel[[i]][1:2]))]
    }
    
    ## Identify same-sex relationships
    rm_rel <- which(sapply(rel_att, function(x) x[1] == x[2]))
  }
  
  if(dat$param$model_sex == "msm") {
    
    ## Extract role of each node in each partnership
    for(i in 1:length(nw$mel)) {
      rel_att[[i]] <- dat$attr$role[unname(unlist(nw$mel[[i]][1:2]))]
    }
    
    ## Identify same-role relationships
    rm_rel <- which(sapply(rel_att, function(x) (x[1] == "I" & x[2] == "I") | (x[1] == "R" & x[2] == "R")))
  }
  
  ## Remove same-sex or same-role relationships
  nw$mel <- nw$mel[-rm_rel]

  ## Assign nw object according to fast_edgelist mode
  if(dat$param$fast_edgelist) {
    dat$el[[1]] <- as.edgelist(nw)
  } else {
    dat$nw <- nw
  }

  return(dat)
}