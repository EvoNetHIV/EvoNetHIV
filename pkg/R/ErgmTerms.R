#' @title Definition for absdiffby ERGM Term
#'
#' @description This function defines and initialize the absdiffby ERGM term
#'              that allows for targeting age homophily by sex.
#'
#' @param nw An object of class \code{network}.
#' @param arglist A list of arguments as specified in the \code{ergm.userterms}
#'        package framework.
#' @param ... Additional data passed into the function as specified in the
#'        \code{ergm.userterms} package framework.
#'
#' @details
#' This ERGM user term was written to allow for age-based homophily in partnership
#' formation that is asymmetric by sex. The absdiff component targets age homophily
#' while the by component allows that to be structed by a binary attribute such
#' as "male", in order to enforce an offset in the average difference.
#'
#' @export
InitErgmTerm.absdiffby <- function(nw, arglist, ...) {
  a <- check.ErgmTerm(nw,
                      arglist,
                      directed = FALSE,
                      bipartite = FALSE,
                      varnames = c("attrname", "by", "assym", "values"),
                      vartypes = c("character", "character", "numeric", "vector"), 
                      required = c(TRUE, TRUE, TRUE, FALSE), 
                      defaultvalues = list(NULL, NULL, NULL, NULL)) 
  
  nodecov <- get.node.attr(nw, a$attrname)
  nodeby <- 1 * (get.node.attr(nw, a$by) == "m")
  coef.names <- paste("absdiffby", a$attrname, a$by, sep = ".")
  
  list(name = "absdiffby",
       coef.names = coef.names,
       pkgname = "evonet",
       inputs = c(a$assym, nodecov, nodeby),
       dependence = FALSE,
       emptynwstats = 0
  )
}