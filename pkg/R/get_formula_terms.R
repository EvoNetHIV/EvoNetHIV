#' @export
#'
get_formula_terms <- function(formula) {
  
  fterms <- attributes(terms.formula(formula))$term.labels
  # if it has an offset term, needs to be processed differently
  if(!is.null(attr(terms.formula(formula),'offset'))){
    offTerm <- attr(terms.formula(formula),'offset')
    fterms <-c(fterms, lapply(offTerm, function(x) { attr(terms.formula(formula),'variables')[[x+1]] }))
  }
  fterms <- strsplit(as.character(fterms), split = "[\"]")
  tl <- sapply(fterms, length)
  if (all(tl == 1)) {
    fterms <- NULL
  } else {
    fterms <- fterms[tl > 1]
    fterms <- unique(sapply(fterms, function(x) x[2]))
  }
  
  return(fterms)
}
