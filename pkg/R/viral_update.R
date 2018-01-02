

#' @export
viral_update <- function(dat, at){
  
  dat <- viral_update_gamma(dat,at)
  dat <- viral_update_cd4_daily(dat,at)
  return(dat)
}