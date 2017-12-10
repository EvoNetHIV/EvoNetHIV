#' @title Title
#'
#' @description Description
#'
#' @param x A number.
#' @param y A number.
#' @return return value here.
#' @details
#' Additional details here
#' @examples
#' example function call here

#' @export
summary_partner_list <- function(dat){
  aa <- dat$partner_list
  f1 <- function(x,y){if(length(x)>0){cbind(x,rep(y,nrow(x)))}}
  aa <- lapply(1:length(aa),function(x){f1(aa[[x]],x)})
  
  bb <- do.call(rbind,aa)
  ix <- which(bb[,3]==1)
  if(length(ix)>1){
    cc <- bb[ix,,drop=F]
  }
  ss <- cbind(c(cc[,1],cc[,2]),c(cc[,4],cc[,4]))
  tt <- split(ss[,2],ss[,1])
  no_agents <- length(dat$pop$Status)
  outlist <- vector('list',length=no_agents)
  outlist <- as.list(rep(NA_real_,no_agents))
  lapply(1:length(tt),function(x) outlist[[ as.numeric(names(tt))[x] ]]<<-tt[[x]])
  return(outlist)
}
