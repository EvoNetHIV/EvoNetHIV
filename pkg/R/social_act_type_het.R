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
social_act_type_het <- function(dat, at) {
  
  if(is.null(dat$discord_coital_df)) {
    return(dat)
  }
  # if only serocord couples present
  if(all(is.na(dat$discord_coital_df$inf_id))) {
    return(dat)
  }
  
  ix <- which(!is.na(dat$discord_coital_df$inf_id))
  dc <- dat$discord_coital_df[ix,]
  fem.ids <- dc$inf_id
  if(length(dc$sus_id[which(dc$sus_sex == 'f')]) != 0) {
    fem.ids[which(dc$inf_sex != 'f')] <- dc$sus_id[which(dc$sus_sex == 'f')]
  }
  
  dc$ai_prob <- sapply(fem.ids, function(x) {
    dat$pop$ai_prob[which(dat$pop$id == x)]
  })
  
  dc$ai <- rbinom(nrow(dc), 1, dc$ai_prob)
  dc$vi <- ifelse(dc$ai == 1, 0, 1)
  
  dcsero=dat$discord_coital_df[-ix,]
  dcsero$ai_prob=rep(NA,nrow(dcsero))
  dcsero$ai=rep(NA,nrow(dcsero))
  dcsero$vi=rep(NA,nrow(dcsero))
  
  dcfinal <- rbind(dc,dcsero)
  dat$discord_coital_df <- dcfinal
  
  return(dat)
  
}
