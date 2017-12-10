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
  
  dc <- dat$discord_coital_df
  
  fem.ids <- dc$inf_id
  fem.ids[which(dc$inf_sex != 'f')] <- dc$sus_id[which(dc$sus_sex == 'f')]
  
  dc$ai_prob <- sapply(fem.ids, function(x) {
    dat$pop$ai_prob[which(dat$pop$id == x)]
  })
  
  dc$ai <- rbinom(nrow(dc), 1, dc$ai_prob)
  dc$vi <- ifelse(dc$ai == 1, 0, 1)
  
  dat$discord_coital_df <- dc
  
  return(dat)
  
}