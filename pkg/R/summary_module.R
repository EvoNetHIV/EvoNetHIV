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
summary_module <- function(dat,at)
{

  dat <- summary_popsumm(dat,at)
  dat <- summary_misc(dat,at)

  return(dat)
  
}


