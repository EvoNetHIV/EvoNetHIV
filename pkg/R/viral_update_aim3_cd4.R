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
viral_update_aim3_cd4 <- function(dat,at)
{
  # Initial attempt to translate these continuous CD4 T-cell into the categorical T-CD4 counts used previously
  # Note that time element (i.e., time-between-stages) is handled dynamically by the progression parameter "k" above
  #input: 'pop' list: Status, CD4 variables
  #output: 'pop' list: CD4 variable
  pop=dat$pop
  List_High_CD4 <- which(pop$CD4count >= 500 & pop$Status==1)
  List_Moderate_CD4 <- which(pop$CD4count < 500 & pop$CD4count >= 350 & pop$Status==1)
  List_Low_CD4 <- which(pop$CD4count < 350 & pop$CD4count >= 200 & pop$Status==1)
  List_VeryLow_CD4 <- which(pop$CD4count < 200 & pop$CD4count >= 1 & pop$Status==1)
  List_FatallyLow_CD4 <- which(pop$CD4count < 1 & pop$Status==1)
  
  pop$CD4[List_High_CD4] = 1
  pop$CD4[List_Moderate_CD4] = 2
  pop$CD4[List_Low_CD4] = 3
  pop$CD4[List_VeryLow_CD4] = 4
  pop$CD4[List_FatallyLow_CD4] = 5
  
  dat$pop=pop
  return(dat)
}
