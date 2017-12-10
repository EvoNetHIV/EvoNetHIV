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
#' 
#' @export
summary_viral_lineage <- function(poplist)
{
  
  founders     <- which( poplist$Time_Inf <= 0 )
  if(length(founders) == 0){ return(poplist) } 
  viral_lineage_list <- vector('list', length = length(founders))
  
  for(jj in 1 : length(viral_lineage_list))
  {
    index          <- 1
    temp_list      <- list()
    temp_list[[1]] <- founders[jj]
    
    while(TRUE)
    {
      index      <- index+1
      temp_which <- which(poplist$Donors_Index %in% temp_list[[index-1]])
      
      if(length(temp_which) == 0){break}
      
      temp_list[[index]] <- temp_which
    }#end of while loop    
    viral_lineage_list[[jj]] <- temp_list
  }#end of for loop
  return(viral_lineage_list)
}#end of fxn
##############################
#' @export
viral_lineage_fnx2 <- function(vl_list,poplist)
{
  for(jj in 1:length(vl_list))
  {
    xx <- unlist( vl_list[[jj]] )
    if( length(xx) == 1 ){ next }
    poplist$viral_lineage[xx] <- xx[1] 
  }
  return(poplist)
}
################################
#' @export
summary_spvl_lineage <- function(poplist)
{
  #note: summary_viral_lineage and viral_lineage_fnx2
  #must be run before this fxn
  
  index     <- which(!is.na(poplist$viral_lineage))
  founder_index <- poplist$viral_lineage[index]
  founder_spvl  <- poplist$LogSetPoint[founder_index]
  poplist$viral_lineage_init_SPVL[index] <- founder_spvl 
  return(poplist)
}
