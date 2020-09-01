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
vital_births_bookkeeping_pop <- function(no_births,dat,timestep)
{
  #Description: 
  # Expand "pop" list based on number of new births and fill in default values with "new_additions_fxn" 
  # note: fxn only called in nbirths>=1
  # Inputs: dat$param$no_loci, no_births, dat$pop
  # Outputs: tempList
  
  V_vec_length <- 2^dat$param$Max_Allowable_Loci
  temp_matrix  <- matrix(0, nrow = no_births, ncol = V_vec_length)
  
  tempList <- lapply(1:length(dat$pop),
                     function(jj){
                       if(is.vector(dat$pop[[jj]])){
                         dat$pop[[jj]] <- c(dat$pop[[jj]],rep(NA_real_,no_births))}
                       else{dat$pop[[jj]] <- rbind(dat$pop[[jj]],temp_matrix) } 
                     })
  
  
  names(tempList) <- names(dat$pop)
  
  #this assumes all pop list elements have the same length, so it doesn't matter
  #which one you use to create an index.
  if(no_births > 1){
    ix_start <-  length(tempList[[1]])-no_births+1
    ix_end <-  length(tempList[[1]])
  }
  if(no_births == 1){
    ix_start  <-  length(tempList[[1]])
    ix_end  <-  ix_start
  }
  
  tempList <- new_additions_fxn(input_list = tempList, dat = dat,
                                index = ix_start:ix_end,
                                type= "births", at = timestep)   
  
  return(tempList)
}
