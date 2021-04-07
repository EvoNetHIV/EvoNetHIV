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
  # Expand “pop” list based on number of new births and fill in default values with “new_additions_fxn” 
  # note: fxn only called in nbirths>=1
  # Inputs: dat$param$no_loci, no_births, dat$pop
  # Outputs: attr_new
  
#for scalar (single value) attributes (age,sex,etc)  
attr_new <- lapply(1:length(dat$attr),
                     function(jj){
                         dat$attr[[jj]] <- c(dat$attr[[jj]],rep(NA_real_,no_births))} )
names(attr_new) <- names(dat$attr)

#aim3 matrices 
if (dat$param$VL_Function == "aim3") {
  V_vec_length <- 2^dat$param$Max_Allowable_Loci
  temp_matrix  <- matrix(0, nrow = no_births, ncol = V_vec_length)
  dat$V_vec <- rbind(dat$V_vec,temp_matrix)
  dat$I_vec <- rbind(dat$I_vec,temp_matrix)
  dat$M_vec <- rbind(dat$M_vec,temp_matrix)
  dat$L_vec <- rbind(dat$L_vec,temp_matrix)
}


  
  
  #this assumes all pop list elements have the same length, so it doesn't matter
  #which one you use to create an index.
  if(no_births > 1){
    ix_start <-  length(attr_new[[1]])-no_births+1
    ix_end <-  length(attr_new[[1]])
  }
  if(no_births == 1){
    ix_start  <-  length(attr_new[[1]])
    ix_end  <-  ix_start
  }
  
  #note: "births" includes initial population
  evo_index <- (dat$total_agents+1):(dat$total_agents+no_births)
  dat$total_agents <- dat$total_agents+no_births
  

attr_final <- new_additions(input_list = attr_new, dat = dat,
                                index = ix_start:ix_end,
                                evo_index = evo_index,
                                type= "births", at = timestep)   
#add epimodel specific attributes (not part of evonet)
attr_final$status     <- c(dat$attr$status,   rep("s",no_births))
attr_final$active     <- c(dat$attr$active,   rep(1, no_births))
attr_final$entrTime   <- c(dat$attr$entrTime, rep(timestep, no_births ) )
attr_final$exitTime   <- c(dat$attr$exitTime, rep(NA_real_, no_births ) )
attr_final$infTime    <- c(dat$attr$infTime,  rep(NA_real_, no_births ) )

dat$attr <- attr_final

return(dat)
}
