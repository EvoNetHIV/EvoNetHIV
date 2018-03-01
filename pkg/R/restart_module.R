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
restart_module <- function(dat, at)
{
# Description
#   Does one of three things, depending on the value of restart_val 
#     "save" -- saves 'dat' to file 'dat.saved' in getwd() at restart_time
#     "restart" --  replaces 'dat' wt 'dat.saved' at restart_time
#     "restart_save_all": saves dat files from all simulations at specified restart_time
#     others -- does nothing
#  
# Inputs
#    param$restart_val
#    param$restart_time
#    tx_type_restart
#    
# Outputs:
#    file 'dat.saved' (if restart_val = "save" and simulation==1)
#    Entire contents of dat replaced with dat from simulation 1 of the "save" run (all other runs) 

 if (at==dat$param$restart_time) {
   if (dat$param$restart_val == "save" && dat$simulation == 1) {  # Save first simulation from the "save" run.  
     cat(paste("Saving dat as",getwd(),"/dat.saved\n",sep=""))
     save(dat, file= paste(getwd(),"/dat.saved",sep=""))
   } else if(dat$param$restart_val == "restart") {
     # All "restart" runs (as wells as runs 2 through nsims of the "save" run) overwrite dat with dat.saved
     cat(paste("*** Replacing 'dat' with contents of",getwd(),"/dat.saved ***\n",sep=""))
     scale_up_type_save <- dat$param$scale_up_type  # Need to save values of parameters that will be vary between different restart runs.
     tx_type_save <- dat$param$tx_type
     load(file= paste(getwd(),"/dat.saved",sep=""))
     dat$param$scale_up_type <- scale_up_type_save
     dat$param$tx_type <- tx_type_save
   } else if(dat$param$restart_val == "restart_save_all") {
     save(dat, file = paste0(dat$param$output_path, "/dat_saved_", dat$simulation, ".RDATA"))
   } else if(dat$param$restart_val == "restart_vacc_cea") {
     ## Save parameters that differ from saved dat object and new simulation
     param_save <- dat$param
     load(file = saved_dat) # saved_dat must be global variable specified in simulation script
     ## Reassign saved parameters to overwrite those in saved dat object
     dat$param <- param_save
   }
 }
 return(dat)
}
