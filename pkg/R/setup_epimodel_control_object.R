#' @title Run EpiModel's control.net() with EvoNet parameterization.
#'
#' @description Helper function to fill in and run EpiModel control.net(). An important component is the save.other vector created within the function. This vector of object names specifies the objects that will be returned at the end of the model run (i.e., after netsim called) beyond the default EpiModel objects. Thus, any new output objects created must be specified with the save.other vector.
#'
#' @param evonet_params List of EvoNet parameters.
#' @param module_list List of EvoNet functions/modules to simulate epidemic.
#' @return EpiModel 'control' object.
#' @details
#' 
#' @examples
#' evocontrol <- setup_epimodel_control_object(evonet_params = evoparams,
#'      module_list   = evo_module_list)


#' @export
setup_epimodel_control_object <- function(evonet_params,module_list)
{ 

  #Description:
  # Helper function to fill in and run EpiModel's control.net(); 
  # of minor utility, maybe should be removed and control.net just called straight from master_script.
  # Called in master_script
  #calls epimodel::control.net, ?EpiModel::control.net for details
  #input: module_list (modules to be called)
  #input: evoparams "save_network","nsims","n_steps"
  #output: EpiModel "control" object (used in EpiModel netsim fxn)
  
  ####################################################
  #function for original epimodel
  # control_epimodel_params_list <- list(ncores=evonet_params$ncores,
  #                                      type   = NULL,
  #                                      nsims  = evonet_params$nsims,
  #                                      nsteps = evonet_params$n_steps,  #set in parameter_list.R
  #                                       start  = evonet_params$start_timestep,
  #                                      depend = TRUE,
  #                                      use.pids = FALSE,
  #                                      tea.status    = FALSE,
  #                                      save.transmat = FALSE,
  #                                      save.nwstats  = FALSE,
  #                                      save.network  = evonet_params$save_network,
  #                                      delete.nodes  = !evonet_params$save_network,
  #                                      tergmLite = TRUE,
  #                                      module.order  = names(module_list)[-c(1,length(names(module_list)))],
  #                                      save.other    = c("attr","pop","param","nw","coital_acts_list",
  #                                                        "popsumm","vl_list","InfMat","age_list","el",
  #                                                        "sessionInfo","partner_list"))
  #############################################################
  
  f1=function(x,type="hello",s=1,at=2){invisible()}
  
  control_epimodel_params_list <- list(type=NULL,
              nsteps=evonet_params$n_steps,
              start = 1,
              nsims =  evonet_params$nsims,
              ncores =  evonet_params$ncores,
              resimulate.network = TRUE,
              tergmLite = TRUE,
              save.nwstats = TRUE,
              nwstats.formula = "formation",
              verbose = F,
              verbose.int = 1e6,
              skip.check = TRUE,
              raw.output = FALSE,
              initialize.FUN = initialize_module,
              resim_nets.FUN = resim_nets,
              nwupdate.FUN = evo_nwupdate,
              verbose.FUN = f1,
              module.order  = c(names(module_list),"nwupdate.FUN","resim_nets.FUN"),
              save.other    = c("attr","pop","param","nw","coital_acts_list",
                           "popsumm","vl_list","InfMat","age_list","el",
                           "sessionInfo","partner_list","vacc_model"))   
  
  
  
 
  #base EpiModel, do. call wasn't working, replaced with "invoke"
  #control <- do.call(EpiModel::control.net,  
   #                  control_epimodel_params_list )
  
  control <- rlang::invoke(EpiModel::control.net,  
                     c(module_list,control_epimodel_params_list) )
  
  return(control)
}
###############################################
