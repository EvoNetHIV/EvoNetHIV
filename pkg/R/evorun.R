#' @export

evorun <- function(modules,params,nw=NULL){

params <-  input_parameters_derived(params)
  
names(modules) <- paste(modules,".FUN",sep="")
module_list <- as.list(modules)

evo_module_list<- c(
  list("initialize.FUN"= initialize_module,
       "plot_network.FUN"=plot_network_fxn),  
  module_list,
  list("resim_nets.FUN" = EpiModel::resim_nets,
  "verbose.FUN"= NULL))


#--- call epimodel's control fxn (load evonet modules into epimodel)
evocontrol <- setup_epimodel_control_object(evonet_params = params,
                                            module_list   = evo_module_list)



#-- create initial vector of infection status as an epimodel object
infected_list <- EpiModel::init.net(i.num=params$initial_infected,
                                    status.rand = FALSE)

if(!params$hyak_par){
  
  runtime <- system.time({
    
    evomodel  <- EpiModel::netsim(x = nw,
                                  param = params,
                                  init = infected_list,
                                  control = evocontrol)
    
  })
  cat("Model runtime (minutes)\n")
  cat(runtime[3]/60,"\n")
}else{
  #hyak parallel run
  evomodel  <- EpiModelHPC::netsim_hpc(x = "estimated_nw.RData",
                                       param = params,
                                       init = infected_list,
                                       save.min = FALSE,
                                       save.max = FALSE,
                                       control = evocontrol)
}   


save(evomodel,
     file = file.path(evoparams$output_path,"evomodel.RData"))

return(evomodel)

}
