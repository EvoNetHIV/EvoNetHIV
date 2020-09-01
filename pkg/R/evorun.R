#' @export

evorun <- function(modules,params,nw=NULL){

  
  
params <-  input_parameters_derived(params)
  
module_list <-lapply(modules,get)
names(module_list) <- paste(modules,".FUN",sep="")

evo_module_list<- c(
  list("plot_network.FUN"=plot_network_fxn),  
  module_list)


#--- call epimodel's control fxn (load evonet modules into epimodel)
evocontrol <- setup_epimodel_control_object(evonet_params = params,
                                            module_list   = evo_module_list)



#-- create initial vector of infection status as an epimodel object
#original epimodel
#infected_list <- EpiModel::init.net(i.num=params$initial_infected,
#                                    status.rand = FALSE)
#new epimodel
status <- rep("s",params$initial_pop)
status[sample(1:params$initial_pop,size=params$initial_infected)]<- "i"
infected_list <- EpiModel::init.net(status.vector = status)



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
                                       control = evocontrol,
                                       cp.save.int = params$cp_int)
}   


save(evomodel,
     file = file.path(params$output_path,"evomodel.RData"))

return(evomodel)

}
