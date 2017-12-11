#' @export

evorun <- function(module_list,params,nw){

  
  
evo_module_list<- c(
  list("initialize.FUN"= initialize_module,
  "plot_nw.FUN" = plot_network_fxn),  
  module_list,
  list("resim_nets.FUN" = EpiModel::resim_nets,
  "verbose.FUN"= NULL))


#--- call epimodel's control fxn (load evonet modules into epimodel)
evocontrol <- setup_epimodel_control_object(evonet_params = params,
                                            module_list   = evo_module_list)



#-- create initial vector of infection status as an epimodel object
infected_list <- EpiModel::init.net(i.num=params$initial_infected,
                                    status.rand = FALSE)

runtime <- system.time({
  
  evomodel  <- EpiModel::netsim(x = nw,
                                param = evoparams,
                                init = infected_list,
                                control = evocontrol)
  
})

cat("Model runtime (minutes)\n")
cat(runtime[3]/60,"\n")



save(evomodel,
     file = file.path(evoparams$output_path,"evomodel.RData"))

return(evomodel)

}
