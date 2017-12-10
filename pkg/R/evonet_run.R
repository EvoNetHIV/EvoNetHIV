#two functions: evonet_run() and evonet()

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
evonet_run<-function(user_base_params=NULL,user_var_param=NULL,model_names="evomodel",
                    outpath=getwd(), 
    module_list=list(
  "initialize.FUN"     = initialize_module,
  "plot_nw.FUN"        = plot_network_fxn,  
  "aging.FUN"          = vital_aging_module,
  "testing.FUN"        = social_testing_diagnosis_module,
  "treatment.FUN"      = social_treatment_module,
  "update_vl.FUN"      = viral_update_gamma,
  "update_cd4.FUN"     = viral_update_cd4_daily, 
  "coital_acts.FUN"    = social_coital_acts_module,
  "trans.FUN"          = transmission_main_module,
  "trans_book.FUN"     = transmission_bookkeeping_module,
  "trans_cd4.FUN"      = transmission_cd4_module,
  "deaths.FUN"         = vital_deaths_module,
  "births.FUN"         = vital_births_module,
  "social_trans.FUN"   = social_attribute_transition_module,
  "summary.FUN"        = summary_module,
  "resim_nets.FUN"     = EpiModel::resim_nets,
  "verbose.FUN"        = NULL)){
  if(is.null(user_var_param)){
    evo_out=evonet(param_base=user_base_params,
                                     param_list=user_var_param,
                                     module_list=module_list)
  }else{
    if(length(user_var_param)!=length(model_names)){
      model_names=paste("evomodel",1:length(user_var_param),sep="")
      }
    evo_out=mapply(evonet,param_list=user_var_param,model_name=model_names,
             MoreArgs=list(param_base=user_base_params,module_list=module_list))
    }
  
  #return(evo_out)
  
}


#' @export
evonet<-function(param_base=NULL,param_list=NULL,module_list,model_name="evomodel",
                 outpath=outpath){

  
#--------------------------------------------------------------
#Load default parameters

primary_parameters  <- input_parameters_primary()
cd4_data            <- input_parameters_cd4_data()

#--- combine individual parameters into single list
evoparams <- c(primary_parameters, cd4_data)

#--------------------------------------------------------------

#overide default parameters, re-initialize network with this code
#used as example, users should create own file to source.
if(is.character(param_base))
source(param_base,local=TRUE)
#----------------------------
if(is.list(param_base)){
  names_param_base=names(param_base)
  if(any(!is.element(names_param_base,names(evoparams)))){
    stop("user specified parameter in var_param list incorrectly named or not an evoparameter")
  }
  for(ii in 1:length(param_base)){
    evoparams[[names_param_base[ii]]]=param_base[[ii]]
  }
}
#----------------------------
if(is.list(param_list)){
  names_param_list=names(param_list)
  if(any(!is.element(names_param_list,names(evoparams)))){
    stop("user specified parameter in var_param list incorrectly named or not an evoparameter")
  }
  for(ii in 1:length(param_list)){
    evoparams[[names_param_list[ii]]]=param_list[[ii]]
  }
}
#--------------------------------------------------------------
#add parameters that are functions of other input parameters
evoparams  <- input_parameters_derived(evoparams)

#convert raw parameter list into EpiModel object
evoparams  <- do.call(EpiModel::param.net,evoparams )

#--------------------------------------------------------------

nw <- setup_initialize_network(evoparams)

#--------------------------------------------------------------

#run qaqc on input parameters
input_parameters_qaqc(evoparams)

#--------------------------------------------------------------

#estimate initial network (create argument list, then call fxn)
netest_arg_list <- list(
  nw            =  nw,
  formation     =  as.formula(evoparams$nw_form_terms),
  target.stats  =  evoparams$target_stats,
  coef.form     =  evoparams$nw_coef_form,
  constraints   =  as.formula(evoparams$nw_constraints),
  verbose       =  FALSE,
  coef.diss     =  dissolution_coefs( dissolution =  ~offset(edges),
                                      duration    =  evoparams$relation_dur,
                                      d.rate      =  3e-05) )

estimated_nw <- do.call(EpiModel::netest, netest_arg_list)

#--------------------------------------------------------------

#-- create initial vector of infection status as an epimodel object
infected_list <- EpiModel::init.net(i.num=evoparams$initial_infected,
                                    status.rand = FALSE)

#--------------------------------------------------------------

#---  Create list of modules to run for input into epimodel_control_fxn() below

# ***   Note: initialize fxn must always be first and verbose fxn last AND death fxn
# ***   must precede birth fxn (these conditions may change in future)
# ***   treatment_fxn must be before update_vl and update_cd4

evo_module_list<- module_list


#--- call epimodel's control fxn (load evonet modules into epimodel)
evocontrol <- setup_epimodel_control_object(evonet_params = evoparams,
                                            module_list   = evo_module_list)

#--------------------------------------------------------------

runtime <- system.time({
  
  evomodel  <- EpiModel::netsim(x = estimated_nw,
                                param = evoparams,
                                init = infected_list,
                                control = evocontrol)
})

print(runtime)

assign(model_name,evomodel)
file_name <- paste(model_name,".RData",sep="")
save(list=model_name,
     file = file.path(evoparams$output_path,file_name) )


#--------------------------------------------------------------
# #note! these pdfs have to closed to run again (old copy can't be open)

plots_popsumm(evomodel,outpath=evoparams$output_path,
              name=paste(model_name,".pdf",sep=""),nw_stats=TRUE,max_points_rep=100,
              evoparams$popsumm_frequency)

#--------------------------------------------------------------

#recommended only for runs with very small populations size
#evoparams$save_network=TRUE must be set for this fxn

if(evoparams$save_network==TRUE){
  plot_relationship_history(model=evomodel,sim=1,
                            outpath=evoparams$output_path,
                            name=model_name)
}

#--------------------------------------------------------------

# #to pdf: evonet/evomodel_output/model/vl_trajectories.pdf
# #note! these pdfs have to closed to run again (old copy can't be open)
# save_vl_list=TRUE must be set for this fxn
if(evoparams$save_vl_list==TRUE){
  plot_vl_trajectories(model=evomodel,sim=1,
                       outpath=evoparams$output_path,
                       name=model_name)
}  

#--------------------------------------------------------------

#recommended for only very short runs <50 timesteps
#evoparams$save_coital_acts_list=T must be set

if(evoparams$save_coital_acts==TRUE){
  plot_disc_coital_acts_history(model=evomodel,
                                outpath=evoparams$output_path,
                                sim=1,
                                name=model_name)
}
#return(invisible(NULL))
}#end fxn