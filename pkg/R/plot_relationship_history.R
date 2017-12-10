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
plot_relationship_history<-function(model,sim,outpath,name="evomodel")
{

if(model$param[[sim]]$save_network==FALSE){
  print("Note: network data not saved, function stopping. Set save_network=TRUE in initialize_output_params() in
master script if desired to see relationship/infection history")
  return(invisible(NULL))
}
  
 name <- paste(name,"_relationship_history.pdf",sep="")    
  
pdf(file.path(outpath,name),width=12,height=10)

pop <- model$pop[[sim]]
  nw  <- model$nw[[sim]]
  params <-  model$param[[sim]]

  ind_ix <- which(!is.na(pop$Time_Inf))
  
  for(ind in ind_ix)
  {  
  
    out <- infection_history_plot(ind=ind,pop=pop,params=params,nw=nw)
    #coital_acts_table_fxn(model=model,dframe=out$dframe,pop=pop,ind=ind,
     #           ind_inf_time=out$ind_inf_time,ind_inf_donor=out$ind_inf_donor,
    #            partners=out$partners)
    #infection_summary_table_fxn(pop=pop,partners=out$partners,dframe=out$dframe,
    #                            ind=ind)
    #infection_history_vl_cd4_plot(ind=ind,vl_df=vl_df)
  }
dev.off()
}


