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
summary_evo_newick<-function(model,path=getwd(),name=NULL,plot=TRUE){
  #plots transmission tree and writes out newick file
  #note to run, set evomparams$save_coital_acts=TRUE
  
  
  if(is.null(model$coital_acts_list[[1]])){
    stop("fxn stopping: coital acts list not saved")}
  if(sum(model$popsumm[[1]]$new_infections,na.rm=T)==0){
    stop("fxn stopping: no new infections occurred")}
  
  bb <- do.call("rbind",model$coital_acts_list[[1]])
  cc <- bb[which(bb$infection==1),]
  sus <- unique(cc$sus_id)
  ix <- match(sus,cc$sus_id)
  dd <- cc[ix,]
  evo_tm <- data.frame(at=dd$timestep,sus=dd$sus_id,inf=dd$inf_id)
  evo_exit_times <- model$pop[[1]]$Time_Death
  evo_phylo <- as.phylo.transmat(x=evo_tm,vertex.exit.times = evo_exit_times)
 
  if(plot){
  plot(evo_phylo, show.node.label = TRUE,
       root.edge=TRUE, 
       cex = 0.5)
  }
  
  if(is.null(name)){
    name="EvoNewick.txt"
  }else{
    name=paste(name,"_EvoNewick.txt",sep="")    
  }
  ape::write.tree(evo_phylo,file=file.path(path,name))
  
  return(evo_tm)
}
