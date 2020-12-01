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
plot_network_fxn <-function(dat,at)
{
  
  #exit if no network plotting    
  if(dat$param$plot_nw != TRUE){return(dat)}
  #exit if not first timestep or not timestep to plot network
  if( (at != 2) & ((at %% dat$param$network_print_frequency) != 0)){
    return(dat)
  }
  #exit if "save_network==T" (except if first timestep),
  #as plotting doesn't work well on network with complete history
  if(dat$param$save_network== TRUE & at>2){return(dat)} 
  #TODO: could just extract current time point to plot? 


  # if we are in fast_edgelist mode, construct a network from the edgelist
  if(is.null(dat[['nw']])){
    nw <- as.network.matrix(dat$el[[1]],matrix.type='edgelist',directed=FALSE)
  } else {
    nw <- dat$nw
  }
  
    
  #color from low to high values (spvl, connectivity)
  color_vec <- c("white","yellow","orange","red","purple","black")
  
  # Create four plots if we have generic attribute groups
  if(!is.logical(dat$param$generic_nodal_att_values))
     par(mfrow=c(2,2)) 
  else
     par(mfrow=c(2,1)) 
  
  par(mfrow=c(1,1))
  
  #get ids of agents on network that are infected
  inf_index <- which(dat$attr$Status == 1 & dat$attr$active == 1)  
  
  # Plot # 1 -- Nodes colored by viral load
  spvl_breaks=c(0,1e3,1e4,1e5,1e6,1e10)
  spvl_label <- as.numeric(cut(dat$attr$V[inf_index],breaks=spvl_breaks,
                           labels=1:5))
  color_ix <- rep("white",length(dat$attr$Status))
  color_ix[inf_index] <- color_vec[spvl_label]
  
  #sex-specific symbols
  pch_vec <- rep(50,length(dat$attr$Status))
  pch_vec[dat$attr$sex==0]=3
  
  plot(nw,vertex.col=color_ix,vertex.sides=pch_vec)
  mtext(paste("VLs at day",at,"(dark = high VL)"),side=3)
  
  
  # Plot # 2 -- Nodes colored by SPVL
  inf_index <- which(dat$attr$Status == 1 & dat$attr$active == 1)  # No longer used now that I try to colorize the nodes
  inf_index <- dat$attr$id[inf_index]
  
  spvl_breaks=c(0,1e3,1e4,1e5,1e6,1e10)
  spvl_label <- as.numeric(cut(dat$attr$SetPoint[inf_index],breaks=spvl_breaks,
                               labels=1:5))
  #sex-specific symbols
  pch_vec <- rep(50,length(dat$attr$Status))
  pch_vec[dat$attr$sex==0]=3
  
  color_ix <- rep("white",length(dat$attr$Status))
  color_ix[inf_index] <- color_vec[spvl_label]
  
  if(length(which(dat$attr$active ==1))<=100 & !is.null(dat[['nw']])){
    plot(nw,vertex.col=color_ix,label="id",label.cex=.85,vertex.sides=pch_vec)  
  } else {
    plot(nw,vertex.col=color_ix,vertex.sides=pch_vec)
  }
  mtext(paste("SPVLs at day",at,"(dark = high VL)"),side=3)
    
  # Plot # 3 -- Nodes colored by connection group
  if(!is.logical(dat$param$generic_nodal_att_values)){
    
    active_index <- which(dat$attr$Status >=0 & dat$attr$active ==1) 
    attr_index_id <- dat$attr$id[active_index]
    color_ix <- rep("white",length(dat$attr$Status))
    att_color_vec <- c("white","red","blue","green","purple","black")
    color_ix <- att_color_vec[dat$attr$att1[attr_index_id]] 
  
    #sex-specific symbols
    pch_vec <- rep(50,length(dat$attr$Status))
    pch_vec[dat$attr$sex==0]=3
    
    
  # only show labels on network if the network is small and not in fast edgelist  
  if(length(which(dat$attr$active ==1))<=100 & !is.null(dat[['nw']])){
    plot(nw, vertex.col = color_ix,label="id",label.cex=.85,vertex.sides=pch_vec)  
  } else {
    plot(nw, vertex.col = color_ix,vertex.sides=pch_vec)
  }
    mtext(paste("Risk groups day",at,"(white, red, blue, green, purp, black: grps 1-5)"),side=3)
    
    # Plot # 4 -- Nodes colored by age index
    active_index <- which(dat$attr$Status >=0 & dat$attr$active ==1) 
    attr_index_id <- dat$attr$id[active_index]
    age_breaks <- c(0,25,35,45,55,65,100)
    age_label <- as.numeric(cut(dat$attr$age[attr_index_id],breaks=age_breaks,
                                labels=1:6))
    
    color_ix <- rep("white",length(dat$attr$Status))
    age_color_vec <- c("purple","red","pink","white","green","black")
    color_ix <- age_color_vec[age_label]
    
    #sex-specific symbols
    pch_vec <- rep(50,length(dat$attr$Status))
    pch_vec[dat$attr$sex==0]=3
    
    plot(nw,vertex.col=color_ix,vertex.sides=pch_vec)
    mtext(paste("Ages at day",at," [ <25:purple, 25-35:red\n35-45:pink, 45-55:white, 55-65:green, >65:black"),side=3)
    
 }

    
  return(dat)
}
