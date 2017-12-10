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
social_discord_edgelist <- function (el, status_vec, at) 
{ 
  
  #######################################################
  #-- returns discordant edgelist from network
  #-- called in social_discord_edgelist_df
  #-- individual IDs refer tovnetwork position
  #-- called in social_discord_edgelist_df()
  #-- modification of Sam's EpiModel discord_edgelist()
  
  #input: network, status of agents (inf/sus)
  #output: 2 column matrix of discordant pairs (by agent ids)
  
  #######################################################
  
  status  <- status_vec
  edge_list  <- el 
  if(length(edge_list)==0){return(NULL)}
  edge_list  <- edge_list[sample(1:nrow(edge_list)), , drop = FALSE]
  stat    <- matrix(status[edge_list], ncol = 2)
  sums    <- rowSums(stat)
  disc    <- which(sums==1)
  
  #if(length(disc)==0){return(NULL)}
  
  col2inf <- which( stat[,1] / stat[,2] == 0)
  col1inf <- disc[ which(!is.element(disc,col2inf)) ]
  del_mat <- rbind( edge_list[col1inf,], edge_list[col2inf,2:1] )
 
if(length(del_mat[,1])>0){  
del     <- data.frame(timestep=at, #timestep 
                        agent1=del_mat[,1], #infected 
                        agent2=del_mat[,2],#susceptible
                        inf_id = del_mat[,1],
                        sus_id = del_mat[,2],
                        infected=1) #discordant couple? (0/1)
}else{del<-NULL}
 if(length(edge_list[-disc,1])>0){  
 nondel <-  data.frame(timestep=at, 
                       agent1= edge_list[-disc,1], 
                       agent2= edge_list[-disc,2],
                       inf_id = NA_real_,
                       sus_id = NA_real_,
                       infected=0) #discordant couple? (0/1)
 }else{nondel<-NULL}
  coital_el <- rbind(del,nondel)
  return(coital_el)
}
