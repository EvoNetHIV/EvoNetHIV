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
social_treatment_module_john_v3 <- function(dat, at)
{
  #Description
  #Determines which infected,diagnosed,eligible-for-care agents gets treatment
  #given treatment campaign in effect
  #possible treatment scenarios:c("VL3","VL4","fifo","CD42","generic_attr","random",
  #"MS1","MS2")
  
  # In this version, the number treated at each timestep is determined by "proportion_treated" at each timestep.
  # In social_treatment_module_john, by contrast, the number treated at each timestep is determined by 
  # proportion_treated at the start of the treatment campaign
  
  #Inputs: 
    #param$start_treatment_campaign
    #param$proportion_treated
    #pop$treated
    #pop$Status
    #pop$diag_status
    #param$tx_type
    #param$tx_in_acute_phase
 #Output:
    #pop$treated
    #pop$tx_init_time
  
  
  if(at < dat$param$start_treatment_campaign){
    return(dat)
  }
  
  if(length(which(dat$pop$Status==1))==0){return(dat)}
  
  if(is.logical(dat$param$generic_nodal_att_mean_trtmnt_delay)){
  eligible_agents_index <- which(dat$pop$Status == 1 & 
                                   dat$pop$treated == 0 &
                                   dat$pop$eligible_care == 1 & 
                                   dat$pop$diag_status == 1 &
                                  ((dat$pop$Time_Inf+dat$param$mean_trtmnt_delay)<=at)) 
  
  eligible_agents_index_including_treated <- which(dat$pop$Status == 1 & 
                                              dat$pop$eligible_care == 1 & 
                                              dat$pop$diag_status == 1 &
                                             ((dat$pop$Time_Inf+dat$param$mean_trtmnt_delay)<=at))}
  
  if(!is.logical(dat$param$generic_nodal_att_mean_trtmnt_delay)){
    eligible_agents_index <- which(dat$pop$Status == 1 & 
                                     dat$pop$treated == 0 &
                                     dat$pop$eligible_care == 1 & 
                                     dat$pop$diag_status == 1 &
                                     ((dat$pop$Time_Inf+(dat$param$mean_trtmnt_delay*(dat$pop$att1==1)+dat$param$generic_nodal_att_mean_trtmnt_delay*(dat$pop$att1==2)))<=at)) 
    
    eligible_agents_index_including_treated <- which(dat$pop$Status == 1 & 
                                                       dat$pop$eligible_care == 1 & 
                                                       dat$pop$diag_status == 1 &
                                    ((dat$pop$Time_Inf+(dat$param$mean_trtmnt_delay*(dat$pop$att1==1)+dat$param$generic_nodal_att_mean_trtmnt_delay*(dat$pop$att1==2)))<=at))}
  
  
  
  if(length(eligible_agents_index)==0){return(dat)}
  
  no_on_tx <- length(which(dat$pop$treated==1 & dat$pop$Status==1))
  
  #is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol
  #if(is.wholenumber(at/365/10)==T){browser()}
  #{browser()}
  #------------------------------------------------------------------------  
  # this section for treatment scenarios where there is limit to number treated (max_treated)  
  if (dat$param$tx_type %in% c("VL_high","all_men","VL_low","youngest","fifo","CD4_low","generic_attr","random",
                              "MS1","MS2","men","role","women","men_cd4","under45","risk_age")){
   
    length_eligible_agents_index <- length(eligible_agents_index)
    if(length_eligible_agents_index==0){return(dat)}
        
    max_treated <- trunc(dat$param$proportion_treated*length(eligible_agents_index_including_treated))
    max_starting_therapy <- max_treated - no_on_tx 
    if (max_starting_therapy <= 0 && dat$param$tx_type != "all_men") {return(dat)}
          
    subsample <- length_eligible_agents_index > max_starting_therapy
   
    if(subsample || dat$param$tx_type == "all_men") {      #if more elibible for tx than allowed, need to subset

      if(dat$param$tx_type=="VL_low" ){
        rank_eligible_agents<- rank(dat$pop$V[eligible_agents_index],ties.method="random")
        eligible_agents_receive_tx <- eligible_agents_index[which(rank_eligible_agents<= max_starting_therapy)]
      }

      if(dat$param$tx_type=="VL_high" ){
        rank_eligible_agents <- rank(-dat$pop$V[eligible_agents_index],ties.method="random")
        eligible_agents_receive_tx <- eligible_agents_index[which(rank_eligible_agents<= max_starting_therapy)]
      }
      
     if (dat$param$tx_type=="generic_attr" ){
        rank_eligible_agents<- rank(-dat$pop$att1[eligible_agents_index],ties.method="random")
        eligible_agents_receive_tx <- eligible_agents_index[which(rank_eligible_agents<= max_starting_therapy)]    
      }

     if(dat$param$tx_type=="MS2" ){
        rank_eligible_agents<- rank(-(10*dat$pop$att1[eligible_agents_index] + 0.3333*log10(dat$pop$V[eligible_agents_index])),
                                               ties.method="random")
        eligible_agents_receive_tx <- eligible_agents_index[which(rank_eligible_agents<= max_starting_therapy)]    
      }
      
      if(dat$param$tx_type=="MS1" ){
        tempvec <- dat$pop$att1[eligible_agents_index]+log10(dat$pop$V[eligible_agents_index]) 
        rank_eligible_agents<- rank(-tempvec,ties.method="random")
        eligible_agents_receive_tx <- eligible_agents_index[which(rank_eligible_agents<= max_starting_therapy)]    
      }

      if(dat$param$tx_type=="under45" ){
         eligible_agents_receive_tx <- eligible_agents_index[which(dat$pop$age[eligible_agents_index] <= 45)]
      } 

      if (dat$param$tx_type=="risk_age" ){
        tempvec <- 100*dat$pop$att1[eligible_agents_index]+dat$pop$age[eligible_agents_index]
        rank_eligible_agents<- rank(-tempvec,ties.method="random")
        eligible_agents_receive_tx <- eligible_agents_index[which(rank_eligible_agents<= max_starting_therapy)]    
      }

      if(dat$param$tx_type=="fifo" ){
        rank_eligible_agents<- rank(-(at - dat$pop$diag_time[eligible_agents_index]),ties.method="random")
        eligible_agents_receive_tx <- eligible_agents_index[which(rank_eligible_agents<= max_starting_therapy)]    
      }
      
      if(dat$param$tx_type=="CD4_low" ){
        rank_eligible_agents<- rank(-dat$pop$CD4[eligible_agents_index],ties.method="random")
        eligible_agents_receive_tx <- eligible_agents_index[which(rank_eligible_agents<= max_starting_therapy)]
      }
  
       if(dat$param$tx_type=="men" ){
        eligible_agents_receive_tx <- eligible_agents_index[which(dat$pop$sex[eligible_agents_index] == "m")]   
      }
      
      if(dat$param$tx_type=="youngest" ){
        rank_eligible_agents<- rank(dat$pop$age[eligible_agents_index],ties.method="random")
        eligible_agents_receive_tx <- eligible_agents_index[which(rank_eligible_agents<= max_starting_therapy)]
      }

      if(dat$param$tx_type=="men_cd4" ){
        tempvec <- dat$pop$sex[eligible_agents_index] 
        tempvec2 <- as.numeric(factor(tempvec,c("f","m")))
        rank_eligible_agents<- rank(-10*tempvec2 - dat$pop$CD4[eligible_agents_index],ties.method="random")
        eligible_agents_receive_tx <- eligible_agents_index[which(rank_eligible_agents<= max_starting_therapy)]
      }
               
      if(dat$param$tx_type=="random" ){
        eligible_agents_receive_tx <- sample(eligible_agents_index,max_starting_therapy)
      }
      if(dat$param$tx_type=="role" ){
        tempvec <- dat$pop$sex[eligible_agents_index] 
        tempvec2 <- as.numeric(factor(tempvec,c("I","R","V")))
        rank_eligible_agents<- rank(tempvec2)
        eligible_agents_receive_tx <- eligible_agents_index[which(rank_eligible_agents<= max_starting_therapy)]    
      }

      if(dat$param$tx_type=="women" ){
        tempvec <- dat$pop$role[eligible_agents_index] 
        tempvec2 <- as.numeric(factor(tempvec,c("I","R","V")))
        rank_eligible_agents<- rank(-tempvec2)
        eligible_agents_receive_tx <- eligible_agents_index[which(rank_eligible_agents<= max_starting_therapy)]    
      }
   }else{  # end of if(subsample)
     eligible_agents_receive_tx <- eligible_agents_index
   } 
  
    dat$pop$treated[eligible_agents_receive_tx] <- 1
    dat$pop$tx_init_time[eligible_agents_receive_tx] <- at
   
    return(dat)
  } else {
    cat("Warning: tx_type, ",dat$param$tx_type," is not a recognized category for social_treatment_module_john_v3\n")
  }
  
return(dat)
}
