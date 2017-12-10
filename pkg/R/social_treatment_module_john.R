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
social_treatment_module_john <- function(dat, at)
{
  #Description
  #Determines which infected,diagnosed,eligible-for-care agents gets treatment
  #given treatment campaign in effect
  #possible treatment scenarios:c("VL3","VL4","fifo","CD42","generic_attr","random",
  #"MS1","MS2")
  #Inputs: 
    #param$start_treatment_campaign
    #pop$treated
    #pop$Status
    #pop$diag_status
    #param$tx_type
    #param$max_treated
    #param$tx_in_acute_phase
 #Output:
    #pop$treated
    #pop$tx_init_time
  
  
  if(at < dat$param$start_treatment_campaign){
    return(dat)
  }
  
  if(length(which(dat$pop$Status==1))==0){return(dat)}
  
  eligible_agents_index <- which(dat$pop$Status == 1 & 
                                   dat$pop$treated == 0 &
                                   dat$pop$eligible_care == 1 & 
                                   dat$pop$diag_status == 1) 
  
  if(length(eligible_agents_index)==0){return(dat)}
  
  no_on_tx <- length(which(dat$pop$treated==1 & dat$pop$Status==1))
  
  #------------------------------------------------------------------------  
  # this section for treatment scenarios where there is limint to number treated (max_treated)  
  if(dat$param$tx_type %in% c("VL3","VL4","fifo","CD42","generic_attr","random",
                              "MS1","MS2")){
    
    if(at>=dat$param$start_treatment_campaign & is.na(dat$param$max_treated)){
      dat$param$max_treated <- trunc(dat$param$proportion_treated*length(eligible_agents_index))
      dat$param$max_treated_diff <- length(which(dat$pop$Status==1)) - dat$param$max_treated
    }
    
    if(dat$param$max_treated==0){return(dat)}
    if(no_on_tx >= dat$param$max_treated){return(dat)}
    
    if(dat$param$tx_in_acute_phase){
      eligible_agents_index_acute_flag<-eligible_agents_index
    }else{
      eligible_agents_index_acute_flag <- eligible_agents_index[which((at - dat$pop$Time_Inf[eligible_agents_index]) > dat$param$t_acute)]
    }
   
    length_eligible_agents_index_acute_flag <- length(eligible_agents_index_acute_flag)
    
    if(length_eligible_agents_index_acute_flag==0){return(dat)}
    
    max_starting_therapy <- dat$param$max_treated - no_on_tx 
    subsample <- length_eligible_agents_index_acute_flag > max_starting_therapy
   
    if(subsample){      #if more elibible for tx than allowed, need to subset

      if(dat$param$tx_type=="VL3" ){
        rank_eligible_agents_acute_flag<- rank(-dat$pop$V[eligible_agents_index_acute_flag],ties.method="random")
        eligible_agents_acute_flag_receive_tx <- eligible_agents_index_acute_flag[which(rank_eligible_agents_acute_flag<= max_starting_therapy)]
      }
      
      if(dat$param$tx_type=="VL4" ){
        rank_eligible_agents_acute_flag<- rank(dat$pop$V[eligible_agents_index_acute_flag],ties.method="random")
        eligible_agents_acute_flag_receive_tx <- eligible_agents_index_acute_flag[which(rank_eligible_agents_acute_flag<= max_starting_therapy)]
      }
      
      if(dat$param$tx_type=="fifo" ){
        rank_eligible_agents_acute_flag<- rank(-(at - dat$pop$diag_time[eligible_agents_index_acute_flag]),ties.method="random")
        eligible_agents_acute_flag_receive_tx <- eligible_agents_index_acute_flag[which(rank_eligible_agents_acute_flag<= max_starting_therapy)]    
      }
      
      if(dat$param$tx_type=="CD42" ){
        rank_eligible_agents_acute_flag<- rank(-dat$pop$CD4[eligible_agents_index_acute_flag],ties.method="random")
        eligible_agents_acute_flag_receive_tx <- eligible_agents_index_acute_flag[which(rank_eligible_agents_acute_flag<= max_starting_therapy)]
      }
      
      if(dat$param$tx_type=="generic_attr" ){
        rank_eligible_agents_acute_flag<- rank(-dat$pop$att1[eligible_agents_index_acute_flag],ties.method="random")
        eligible_agents_acute_flag_receive_tx <- eligible_agents_index_acute_flag[which(rank_eligible_agents_acute_flag<= max_starting_therapy)]    
      }
          
  
      if(dat$param$tx_type=="VL3" ){
        rank_eligible_agents_acute_flag<- rank(-dat$pop$V[eligible_agents_index_acute_flag],ties.method="random")
        eligible_agents_acute_flag_receive_tx <- eligible_agents_index_acute_flag[which(rank_eligible_agents_acute_flag<= max_starting_therapy)]
      }
      
      if(dat$param$tx_type=="MS2" ){
        rank_eligible_agents_acute_flag<- rank(-(5 - dat$pop$att1[eligible_agents_index_acute_flag] + 0.3333*log10(dat$pop$V[eligible_agents_index_acute_flag])),
                                               ties.method="random")
        eligible_agents_acute_flag_receive_tx <- eligible_agents_index_acute_flag[which(rank_eligible_agents_acute_flag<= max_starting_therapy)]    
      }
      
      if(dat$param$tx_type=="MS1" ){
        tempvec <- dat$pop$att1[eligible_agents_index_acute_flag]+log10(dat$pop$V[eligible_agents_index_acute_flag]) 
        rank_eligible_agents_acute_flag<- rank(-tempvec,ties.method="random")
        eligible_agents_acute_flag_receive_tx <- eligible_agents_index_acute_flag[which(rank_eligible_agents_acute_flag<= max_starting_therapy)]    
      }
 
      if(dat$param$tx_type=="random" ){
        eligible_agents_acute_flag_receive_tx <- sample(eligible_agents_index_acute_flag,max_starting_therapy)
      }
   }else{  # end of if(subsample)
     eligible_agents_acute_flag_receive_tx <- eligible_agents_index_acute_flag
   } 
  
    dat$pop$treated[eligible_agents_acute_flag_receive_tx] <- 1
    dat$pop$tx_init_time[eligible_agents_acute_flag_receive_tx] <- at
   
    return(dat)
  } #end of   if(dat$param$tx_type %in% c("VL3","fifo","CD42","generic_attr","random")){

  
return(dat)
}
