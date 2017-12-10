#' @export
viral_update_aim3 <-function(dat,at)
{
  ###########################################################################################################
  #  Update viral loads and CD4 T-cell counts of all infected individuals using a dynamic model             #
  # Contains 6 subfxns:
      #viral_update_modified_logistic_args_list_1)
      #viral_update_modified_logistic_args_list_2)
      #mod_logistic_v5 (rcpp fxn)
  #  Input: "dat" and "at" data structures                                                    #
  #  Primary output: Update dat$pop$V, dat$pop$CD4                                                #
  #  Secondary outputs: Vr, c, K,                                                   #
  ###########################################################################################################

  
  timeIndex          <- at
    
  if (dat$param$PrEP_Model) {
    List_Simulated_Agents  <- which(dat$pop$Status>=0)
  } else {
    List_Simulated_Agents <- which(dat$pop$Status==1)
  }

  for (ind in List_Simulated_Agents) 
   {
  
    # list_1 gives parameters.  list_2 gives variables
    arg_list <- c( viral_update_aim3_args_list_1(dat,dat$param,ind),
                   viral_update_aim3_args_list_2(dat,timeIndex,ind) )
  
   
    # Rcpp routine to update viral loads, infected cell densities, 
    # CD4 T-cell counts, clearance rate and carrying capacity
    #if (arg_list$SecondLineTherapyStarted == 1) {
    #  cat("Second line tx started\n")
    #}
 
    out     <- do.call(viral_dynamics_aim3,arg_list)
    
    dat$pop <- viral_update_aim3_rcpp_output(dat,out,ind)

    #if (arg_list$SecondLineTherapyStarted == 1) {
    # cat("pause here\n")
    #}
 
    
    #calculate number mutations for agent
    if(dat$pop$V[ind] > dat$param$vl_undetectable){
    
    seq0 <- c(1) #0  mutations
    seq1 <- c(2,3,5,9,17) #1 mutations
    seq2 <- c(4,6,7,10,11,13,18,19,21,25) #2 mutations
    seq3 <- c(8,12,14,15,20,22,23,26,27,29) #3 mutatoins
    seq4 <- c(16,24,28,30,31) #5 mutations
    seq5 <- c(32) #5 mutations
 
    # See summary_vl_list for notes about convertion of binary to decimals

    
    mutation_vec <- c( sum(dat$pop$V_vec[ind,seq0]), 
                       sum(dat$pop$V_vec[ind,seq1]),
                       sum(dat$pop$V_vec[ind,seq2]),
                       sum(dat$pop$V_vec[ind,seq3]),
                       sum(dat$pop$V_vec[ind,seq4]),
                       sum(dat$pop$V_vec[ind,seq5]))
    
    no_mutations <- which.max(mutation_vec)-1
    dat$pop$aim3_no_muts[ind] <- no_mutations
    
    if((dat$pop$M_vec[ind,seq0]+dat$pop$L_vec[ind,seq0])>0)
      dat$pop$aim3_mutations_long[ind] <- 0
    if(sum(dat$pop$M_vec[ind,seq1])+sum(dat$pop$L_vec[ind,seq1])>0)
      dat$pop$aim3_mutations_long[ind]<- 1
    if(sum(dat$pop$M_vec[ind,seq2])+sum(dat$pop$L_vec[ind,seq2])>0)
      dat$pop$aim3_mutations_long[ind]<- 2
    if(sum(dat$pop$M_vec[ind,seq3])+sum(dat$pop$L_vec[ind,seq3])>0)
      dat$pop$aim3_mutations_long[ind]<- 3
    if(sum(dat$pop$M_vec[ind,seq4])+sum(dat$pop$L_vec[ind,seq4])>0)
      dat$pop$aim3_mutations_long[ind]<- 4
    if((dat$pop$M_vec[ind,seq5]+dat$pop$L_vec[ind,seq5])>0)
      dat$pop$aim3_mutations_long[ind]<- 5
    
    
    
    
    
    
    }
  } #end of   for (ind in List_HIV_Infected) loop

  return(dat)
}




