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
initialize_vl_cd4_list <- function(dat,at)
{
#vl_list saves the viral load of each agent each timestep
#only want to do this for short/small models runs for qaqc steps  
if(dat$param$save_vl_list){
  dat$vl_list<-vector('list',length=dat$param$n_steps)
  #populate vl/cd4 list for timestep 1
  vl_ix <- which(dat$attr$Status>=0)
  
  if (dat$param$VL_Function == "aim2") {
   dat$vl_list[[at]] <- cbind(dat$attr$id[vl_ix], #Agent
                        dat$attr$V[vl_ix], #VL
                        dat$attr$CD4[vl_ix],#CD4
                        dat$attr$CD4count[vl_ix]/200, # CD4c
                        at) # last element is time
  }

  if (dat$param$VL_Function == "aim3") {
  
    # Define new sequences to identify viruses with 0, 1, 2, 3, or 4 mutations  
    seq0 <- c(1) #0  mutations
    seq1 <- c(2,3,5,9,17) #1 mutations
    seq2 <- c(4,6,7,10,11,13,18,19,21,25) #2 mutations
    seq3 <- c(8,12,14,15,20,22,23,26,27,29) #3 mutatoins
    seq4 <- c(16,24,28,30,31) #5 mutations
    seq5 <- c(32) #5 mutations

    K65R <-  c(2,4,6,8,10,12,14,16, 2+16,4+16,6+16,8+16,10+16,12+16,14+16,16+16) # TDF resistance
    M184V <- c(3,4,7,8,11,12,15,16, 3+16,4+16,7+16,8+16,11+16,12+16,15+16,16+16)  # 3TC
    K103N <- c(5,6,7,8,13,14,15,16, 5+16,6+16,7+16,8+16,13+16,14+16,15+16,16+16)    # EFV
    K103N_gEFV <- c(13,14,15,16, 13+16,14+16,15+16,16+16) # EFV-high
    M184V_K65R <-  c(4,8,12,16, 4+16,8+16,12+16,16+16)# 3TC high / TDF high
    GenericTDF <- c(17:32)
    GenericEFV <- c(9:16,25:32)

  
    dat$vl_list[[at]] <- cbind(dat$attr$id[vl_ix], #Agent
                               dat$attr$V[vl_ix], #VL
                               dat$attr$CD4[vl_ix],#CD4
                               dat$attr$CD4count[vl_ix]/200, # CD4c
                               at, #Time
                               dat$V_vec[vl_ix,seq0], # Muts0
                               rowSums(dat$V_vec[vl_ix,seq1,drop=F]), # Muts1
                               rowSums(dat$V_vec[vl_ix,seq2,drop=F]), # Muts2
                               rowSums(dat$V_vec[vl_ix,seq3,drop=F]), # Muts3
                               rowSums(dat$V_vec[vl_ix,seq4,drop=F]), # Muts4
                               dat$V_vec[vl_ix,seq5], # Muts5
                               dat$attr$Drug1[vl_ix], #D1
                               dat$attr$Drug2[vl_ix], #D2
                               dat$attr$Drug3[vl_ix], #D3
                               rowSums(dat$attr$I_vec[vl_ix,,drop=F]), #I
                               rowSums(dat$attr$M_vec[vl_ix,,drop=F]), #M
                               rowSums(dat$attr$L_vec[vl_ix,,drop=F]), #L
                               rowSums(dat$V_vec[vl_ix,K65R,drop=F])/dat$attr$V[vl_ix],#K65R
                               rowSums(dat$V_vec[vl_ix,M184V,drop=F])/dat$attr$V[vl_ix], #M184V
                               rowSums(dat$V_vec[vl_ix,K103N,drop=F])/dat$attr$V[vl_ix], #K103N
                               rowSums(dat$V_vec[vl_ix,K103N_gEFV,drop=F])/dat$attr$V[vl_ix], # K103N + generic EFV 
                               rowSums(dat$V_vec[vl_ix,M184V_K65R,drop=F])/dat$attr$V[vl_ix], #M184VK65R
                               rowSums(dat$V_vec[vl_ix,GenericTDF,drop=F])/dat$attr$V[vl_ix],
                               rowSums(dat$V_vec[vl_ix,GenericEFV,drop=F])/dat$attr$V[vl_ix],
                               dat$attr$Drug4[vl_ix], # Second line therapy
                               dat$V_vec[vl_ix,seq0]/dat$attr$V[vl_ix]) #WT
  } # end of aim 3 initial values
 
} else{dat$vl_list<-NULL}
  
  return(dat)
}
