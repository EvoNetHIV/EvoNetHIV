#' @title Initialize viral load and SPVL values
#'
#' @param dat master data object
#' @param at timestep
#' @return 'dat' object with agent attributes for viral load and SPVL calculated.
#' @details
#' Subfunction in 'initialize_module'. Calculates initial viral load and associated values for initial infected agents.
#' @examples
#' initialize_infecteds_vl(dat,at=1)


#' @export
initialize_infecteds_vl <- function(dat,at)
{
  #Description:
  # Sets up initial viral load and associated values for initial infected individuals 
  # including contributions to SPVL from viral features and the environment
  # (direct porting from virulence model)
  # inputs: dat$param, dat$pop, at, 
  # outputs: dat$pop including values for acute_inf_flat_vl_index, acute_inf_prepeak_index, post_acute_inf_index_1,
  # post_acute_inf_index_2, and aids_index, pop$ViralContribToLogSP0, pop$EnvirContribToLogSP0, pop$LogSetPoint, 
  # pop$SetPoint,pop$d_acute, pop$Status, pop$Generation, pop$disclosure_status, pop$Adherence 
  
  param <-  dat$param 
  pop   <-  dat$pop
  timeIndex  <- at
  
  # EpiModel determines infected status in init_status.net and puts it in "dat$attr$status
  # ind represents infected individuals of initial population
  ind <- which(dat$attr$status=="i")
  
  # If using "time_dist" tx_type, assign a distribution of treatment initiation delay times
  # to infected agents.
  if(!is.logical(param$tx_type)){
  if(param$tx_type == "time_dist" | param$tx_type == "cd4_and_time_dist") {
    pop$min_time_tx[ind] <- rnorm(length(ind), param$mean_time_tx, param$sd_time_tx)
  }
 }
  
  #initial infecteds not treated
  pop$treated[ind] <- 0
  pop$treated_2nd_line[ind] <- 0
  
  pop$Drug1[ind] <- 0
  pop$Drug2[ind] <- 0
  pop$Drug3[ind] <- 0
  pop$Drug4[ind] <- 0
  pop$Aim3RoundingErrors[ind] <-0
  
  if(length(ind)==0){
    stop("hmmm...no infecteds in initial population.....
         that is definitely weird...")
  }
  
  #create "genetic" and "environmental" components to spvl
  pop$ViralContribToLogSP0[ind] <- (rnorm(n = param$initial_infected,
                                          mean = param$AverageLogSP0,
                                          sd = sqrt(param$h^2 * param$VarianceLogSP0)))
  

  pop$EnvirContribToLogSP0[ind] <- rnorm(n = param$initial_infected,
                    mean = 0,sd = sqrt((1-param$h^2)*param$VarianceLogSP0))
  
  #calculate spvl (temp_spvl) and constrain spvl values to min/max allowed
  temp_spvl<- (pop$ViralContribToLogSP0[ind] + pop$EnvirContribToLogSP0[ind])
  if(any(temp_spvl< dat$param$min_spvl_allowed))
    temp_spvl[temp_spvl< dat$param$min_spvl_allowed] <- dat$param$min_spvl_allowed
  if(any(temp_spvl> dat$param$max_spvl_allowed))
    temp_spvl[temp_spvl> dat$param$max_spvl_allowed] <- dat$param$max_spvl_allowed
  
  #final spvl values
  pop$LogSetPoint[ind] <- temp_spvl
  pop$SetPoint[ind] <- "^"(10.0,pop$LogSetPoint[ind])
  pop$vl_peak_agent[ind] <- 10^(4.639 + 0.495*pop$LogSetPoint[ind])
  if(param$vl_peak_agent_flag){
    vl_peak <-pop$vl_peak_agent[ind]
    pop$r0[ind] <- log(vl_peak / param$V0) /param$t_peak
    
  }else{
    vl_peak <- param$vl_peak_acute
    }
  pop$vl_phase2_trans[ind] = exp((2.5*log(pop$SetPoint[ind])+
                                log(vl_peak ))/3.5)

  pop$rate_phase2[ind] = -1*(log(pop$vl_phase2_trans[ind]/pop$SetPoint[ind])/
                            (param$t_acute - param$t_acute_phase2))
  
  dat$pop$d_acute[ind] <- (log(vl_peak /
                                       pop$vl_phase2_trans[ind]) /
                                   (param$t_acute_phase2- param$t_peak))                      
  
  # Viruses with per-pathogen-pathongenecities (PPPs) greater than 1.0, respectively, kill hosts faster than expected given their VL.
  pop$PPP[ind] <-runif(n = param$initial_infected,
                                            min = 1.0, max= 1.0 + param$MaxPPP)
  
  pop$SetPoint[ind] <- "^"(10.0,pop$LogSetPoint[ind])
  
  #Status=1 means infected
  pop$Status[ind] <- 1
  #Generation=1 means founder
  pop$Generation[ind] <- 1
  #disclosure status: 0/1 does infected agent tell partnter hiv status,
  #influences condom usage/sex frequency
  pop$disclosure_status[ind][runif(length(ind)) < param$disclosure_prob] <- 1 #default is zero
  #age at infection
  pop$age_infection[ind] <- pop$age[ind]
  #calculation of time to aids based on gamma distribution
  aa <- param$Dmax * (param$D50^param$Dk)
  bb <- "^"(  pop$SetPoint[ind],param$Dk) + "^"(param$D50,param$Dk)
  ExpectedDelayTime <- aa/bb
  theta <-  ExpectedDelayTime / param$shape_parameter
  pop$RandomTimeToAIDS[ind] <-  rgamma(n=length(ind),
                                       param$shape_parameter,
                                       1/theta)

  if (dat$param$VL_Function=="aim3") {
    pop$Time_Inf[ind] <- 0.0
    pop$Time_Inf_Adj[ind] <- pop$Time_Inf[ind]
  }else{
   #assign a random time of infection for founders 
      pop$Time_Inf[ind] <-   round(-param$t_acute -
                                   ( runif(param$initial_infected)*
                                       (param$max_time_inf_initial_pop-param$t_acute) ))
      pop$Time_Inf_Adj[ind] <- pop$Time_Inf[ind]
  } 

  #for model runs with vaccination campaigns,
  #assigns sensitivity status of virus (0/1) to vaccine
  pop$virus_sens_vacc[ind] <- rbinom(length(ind),1,
                              prob=dat$param$perc_virus_vaccine_sens)
  
  if(dat$param$vacc_multi_eff){dat$pop$vacc_eff[ind] <- dat$param$vacc_multi_fxn(length(ind))}
  
  
  #give all initial infected the initial vl value, which will be updated
  #in viral_update_gamma to appropriate level (still in initialization module)
  pop$V[ind]<-param$V0
  
  #need to give reasonable value to those in aids
  #due to how "viral_update_gamma" fxn works
  #if just set to V0, then appropriate VL value (for those in aids)
  #will not be computed
  #this gives those in aids, a value halfway in log10 space between
  # spvl and max aids vl
  if(param$VL_Function!="aim3"){
    aids_index <-  which(timeIndex > ( pop$Time_Inf[ind] +pop$RandomTimeToAIDS[ind]))
    if(length(aids_index)>0){
      pop$V[aids_index]<- pop$SetPoint[aids_index] + param$vl_max_aids  # Fixes obvious error in original
    }
  }
  
  ###############################
  #Aim3 dynamics
  
  initial_vec <- rep(0,"^"(2, param$Max_Allowable_Loci))                
  pop$V_vec[ind,] <- initial_vec
  pop$I_vec[ind,] <- initial_vec
  pop$M_vec[ind,] <- initial_vec
  pop$L_vec[ind,] <- initial_vec
  
  pop$V_vec[ind,1] <- pop$V[ind] # Assume 100% WT at time 0. (everything else left at zero)
  pop$I_vec[ind,1] <- pop$V[ind] * param$c / param$p_inf_cells
  
  pop$CD4count[ind] <- param$s_CD4 /param$m_CD4 # Initial concentration of CD4 T-cells
  pop$CD4tot[ind] <- param$s_CD4 /param$m_CD4 # Initial concentration of CD4 T-cells
  pop$Imm_Trig[ind] <- 0
  pop$ChronPhase[ind] <- 0
  pop$OnDrug[ind] <- 0
  pop$K[ind] <- pop$SetPoint[ind]
  pop$CD4count[ind] <- 1000
  
  dat$pop <- pop
  return(dat)
  }
###################################################
