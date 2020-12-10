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
summary_misc <- function(dat,at){
#browser()
  #Description:
  #0) fills in "pop" list (permanent record of agents), when
  #   agents die/age-out
  #1)fills in vl_list (inidividual agent vl per timstep - if flagged)
  #3)saves dat$discord_coital_df if flagged (qaqc to look at acts per agent/couple)
  #4)removed
  #5)prints summary stats to screen
  #6) subset out age data for graphing
  #7) add sessionInfo to output object
  
  
#-----------------------------------------------------------------
#0 fill in "pop" list (permanent record of agents)
# wen agents die/age-out
ix <- which(dat$attr$Time_Death == at)
if(length(ix)>0 & at != dat$param$n_steps){  
     if(length(dat$pop)==0){
          if(dat$param$VL_Function != "aim3"){ 
              dat$pop <- rep(list(NULL),length(dat$attr))  
              names(dat$pop) <- names(dat$attr)
          }else{
            vector_flag <- unlist(lapply(dat$attr,function(x) is.vector(x)))
            non_vectors <- which(!vector_flag) #e.g., aim3 matrices etc.
            attr_list <- dat$attr[-non_vectors]
            dat$pop <- rep(list(NULL),length(attr_list))#remove aim3 matrices
            #browser()
            names(dat$pop) <- names(attr_list)
           }
     }
 
     for(ii in 1:length(ix)){
       agent_index <- ix[ii]
       if(dat$param$VL_Function != "aim3"){ 
           agent_attr <- lapply(1:length(dat$attr), function(xx) dat$attr[[xx]][agent_index ] )
           dat$pop <- lapply(1:length(dat$pop),function(xx) c(dat$pop[[xx]],agent_attr[[xx]]) )
           names(dat$pop) <- names(dat$attr)
      }else{

            #if aim3 don't save attributes that are matrices
            vector_flag <- unlist(lapply(dat$attr,function(x) is.vector(x)))
            non_vectors <- which(!vector_flag) #e.g., aim3 matrices etc.
            attr_list <- dat$attr[-non_vectors]
            agent_attr <- lapply(1:length(attr_list), function(xx) attr_list[[xx]][agent_index ] )
            pop_names <- names(dat$pop)
            dat$pop <- lapply(1:length(dat$pop),function(xx) c(dat$pop[[xx]],agent_attr[[xx]]) )
           if(length(dat$pop) != length(attr_list) ){browser()}
             names(dat$pop) <- names(attr_list)
        
      }
     }
} #end of if(length(ix)>0) & at != dat$param$n_steps){
 
#if end of model run, save all agent attributes
if(at == dat$param$n_steps){
  
   if(dat$param$VL_Function != "aim3"){ 
        #last time-step, fill in all agents on dat$attr
        dat$pop <- lapply(1:length(dat$pop),function(xx) c(dat$pop[[xx]],dat$attr[[xx]]) )
        names(dat$pop) <- names(dat$attr)
    }else{
      # browser()
          vector_flag <- unlist(lapply(dat$attr,function(x) is.vector(x)))
          non_vectors <- which(!vector_flag) #e.g., aim3 matrices etc.
          attr_list <- dat$attr[-non_vectors]
          aa=try({
            out <- lapply(1:length(dat$pop),function(xx) c(dat$pop[[xx]],attr_list[[xx]]) )
          })
          if(class(aa)=="try-error"){browser()}
          dat$pop <- out
          if(length(names(dat$pop) != names(attr_list))){browser()}
          names(dat$pop) <- names(attr_list)
    }
}

  
#1
#populate vl/cd4 list

 if(dat$param$popsumm_frequency==1)
 {
   if ((at == 2) || (at %% dat$param$print_frequency == 0)) {
     dat <- summary_vl_list(dat,at)
   }
 }
 if(dat$param$popsumm_frequency>1)
 {
   if (at%%dat$param$popsumm_frequency==0) {
      dat <- summary_vl_list(dat,at)
   }
  }
  

#-----------------------------------------------------------------
#2 removed

#-----------------------------------------------------------------
#3
if(dat$param$save_coital_acts)
  dat$coital_acts_list[[at-1]] <- dat$discord_coital_df  

#-----------------------------------------------------------------

#4 removed

#-----------------------------------------------------------------
#5 (Version 5a -- Refresh screen very time step)
# 
# if(!dat$param$hpc & !dat$param$scrolling_output){
# 
#   cat("\f")
#   cat("\nEvoNet HIV Simulation")
#   cat("\n----------------------------")
#   cat("\nModel name:" ,dat$param$model_name)
#   cat("\nSimulation:",dat$simulation)
#   cat("\nTimestep: ", at, "/", dat$control$nsteps,  sep = " ")
#   cat("\nTotal population (alive):",  dat$epi$alive[at])
#   cat("\nMean SPVL (untreated):",  dat$epi$mean_spvl_pop_untreated[at])
#   cat("\nTotal infections:", dat$epi$total_infections_alive[at])
#   cat("\nTotal susceptibles:",  dat$epi$susceptibles[at] )
#   
#   cat("\nAIDS deaths", sum(dat$epi$aids_deaths[1:at],na.rm=T))
#   cat("\nOther deaths", sum(dat$epi$natural_deaths[1:at],na.rm=T))
#   cat("\nAged-out", sum(dat$epi$aged_out[1:at],na.rm=T))
#   
#   cat("\n----------------------------")
# }


# 5 (Version 5b -- Don't over-write screen each time step)
if(!dat$param$hpc & dat$param$scrolling_output){
  
  if (at == 2) {
    cat("\nStarting simulation of ",dat$control$nsteps," time steps\n")
    cat ("Sim\t Time\t Alive\t Inf\t Sus\t VL \t SPVL\n") #dAIDS\t dNat\t AgeOut\n")# Pills\n")
  }
  
  
  if( (at%%dat$param$popsumm_frequency==0)){
    index <- (at/dat$param$popsumm_frequency)-1
    cat(
      dat$simulation,"\t",
      at,"\t",
      dat$epi$alive[index],"\t",
      dat$epi$total_infections_alive[index],"\t",
      #dat$epi$total_infections_not_treated[at],"\t",
      dat$epi$susceptibles[index],"\t",
      round(dat$epi$mean_vl_pop_all[index],2),"\t",
      round(mean(dat$attr$LogSetPoint[which(dat$attr$treated!=1)],na.rm=T),2),"\n" )#,"\t",
      #dat$no_deaths_aids,"\t",
      #dat$no_deaths_natural,"\t",
      #dat$no_aged_out,"\n")
       #dat$epi$total_pills_taken[time_index],"\n")
  }
}

if (at == dat$control$nsteps) { # Remind users what the columns mean
  cat ("Sim\t Time\t Alive\t Inf\t Sus\t VL \t SPVL\n") # dAIDS\t dNat\t AgeOut\n")# Pills\n")
}

#----------------------------------------------
#6
if(at==2)
dat$age_list[[1]]<-dat$attr$age[which(dat$attr$Status>=0)]
if(at==round(dat$param$n_steps*.25))
dat$age_list[[2]]<-dat$attr$age[which(dat$attr$Status>=0)]
if(at==round(dat$param$n_steps *.50))
dat$age_list[[3]]<-dat$attr$age[which(dat$attr$Status>=0)]
if(at==round(dat$param$n_steps *.75))
dat$age_list[[4]]<-dat$attr$age[which(dat$attr$Status>=0)]
if(at==dat$param$n_steps)
dat$age_list[[5]]<-dat$attr$age[which(dat$attr$Status>=0)]

#-----------------------------------------------------------------
#7
if(at == dat$param$n_steps){    
  dat$sessionInfo <-  sessionInfo()
}

#-----------------------------------------------------------------
#8
#-----------------------------------------------------------------
#9
 if(dat$param$fast_edgelist & dat$param$save_partner_list){
   if(at==2){
     aa <- dat$el[[1]]
     attr(aa,"changes")=cbind(aa,1)
     bb <- attr(aa,"changes")
     if(length(bb)>0){dat$partner_list[[at]] <- bb
     dat$partner_list[[at]][,1] <- dat$attr$id[dat$partner_list[[at]][,1]]
     dat$partner_list[[at]][,2] <- dat$attr$id[dat$partner_list[[at]][,2]]
     }
     
   }else{
     bb <- attr(dat$el[[1]],"changes")
     if(length(bb)>0){dat$partner_list[[at]]<- bb
     dat$partner_list[[at]][,1] <- dat$attr$id[dat$partner_list[[at]][,1]]
     dat$partner_list[[at]][,2] <- dat$attr$id[dat$partner_list[[at]][,2]]
     }
   }
   
   if(at==dat$param$n_steps){
     dat$partner_list <- summary_partner_list(dat)
   }
   
 }

return(dat)
}
