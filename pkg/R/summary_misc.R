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
  #1)fills in vl_list (inidividual agent vl per timstep - if flagged)
  #2)viral and spvl lineage attributes per agent filled in if last timestep
  #3)saves dat$discord_coital_df if flagged (qaqc to look at acts per agent/couple)
  #4)updates dat$nwparam[[1]]$coef.form[1] (epimodel's edges correct fxn)
  #5)prints summary stats to screen
  #6) subset out age data for graphing
  #7) add sessionInfo to output object
  #8) add evonet version to output object
  
#-----------------------------------------------------------------
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
#2

#at last timestep, assign "founder" lineage to each infected
#(except for initially infected, who are the founders)
#and assign founder spvl
#if(at == dat$param$n_steps){    
#  viral_list <-  summary_viral_lineage(dat$pop)
#  dat$pop <-  viral_lineage_fnx2(vl_list = viral_list, poplist= dat$pop)
#  dat$pop <-  summary_spvl_lineage(poplist = dat$pop)
#}

#-----------------------------------------------------------------
#3
if(dat$param$save_coital_acts)
  dat$coital_acts_list[[at-1]] <- dat$discord_coital_df  

#-----------------------------------------------------------------
#4

#EpiModel's edges correct fxn here, just for MSM!, need to put hetero/bipartite part in
if(dat$param$popsumm_frequency==1)
{
old.num <- dat$popsumm$alive[at-1]
new.num <- dat$popsumm$alive[at]
dat$nwparam[[1]]$coef.form[1] <- ( dat$nwparam[[1]]$coef.form[1] + 
                                     log(old.num) - log(new.num) )
}else{
  if(at%%dat$param$popsumm_frequency==0 & at>2){
    index1=(at/dat$param$popsumm_frequency)+1
    index2=(at/dat$param$popsumm_frequency)
    old.num <- dat$popsumm$alive[index2]
    new.num <- dat$popsumm$alive[index1]
    dat$nwparam[[1]]$coef.form[1] <- ( dat$nwparam[[1]]$coef.form[1] + 
                                         log(old.num) - log(new.num) )
  }
}

#4

#EpiModel's edges correct fxn here, just for MSM!, need to put hetero/bipartite part in
if(dat$param$model_sex=="msm"){
  if(dat$param$popsumm_frequency==1)
  {
    old.num <- dat$popsumm$alive[at-1]
    new.num <- dat$popsumm$alive[at]
    dat$nwparam[[1]]$coef.form[1] <- ( dat$nwparam[[1]]$coef.form[1] + 
                                         log(old.num) - log(new.num) )
  }else{
    if(at%%dat$param$popsumm_frequency==0 & at>2){
      index1=(at/dat$param$popsumm_frequency)+1
      index2=(at/dat$param$popsumm_frequency)
      old.num <- dat$popsumm$alive[index2]
      new.num <- dat$popsumm$alive[index1]
      dat$nwparam[[1]]$coef.form[1] <- ( dat$nwparam[[1]]$coef.form[1] + 
                                           log(old.num) - log(new.num) )
    }} #end of msm, popsumm_frequency !=1
}#end of msm
else{
mode <- dat$attr$sex
if(dat$param$popsumm_frequency==1){
  old.num.m1 <- dat$popsumm$alive_female[at - 1]
  old.num.m2 <- dat$popsumm$alive_male[at - 1]
  new.num.m1 <- sum(dat$attr$active == 1 & mode =="f")
  new.num.m2 <- sum(dat$attr$active == 1 & mode == "m")
  dat$nwparam[[1]]$coef.form[1] <- (dat$nwparam[[1]]$coef.form[1]+
        log(2 * old.num.m1 * old.num.m2/(old.num.m1 +
         old.num.m2)) - log(2 * new.num.m1 *
         new.num.m2/(new.num.m1+new.num.m2)))
}else{
  if(at%%dat$param$popsumm_frequency==0 & at>2){
    index1=(at/dat$param$popsumm_frequency)+1
    index2=(at/dat$param$popsumm_frequency)
    old.num.m1 <- dat$popsumm$alive_female[index2]
    old.num.m2 <- dat$popsumm$alive_male[index1]
    new.num.m1 <- sum(dat$attr$active == 1 & mode =="f")
    new.num.m2 <- sum(dat$attr$active == 1 & mode == "m")
    dat$nwparam[[1]]$coef.form[1] <- (dat$nwparam[[1]]$coef.form[1]+
        log(2 * old.num.m1 * old.num.m2/(old.num.m1 +
        old.num.m2)) - log(2 * new.num.m1 *
        new.num.m2/(new.num.m1+new.num.m2)))
  }
}#end of hetero, popsumm_frequency!=1
}#end of hetero part

#-----------------------------------------------------------------
#5 (Version 5a -- Refresh screen very time step)

if(!dat$param$hpc & !dat$param$scrolling_output){

  cat("\f")
  cat("\nEvoNet HIV Simulation")
  cat("\n----------------------------")
  cat("\nModel name:" ,dat$param$model_name)
  cat("\nSimulation:",dat$simulation)
  cat("\nTimestep: ", at, "/", dat$control$nsteps,  sep = " ")
  cat("\nTotal population (alive):",  dat$popsumm$alive[at])
  cat("\nMean SPVL (untreated):",  dat$popsumm$mean_spvl_pop_untreated[at])
  cat("\nTotal infections:", dat$popsumm$total_infections_alive[at])
  cat("\nTotal susceptibles:",  dat$popsumm$susceptibles[at] )
  
  cat("\nAIDS deaths", sum(dat$popsumm$aids_deaths[1:at],na.rm=T))
  cat("\nOther deaths", sum(dat$popsumm$natural_deaths[1:at],na.rm=T))
  cat("\nAged-out", sum(dat$popsumm$aged_out[1:at],na.rm=T))
  
  cat("\n----------------------------")
}

# 5 (Version 5b -- Don't over-write screen each time step)
if(!dat$param$hpc & dat$param$scrolling_output){
  if (at <= 2) {
    cat("\nStarting simulation of ",dat$control$nsteps," time steps\n")
  }
  if (at <= 2) {
    cat ("sim\t time\t Tot\t Inf\t InfNot\t Sus\t VL \t SPVL\t dAIDS\t dNat\t AgeOut\t Pills\n")
  }
  
  if(dat$param$popsumm_frequency==1)
  {
  if ((at == 2) || (at %% dat$param$print_frequency == 0)) {
      
    cat(
      dat$simulation,"\t",
      at,"\t",
      dat$popsumm$alive[at],"\t",
      dat$popsumm$total_infections_alive[at],"\t",
      dat$popsumm$total_infections_not_treated[at],"\t",
      dat$popsumm$susceptibles[at],"\t",
      round(dat$popsumm$mean_vl_pop_all[at],2),"\t",
      round(dat$popsumm$mean_spvl_pop_untreated[at],2),"\t",
      sum(dat$popsumm$aids_deaths[1:at],na.rm=T),"\t",
      sum(dat$popsumm$natural_deaths[1:at],na.rm=T),"\t",
      sum(dat$popsumm$aged_out[1:at],na.rm=T),"\t",
      dat$popsumm$total_pills_taken[at],"\n")
  }
  }
  if(dat$param$popsumm_frequency>1)
  {
    if (at%%dat$param$popsumm_frequency==0) {
         time_index=(at/dat$param$popsumm_frequency)+1
         
        cat(
        dat$simulation,"\t",
        at,"\t",
        dat$popsumm$alive[time_index],"\t",
        dat$popsumm$total_infections_alive[time_index],"\t",
        dat$popsumm$total_infections_not_treated[time_index],"\t",
        dat$popsumm$susceptibles[time_index],"\t",
        round(dat$popsumm$mean_vl_pop_all[time_index],2),"\t",
        round(dat$popsumm$mean_spvl_pop_untreated[time_index],2),"\t",
        sum(dat$popsumm$aids_deaths[1:time_index],na.rm=T),"\t",
        sum(dat$popsumm$natural_deaths[1:time_index],na.rm=T),"\t",
        sum(dat$popsumm$aged_out[1:time_index],na.rm=T),"\t",
        dat$popsumm$total_pills_taken[time_index],"\n")
    }
  }
   if (at == dat$control$nsteps) { # Remind users what the columns mean
     cat ("sim\t time\t Tot\t Inf\t InfNot\t Sus\t VL \t SPVL\t dAIDS\t dNat\t AgeOut\t Pills\n")
   }

}
#----------------------------------------------
#6
if(at==2)
dat$age_list[[1]]<-dat$pop$age[which(dat$pop$Status>=0)]
if(at==round(dat$param$n_steps*.25))
dat$age_list[[2]]<-dat$pop$age[which(dat$pop$Status>=0)]
if(at==round(dat$param$n_steps *.50))
dat$age_list[[3]]<-dat$pop$age[which(dat$pop$Status>=0)]
if(at==round(dat$param$n_steps *.75))
dat$age_list[[4]]<-dat$pop$age[which(dat$pop$Status>=0)]
if(at==dat$param$n_steps)
dat$age_list[[5]]<-dat$pop$age[which(dat$pop$Status>=0)]

#-----------------------------------------------------------------
#7
if(at == dat$param$n_steps){    
  dat$sessionInfo <-  sessionInfo()
}

#-----------------------------------------------------------------
#8
if(at == dat$param$n_steps){
  #note: working directory needs to be versioned,
  aa<-try(system("svnversion"),silent=TRUE)
  if(aa==0)
  dat$evonet_version <-  system("svnversion")
}

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
