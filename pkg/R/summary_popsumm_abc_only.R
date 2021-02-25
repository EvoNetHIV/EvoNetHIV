
#' @export
summary_popsumm_abc_only<-function(dat,at){

  #Simplified popsumm just to re-do Kathryn Peebles ABC runs 
  
  #--------------------------------------------------------------
  #needs to happen each time-step
  #fill in necessary epimodel stats for resim_net and edges_correct fxns
  dat <- set_epi(dat,"s.num", at, length(which(dat$attr$Status==0)))
  dat <-  set_epi(dat,"i.num", at, length(which(dat$attr$Status==1)))
  dat <-  set_epi(dat,"num", at, length(which(dat$attr$Status>=0)))
  
  #----------------------------------------------------------------

  #----------------------------------------------------------------
  #calculation of summary stats: main section
    #logical vectors and indices helpful to calculate summary stats
    inf_index     <-  dat$attr$Status == 1
    alive_index   <-  dat$attr$Status >= 0
  
  #---------------------------------------------
  # stats calculated for every run

  dat$epi$prevalence[at]<-length(which(inf_index))/length(which(alive_index))
  
  
  #--------------------------------------------
  #hetero model
  #if (dat$param$model_sex!="msm") {
    
    male_index    <- dat$attr$sex == 1 & dat$attr$Status >= 0
    female_index  <- dat$attr$sex == 0 & dat$attr$Status >= 0
    inf_male_index <- dat$attr$sex == 1 & inf_index
    inf_female_index <- dat$attr$sex == 0 & inf_index
    
    no_females_alive <- length(which(female_index & alive_index))
    no_males_alive <- length(which(male_index & alive_index))
   
    
    # Age vectors to be used in sex- and age-specific prevalence and treatment
    age_15to24 <- findInterval(dat$attr$age, c(15,25)) == 1
    age_15to49 <- findInterval(dat$attr$age, c(15,50)) == 1
    age_25to34 <- findInterval(dat$attr$age, c(25,35)) == 1
    age_35plus <- findInterval(dat$attr$age, c(35,100)) == 1
    
    #if(at>330){browser()}
    
    # Prevalence vectors to be used in model fitting
    prev_15to24   <- length(which(inf_index & age_15to24))/length(which(alive_index & age_15to24))
    prev_15to49   <- length(which(inf_index & age_15to49))/length(which(alive_index & age_15to49))
    
    prev_f_15to24 <- length(which(inf_female_index & age_15to24))/length(which(female_index & age_15to24))
    prev_f_15to49 <- length(which(inf_female_index & age_15to49))/length(which(female_index & age_15to49))
    
    prev_m_15to24 <- length(which(inf_male_index & age_15to24))/length(which(male_index & age_15to24))
    prev_m_15to49 <- length(which(inf_male_index & age_15to49))/length(which(male_index & age_15to49))
    
    dat$epi$prev_15to24[at] <-prev_15to24
    dat$epi$prev_15to49[at] <-prev_15to49
    dat$epi$prev_f_15to24[at] <-prev_f_15to24
    dat$epi$prev_f_15to49[at] <-prev_f_15to49
    dat$epi$prev_m_15to24[at] <-prev_m_15to24
    dat$epi$prev_m_15to49[at] <-prev_m_15to49
    
    
    out1 <- dat$epi$prev_15to49[at] # Ages 15-49, 1990-2001, 2002, 2005, 2008, 2012
    
    out2 <- c(dat$epi$prev_m_15to24[at], # 2002, males 15-24
              dat$epi$prev_f_15to24[at], # 2002, females 15-24
              dat$epi$prev_m_15to49[at], # 2002, males 15-49
              dat$epi$prev_f_15to49[at]) # 2002, females 15-49
    
    out3 <- c(dat$epi$prev_m_15to24[at], # 2005, males 15-24
              dat$epi$prev_f_15to24[at], # 2005, females 15-24
              dat$epi$prev_m_15to49[at], # 2005, males 15-49
              dat$epi$prev_f_15to49[at]) # 2005, females 15-49
    
    out4 <- dat$epi$prev_15to24[at] # 2008, 15-24
    
    out5 <- c(dat$epi$prev_m_15to24[at], # 2012, males 15-24
              dat$epi$prev_f_15to24[at], # 2012, females 15-24
              dat$epi$prev_m_15to49[at], # 2012, males 15-49
              dat$epi$prev_f_15to49[at]) # 2012, females 15-49
    
    out <- c(out1, out2, out3, out4, out5)
    
    
    #if(at %% 100  == 0){
    #print(at)
    #print( round( dat$epi$prevalence[at],3))
    #print(round(out,2))
    #}
    
    

  #################################################
  return(dat)
}

