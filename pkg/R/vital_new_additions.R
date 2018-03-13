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
new_additions_fxn <- function(input_list,dat,index,type=c("births","initial"),at)
{
  #description:
  #main argument: type="births" or type="initial"
  #fills in agent attribute "pop" list; some variables get same value for
  #initial population and for new additions ("births") while others get
  #different values based on whether its for the initial population or 
  #for new additions. An example is age, ages for initial popn can take a
  #number of distributions/values but ages for births are always the min. age.
  #input: dat$dat$param,dat$pop
  #output: dat$pop
  
  #"pop" attributes with different values/distributions for initial pop vs births:
  #age,last_neg_test,arrival_time,role, att1 (generic attribute),

  total_new <- length(index)
  #------------------------------
  #functions for these variables are same initial population and for new births
      
  input_list$s[index]<- rep(dat$param$prog_rate,total_new)

  input_list$id[index] <-  index   
  
  #Assume new entrants (births) and immigrants aren't treated.
  input_list$treated[index] <-  rep(0,total_new)  
  input_list$vaccinated[index] <- rep(0, total_new)
  input_list$vacc_rr[index] <- rep(1, total_new)
  
  input_list$Status[index] <-  rep(0,total_new)   
  
  input_list$NumRecipients[index] <-  rep(0,total_new)
  
  input_list$sti_status[index] <- rbinom(total_new,1,dat$param$sti_prob)
  
  input_list$circum[index] <- rbinom(total_new,1,dat$param$circum_prob)

  input_list$eligible_care[index] <- rbinom(total_new,1,dat$param$prob_care)
  
  input_list$eligible_ART[index] <- rbinom(total_new,1,dat$param$prob_eligible_ART)
 
  input_list$CD4_nadir[index] <- 1
  
  input_list$CD4[index] <- 1   # categorical value 1: CD4 > 500,..., 4: CD4: <200
  
  input_list$CD4_at_trtmnt[index] <- NA
   
  input_list$r0[index] <- dat$param$r0

  #-----------------------------
  #these variables need different functions for initial population and births
  if(type=="initial")
  {
    # note: for attributes "sex","att1","role", initial values created in 
    # "setup_nw()" and set on nw, these values then transferred to "pop" list
    
 
    input_list$sex[index] <- dat$attr$sex
    input_list$age[index] <- dat$attr$age
    input_list$age_cat[index] <- c(1, 2)[findInterval(x = dat$attr$age, vec = c(14, 25))]
    
    # Assign generic nodal attribute values
    if(!is.logical(dat$param$generic_nodal_att_values)){
      input_list$att1[index] <- dat$attr$att1

      if(!is.logical(dat$param$sti_prob_att)) {
        att_ix <- lapply(1:dat$param$generic_nodal_att_no_categories, function(x) index[which(input_list$att1 == dat$param$generic_nodal_att_values[x])])
        
        for(ii in 1:length(att_ix)) {
          input_list$sti_status[(att_ix[[ii]])] <- rbinom(length(att_ix[[ii]]), 1, dat$param$sti_prob_att[ii])
        }
      }
    }
    
    # Assign role
    if(!is.logical(dat$param$role_props) && dat$param$model_sex=="msm"){
      input_list$role[index] <- dat$attr$role
      
      temp <- index[input_list$role[index]=="I"]
      input_list$insert_quotient[temp] <- 1
      
      temp <- index[input_list$role[index]=="R"]
      input_list$insert_quotient[temp] <- 0
      
      temp <- index[input_list$role[index]=="V"]
      input_list$insert_quotient[temp] <- runif(length(temp))
      
    }
    
    # Assign time since last negative HIV test
    index_male <- index[input_list$sex[index] == 'm'] 
    index_female <- index[input_list$sex[index] == 'f']
    index_under25 <- index[input_list$age[index] <= 25]

  }
  
  #initial values of these variables differ between initial population
  #and new additions during model run
  if(type=="births")
  {
    
    # Assign sex (either all "m" or "m" and "f")
    if(dat$param$model_sex=="msm"){
      input_list$sex[index] <- "m"
    } else {
      input_list$sex[index]<- sample(c("m","f"),length(index),prob=c(0.5,0.5),replace=T)
    }
    
    # Assign generic nodal attribute
    if(!is.logical(dat$param$generic_nodal_att_values)){
      input_list$att1[index] <- sample(dat$param$generic_nodal_att_values ,
                          total_new, replace=TRUE,
                          prob=dat$param$generic_nodal_att_values_props_births)
      
      if(!is.logical(dat$param$sti_prob_att)) {
        att_ix <- lapply(1:dat$param$generic_nodal_att_no_categories, function(x) index[which(input_list$att1[index] == dat$param$generic_nodal_att_values[x])])
        
        for(ii in 1:length(att_ix)) {
          input_list$sti_status[(att_ix[[ii]])] <- rbinom(length(att_ix[[ii]]), 1, dat$param$sti_prob_att[ii])
        }
      }
    }
    
    # Assign role
    if(dat$param$model_sex=="msm"){
      input_list$role[index] <- sample(names(dat$param$role_props) ,
                                            total_new,
                                            replace=TRUE,
                                            prob = dat$param$role_props)
      
      temp <- index[input_list$role[index]=="I"]
      input_list$insert_quotient[temp] <- 1
      
      temp <- index[input_list$role[index]=="R"]
      input_list$insert_quotient[temp] <- 0
      
      temp <- index[input_list$role[index]=="V"]
      input_list$insert_quotient[temp] <- runif(length(temp))
      
    }
    
    #ages for new additions -------------------------
    if(dat$param$age_dist_new_adds=="min_age"){
      input_list$age[index] <- dat$param$min_age+round(runif(length(index)),5)
    }else
      if(dat$param$age_dist_new_adds=="mixed"){  
      ages <- rep(NA_real_,length(index))
      probs <- runif(length(index))
      ix <- which(probs <= dat$param$prop_new_agents_min_age)
      if(length(ix)>0){
        ages[ix] <- dat$param$min_age
      }
      ix<-which(probs > dat$param$prop_new_agents_min_age)
      if(length(ix)>0){
        
        ages[ix]<- sample(x=dat$param$min_age:(dat$param$max_age-1),
                          size=length(ix),
                          replace=TRUE,
                          prob=dat$param$male_age_dist)
        ages[ix] <- ages[ix]+round(runif(length(ix)),5)
      }
      input_list$age[index] <- ages
      }else
        if(dat$param$age_dist_new_adds=="linear_decline_18_55"){
          input_list$age[index]<- sample(x=dat$param$min_age:(dat$param$max_age-1),
                                          size=total_new,
                                          replace=TRUE,
                                          prob=seq(50, 10, -10/9)/1110)
          input_list$age[index] <- input_list$age[index]+round(runif(total_new),5)
        }
    
    # end of ages for new additions --------------------------      
    
    # Assign time from last negative HIV test
    index_male <- index[input_list$sex[index] == 'm'] 
    index_female <- index[input_list$sex[index] == 'f'] 
    index_under25 <- index[input_list$age[index] <= 25] 
    
    input_list$arrival_time[index] <- at
    
  }
  
  # Assign AI probability (same function for initial and births, but relies on assignment of sex, so
  # must be at end of function).
  input_list$ai_prob[index] <- 0
  input_list$ai_prob[index_female] <- rbinom(length(index_female), 1, dat$param$prop_AI)
  input_list$ai_prob[index_female] <- assign_ai_prop(dat, input_list$ai_prob[index_female])
  
  #######################################
  return(input_list)
}

