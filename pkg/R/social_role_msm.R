#' @title Determine sex role of agent for MSM models.
#'
#'
#' @param dat master data object
#' @param at timestep
#' @return 'dat' object with sex role for agents in MSM models appended to 'social_discord_edgelist_df' table
#' @details
#' Subfunction in 'social_coital_acts_module'. Determines coital acts role for msm, any agents are other than 'versatile';  also determines if 'flipping' occurs per V-V couple, if so, adds row to discord_edgelist_df and is treated as new act.
#' @examples
#' dat <- social_role_msm(dat,at)
#' 
#' @export
social_role_msm <- function(dat,at)
  {

  #------------------------------
  #Description:
  # Determines coital acts role for MSM, for any agents who are versatile
  # also determines if flipping occurs per V-V couple
  # if so, adds row to discord_edgelist_df and is treated as new act

  #inputs: dat$discord_coital_df
  #outputs: dat$discord_coital_df$role: I,V,R for each agent in disc. partnership
  #------------------------------

  if(is.null(dat$discord_edgelist_df) || is.null(dat$discord_coital_df)){
    return(dat)}

  if(dat$param$model_sex!="msm"){return(dat)}

  sus_role <- dat$pop$role[dat$discord_coital_df$agent2]
  inf_role <- dat$pop$role[dat$discord_coital_df$agent1]
  dat$discord_coital_df$sus_role <- sus_role
  dat$discord_coital_df$inf_role <- inf_role

  #temporary qaqc
  #if(any( (sus_role==inf_role) & sus_role!="V")){browser()}
  #was initially getting I-I or R-R couples, want to make sure that's fixed
  #end of temporary qaqc

  temp_vec <- rep(NA_real_,nrow(dat$discord_coital_df))
  dat$discord_coital_df$insert_id <- temp_vec
  dat$discord_coital_df$recept_id <- temp_vec

  if(!is.logical(dat$param$role_props)){
    #skipped if popn all "V"

    #dealing with V-I, I-V couples
    index_1 <- ( sus_role=="I" & inf_role=="V")
    index_2 <- ( sus_role=="V" & inf_role=="I")

    dat$discord_coital_df$insert_id[index_1] <- dat$discord_coital_df$sus_id[index_1]
    dat$discord_coital_df$recept_id[index_1] <- dat$discord_coital_df$inf_id[index_1]

    dat$discord_coital_df$insert_id[index_2] <- dat$discord_coital_df$inf_id[index_2]
    dat$discord_coital_df$recept_id[index_2] <- dat$discord_coital_df$sus_id[index_2]

    #R-V,V-R couples
    index_1 <- ( sus_role=="R" & inf_role=="V")
    index_2 <- ( sus_role=="V" & inf_role=="R")

    dat$discord_coital_df$insert_id[index_1] <- dat$discord_coital_df$inf_id[index_1]
    dat$discord_coital_df$recept_id[index_1] <- dat$discord_coital_df$sus_id[index_1]

    dat$discord_coital_df$insert_id[index_2] <- dat$discord_coital_df$sus_id[index_2]
    dat$discord_coital_df$recept_id[index_2] <- dat$discord_coital_df$inf_id[index_2]

    #I-R, R-I couples
    index_1 <- ( sus_role=="I" & inf_role=="R")
    index_2 <- ( sus_role=="R" & inf_role=="I")

    dat$discord_coital_df$insert_id[index_1] <- dat$discord_coital_df$sus_id[index_1]
    dat$discord_coital_df$recept_id[index_1] <- dat$discord_coital_df$inf_id[index_1]
    #note, below is a bit different from previous pairings (VI,IV,RV,VR)
    dat$discord_coital_df$insert_id[index_2] <- dat$discord_coital_df$inf_id[index_2]
    dat$discord_coital_df$recept_id[index_2] <- dat$discord_coital_df$sus_id[index_2]
  } #end of i-v,r-v couples, skipped if popn all "V"


  #dealing with V-V couples,
  index_vv <- which( sus_role=="V" & inf_role=="V")
  no_vv <- length(index_vv)
  dat$discord_coital_df$iev <- rep(0, nrow(dat$discord_coital_df))
  
  if(no_vv > 0){
    #which couples flip
    dat$discord_coital_df$iev[index_vv] <- rbinom(no_vv,1,dat$param$prob_iev)

    #assign who is insertive,receptive
    sus_ins_quot <- dat$pop$insert_quotient[dat$discord_coital_df$sus_id[index_vv]]
    inf_ins_quot <- dat$pop$insert_quotient[dat$discord_coital_df$inf_id[index_vv]]
    insert_probs <- inf_ins_quot/ (inf_ins_quot + sus_ins_quot)
    insert_inf_index <- index_vv[which(runif(no_vv)< insert_probs)]
    recept_inf_index <- setdiff(index_vv,insert_inf_index )
    #infected, insertive
    dat$discord_coital_df$insert_id[insert_inf_index] <- dat$discord_coital_df$inf_id[ insert_inf_index]
    #suscetpible, receptive
    dat$discord_coital_df$recept_id[insert_inf_index] <- dat$discord_coital_df$sus_id[ insert_inf_index]
    #infected, recpetive
    dat$discord_coital_df$recept_id[recept_inf_index ] <- dat$discord_coital_df$inf_id[ recept_inf_index ]
    #susceptible, insertive
    dat$discord_coital_df$insert_id[recept_inf_index ] <- dat$discord_coital_df$sus_id[ recept_inf_index ]
  }

  ######
  #expand data.frame based on iev and switch roles for iev couples 
  #once satisfied, everything works, can remove new_df and simply replace original df
  if(sum(dat$discord_coital_df$iev) > 0){
    
    expan_vec <- rep(1:nrow(dat$discord_coital_df),
                     times = dat$discord_coital_df$iev +1)
    
    new_df    <- dat$discord_coital_df[expan_vec,,drop=F]
    index     <- which(new_df$iev==1)
    temp_seq  <- index[seq(2,length(index),by=2)]
    new_df$insert_id[temp_seq] <- new_df$recept_id[temp_seq-1]
    new_df$recept_id[temp_seq] <- new_df$insert_id[temp_seq-1]
    dat$discord_coital_df      <- new_df
  }
  ######################################
  #add sex_type for SUSCEPTIBLE
  # 1 = Insertive anal
  #  2 = Receptive anal
  #	3 = Insertive vaginal
  #	4 = Receptive vaginal
  # 5 = Other
  
  dat$discord_coital_df$sus_sex_type <- rep(NA_real_,nrow(dat$discord_coital_df))
  index <- dat$discord_coital_df$sus_id == dat$discord_coital_df$insert_id
  dat$discord_coital_df$sus_sex_type[index] <- 1
  dat$discord_coital_df$sus_sex_type[!index] <- 2
  
  ####################################
  return(dat)
}
