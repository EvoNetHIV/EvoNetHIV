#' @title Organize coital acts data for calculation of transmission probabilities
#'
#'
#' @param dat master data object
#' @param at timestep
#' @return 'dat' object with updated coital acts tables for transmission module.
#' @details
#' Wrapper function that calls: social_discord_edgelist_df, social_coital_acts, social_role_msm, social_act_type_het, social_condom_use. These functions identify discordonant relationships, organize relevant behavioral attributes, and calcuate number of sex acts per partnership per timestep. 
#' @examples
#' dat <- social_coital_acts_module(dat,at)

#' @export
social_coital_acts_module <- function(dat,at)
{  
  #Description:
  # Calls in following order: social_discord_edgelist_df, 
  # social_coital_acts, social_role_msm, social_condom_use

 # browser()  
  dat <- social_discord_edgelist_df(dat,at)
  dat <- social_coital_acts(dat,at)
  if(dat$param$model_sex == "msm"){
    dat <- social_role_msm(dat,at)
  }
  if(dat$param$model_sex == "hetero") {
    dat <- social_act_type_het(dat, at)
  }
  dat <- social_condom_use(dat,at)
  
  #
  dat$discord_edgelist_df <- dat$discord_coital_df
  inf_ix <- which(dat$discord_coital_df$infected==1)
  if(length(inf_ix)>0){
  dat$discord_coital_df <- dat$discord_coital_df[which(dat$discord_coital_df$infected==1),]
  }else{dat$discord_coital_df<-NULL}
  
  return(dat)
}
