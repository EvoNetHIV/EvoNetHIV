#' @title Calculate number of coital acts per couple
#'
#' @param dat master data object
#' @param at timestep
#' @return 'dat' object with number of coital acts per couple added to 'social_discord_edgelist_df' table
#' @details
#' Takes the data.frame, discord_edgelist_df, returned from social_discord_edgelist_df and calculates the number of acts per partnership for timestep then expands data.frame so each row represents single sex act if couples have more than one act per timestep; eliminates couples with zero acts for timestep.
#' @examples
#' dat <- social_coital_acts(dat,at)

#' @export
social_coital_acts <-function(dat,at)
{
  #######################################################
  #-- takes the data.frame,discord_edgelist_df, returned from social_discord_edgelist_df
  #-- and calculates the number of acts per partnership for timestep
  #-- then expands data.frame so each row represents a single sex act if the
  #-- couples have more than one act for timestep
  #-- eliminates couples with zero acts for timestep
  #-- main output: discord_coital_df
  # inputs: dat$discord_edgelist_df, dat$pop$disclosure_status, dat$param$act_redux_discl,
  # dat$param$mean_sex_acts_day, dat$param$days_per_timestep
  # outputs: dat$discord_coital_df
  #######################################################
  dat$discord_coital_df <- NULL #need this in case sum of acts=0 
  #(small pop size, low # inf)
  #this will cause role_fxn to be skipped
  
  if(is.null(dat$discord_edgelist_df)){return(dat)}
  
  discord_edgelist_df <- dat$discord_edgelist_df
  disc_inf_ix <- which(dat$discord_edgelist_df$infected==1)
  recipient_id <- discord_edgelist_df$sus_id[ disc_inf_ix] 
  infector_id  <- discord_edgelist_df$inf_id[ disc_inf_ix] 
  
  #calculate no acts per day, stop if none 
  reduction_vec  <- rep(1, length(infector_id))
  temp_index     <- which(dat$pop$diag_status[infector_id]==1 & dat$pop$disclosure_status[infector_id]==1)
  #reduce no. of sex acts due to infected partner disclosing status
  if(length(temp_index)>0)
    reduction_vec[temp_index] <- 1.0 -dat$param$act_redux_discl 
  
  #track when sus agent last (knowingly) had sex with hiv+ agent
  dat$pop$time_hiv_sex[recipient_id][temp_index] <- at
  #track when inf agent last  had sex 
  dat$pop$last_disc_sex[infector_id] <- at
  
  
  #browser()
   if(dat$param$prob_sex_by_age){
     
   recip_age_vec <- dat$pop$age[recipient_id]
   inf_age_vec   <- dat$pop$age[infector_id]
   mean_age_vec  <- rowMeans(cbind(recip_age_vec,inf_age_vec))
   #these calculations come from john
   prob_sex <- pmax ( dat$param$prob_sex_age_19 * 
                    (1 - (mean_age_vec - 19) / 
                    (dat$param$max_age_sex - 19) ), 0 )
   mean_no_acts <- prob_sex* reduction_vec
   
   }else{
     mean_no_acts <- dat$param$mean_sex_acts_day * reduction_vec
   }
   
  
  no_acts  <- rpois(length(infector_id),mean_no_acts)

  #need to have 1 act as default to keep nondiscordonant couples in df, otw they'd be removed below
  discord_edgelist_df$no_acts <- rep(NA_real_,nrow(discord_edgelist_df))
  discord_edgelist_df$no_acts[ dat$discord_edgelist_df$infected==1] <- no_acts
  ix_nondisc <- which(dat$discord_edgelist_df$infected==0)
  discord_edgelist_df$no_acts[ix_nondisc] <- rpois(length(ix_nondisc),dat$param$mean_sex_acts_day)
    
  if(sum(no_acts)==0){return(dat)}
  #for counting number of disc act per agent
  acts_by_agent <- table( c( rep( discord_edgelist_df$inf_id[disc_inf_ix], 
                                  times = discord_edgelist_df$no_acts[disc_inf_ix]),
                             rep( discord_edgelist_df$sus_id[ disc_inf_ix],
                                  times = discord_edgelist_df$no_acts[disc_inf_ix]) ))
  acts_by_agent_index <- as.numeric(names(acts_by_agent))
  
  #discordonant total acts
  dat$pop$total_acts[acts_by_agent_index] <-  
    ( dat$pop$total_acts[acts_by_agent_index]+as.numeric(acts_by_agent) ) 
  
  #add couple id
  discord_edgelist_df$couple_id <- 1:nrow(discord_edgelist_df)
  
  #expand edgelist based on number of acts per pair
  #each row of discord_coital_df  represents single act
  df_index <- rep(1:nrow(discord_edgelist_df) , times=   discord_edgelist_df$no_acts)
  df_temp  <- discord_edgelist_df[df_index,,drop=F]
  df_temp  <- as.data.frame(df_temp,row.names = NULL)
  
  #list of no acts per couple
  aa <- split(df_temp$no_acts,df_temp$couple_id)
  
  #list of "act id" per couple
  bb <- lapply(aa,function(x){ bb=length(x);x=1:length(x)})
  df_temp$act_id_couple  <- unlist(bb)
  
  #final output
  dat$discord_coital_df <- df_temp
  
  return(dat)
  
}
