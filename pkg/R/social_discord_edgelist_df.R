#' @title Organize data for discordonant couples.
#'
#'
#' @param dat master data object
#' @param at timestep
#' @return 'dat' object with 'discord_edgelist_df' table appended.
#' @details
#' Subfunction in 'social_coital_acts_module'. Creates table of discordonant couples with their relevant attributes (e.g., sex, infection status, role): "discord_edgelist_df". This dataframe is main input into transmission functions. Couples with the infected agent past the specified threshold time in AIDS are not included (assumes no sex for these couples). Raw table of discordonant couple IDs is created with call to social_discord_edgelist, then attributes are appended to table to create a data.frame object.
#' @examples
#' dat <- social_discord_edgelist_df(dat,at)

#' @export
social_discord_edgelist_df <- function(dat,at)
{
  #######################################################
  #-- takes couples returned from social_discord_edgelist()
  #-- and creates a data.frame discord_edgelist_df that
  #-- converts network ID of individual to EvoNet ID and
  #-- lists who is infected, suscetpible,  sex of each, 
  #input: discord_edgelist matrix
  #ouput: data frame "discord_edgelist_df" with columns,
  #       sus_id,inf_id,sus_sex, inf_sex,sus_sex_type,insert_id,recept_id
  ########################################################
  
  #clear values#######
  dat$discord_edgelist_df <- NULL
  
  #leav fxn if no susceptibles and infecteds ##########################
  inf_stop <-  all(dat$attr$status_evo[dat$attr$active == 1] == 1)
  sus_stop <-  all(dat$attr$status_evo[dat$attr$active == 1] == 0 )
  
  if(inf_stop || sus_stop){return(dat) }
  
  temp_status_vec <- dat$attr$status_evo
  # extract the edgelist from either the nw or attached existing el
  if(!is.null(dat[['nw']])){
    el <- get.dyads.active(dat$nw, at = at)
    # uncomment enforce sort and column order for consistency between fast.edgelist and normal modes
    #el <- as.edgelist(el,n<-network.size(nw),directed=is.directed(nw),bipartite=is.bipartite(nw),loops=has.loops(nw))
  } else {
    el <- dat$el[[1]]
    # uncomment enforce sort and column order for consistency between fast.edgelist and normal modes
    # el <- as.edgelist(el,n=attr(el,'n'),directed=FALSE)
  }
  discord_edgelist_df <- social_discord_edgelist(el, 
                                                 status_vec = temp_status_vec,at)
  
  #leave fxn if no disc. pairs
  if(is.null(discord_edgelist_df)){return(dat)}
  
  #discord_edgelist_df returns indices for position on network and 
  #these need to be translated to evonet ids (position in entire population (live and dead))
  sus_id <- dat$attr$id[discord_edgelist_df$sus_id]
  inf_id  <- dat$attr$id[discord_edgelist_df$inf_id]
  discord_edgelist_df$sus_id <- sus_id
  discord_edgelist_df$inf_id <- inf_id 
  discord_edgelist_df$agent1 <- dat$attr$id[discord_edgelist_df$agent1]
  discord_edgelist_df$agent2 <- dat$attr$id[discord_edgelist_df$agent2]
  
  
  
  #identify agents in aids, then remove from discord_edgelist_df if present
  if(dat$param$aids_death_model=="Gamma_Death"){
    aids_ids <- which(
      ((at - dat$pop$Time_Inf ) >  ((dat$pop$RandomTimeToAIDS + dat$param$time_in_aids)*
        dat$param$aids_sex_cutoff_prop)) & 
        dat$pop$Status == 1 & dat$pop$treated!=1)
  }else{index_aids <- which(dat$pop$Status==1 & dat$pop$CD4>=4)
  if(length(index_aids)>0){
    
    index_time_in_aids_table <- cbind(dat$pop$spvl_cat[index_aids],rep(4,length(index_aids)))
    time_in_aids_table <- dat$param$CD4_lookup[index_time_in_aids_table]*365
    time_in_aids_agent <- dat$pop$start_aids_cd4[index_aids]
    aids_ids <- index_aids[(at-time_in_aids_agent) >= (time_in_aids_table*dat$param$aids_sex_cutoff_prop)]
  }else{
      aids_ids<-NULL
     }
    }
  
  #if present, remove agents in aids past time threshold for cessation
  #of coital acts from discord_edgelist_df
  #if only one pair of agents and they are removed, leave function
  if(length(aids_ids)>0){
     aids_in_del <- which(discord_edgelist_df$inf_id %in% aids_ids)
     if(length(aids_in_del)>0 & length(aids_in_del)<nrow(discord_edgelist_df)){
       discord_edgelist_df <- discord_edgelist_df[-aids_in_del,]
     }
     if(length(aids_in_del)==nrow(discord_edgelist_df)){
       return(dat)
     }
  }
  
    
  #add sex
  if(dat$param$model_sex=="msm"){
    
    discord_edgelist_df$sus_sex <- rep("m",nrow(discord_edgelist_df))
    discord_edgelist_df$inf_sex <- rep("m",nrow(discord_edgelist_df))
    
  }else{
    #hetero section
    discord_edgelist_df$sus_sex <- dat$pop$sex[discord_edgelist_df$sus_id]
    discord_edgelist_df$inf_sex <- dat$pop$sex[discord_edgelist_df$inf_id]
    
    #add insert_id and recept_id as placeholder (not needed for hetero dynamics)
    #note: for msm, these set in separate fxn "social_role_msm"
    discord_edgelist_df$insert_id <- rep(NA_real_, nrow(discord_edgelist_df))
    discord_edgelist_df$recept_id <- rep(NA_real_, nrow(discord_edgelist_df))
      }
  dat$discord_edgelist_df <- discord_edgelist_df
  return(dat)
}
