#' @title Determine condom use per coital acts
#'
#'
#' @param dat master data object
#' @param at timestep
#' @return 'dat' object with status of condom use (0/1) added to 'discord_edgelist_df' table
#' @details
#' Subfunction in 'social_coital_acts_module'.Assigns condom use (0/1) to each row (analagous to act) in discord_edgelist_df (table used to calculate transmission probabilities).
#' @examples
#' dat <-social_condom_use(dat,at)

#' @export
social_condom_use <- function(dat,at)
{
  # Assigns condom use (0/1) to each row (analagous to act) in discord_coital_df
  # inputs: param$condom_prob, dat$discord_coital_df
  # output: dat$discord_coital_df$condom
  
  ########################################
  #if no disc. pairs or no sex acts, stop fxn
  if(is.null(dat$discord_coital_df)){return(dat)}
  ###########################################
  
  # Calcuate condom use (0/1) based on "condom_prob"
    condom_prob <- rep(dat$param$condom_prob, nrow(dat$discord_coital_df))
    
    if(dat$param$risk_comp_cond) {
      ix_rc <- which(dat$attr$vaccinated[dat$discord_coital_df$sus_id] == 1 |
                      (dat$attr$vaccinated[dat$discord_coital_df$inf_id] == 1 & 
                         is.na(dat$attr$diag_status[dat$discord_coital_df$inf_id])) )
      if(length(ix_rc)>0){
        condom_prob[ix_rc] <- dat$param$condom_prob * dat$param$risk_comp_cond_rr
       }
                   
    }
  
    #update condom prob vec based on relationship durations (Sorry, James for changing index names -- I am  struggling to understand this code.)
if(dat$param$condom_use_rel_dur){
  dat <- update_compact_el_and_rel_durs(dat,at)
  p1 <- floor(dat$compact_el) # agent ID for agents listed in the first column of the edgelist (partner 1)
  p2 <- round(dat$compact_el%%floor(dat$compact_el)*dat$param$compact_el_divisor) # agent ID for agents in second column of edgelist (partner 2)

 #p1i: index of locations within the edgelist where partner1 (1st col of fel) is infected and partner2 (2nd col. of fel) is susceptible]
  p1i <- which(p1 %in% dat$discord_coital_df$inf_id & p2 %in% dat$discord_coital_df$sus_id)
  if(length(p1i)>0){
    #p1i_dc: index of locations in "discord_coital_df" in which partner 1 is infected (note: length(p1i_dc) >= length(p1i) b/c model allows >1 act/day)
    p1i_dc <- which(dat$discord_coital_df$inf_id %in% p1[p1i] & dat$discord_coital_df$sus_id %in% p2[p1i])
    condom_prob[p1i_dc] <- condom_prob[p1i_dc]*(5*365/(5*365+dat$partnership_durs[p1i]))
  }
  
 #p2i: index of locations within the edgelist where partner2 (2nd col of fel) is infected and partner1 (1st col. of fel) is susceptible]
  p2i <- which( p2 %in% dat$discord_coital_df$inf_id  & p1 %in%dat$discord_coital_df$sus_id )
  if(length(p2i)>0){
    #p2i_dc: index of locations in "discord_coital_df" in which partner 2 is infected (note: length(p2i_dc) >= length(p2i) b/c model allows >1 act/day)
    p2i_dc <- which(dat$discord_coital_df$inf_id %in% p2[p2i] & dat$discord_coital_df$sus_id %in% p1[p2i])
    condom_prob[p2i_dc] <- condom_prob[p2i_dc]*(5*365/(5*365+dat$partnership_durs[p2i]))
  }
  condom_user_modifier_non_users <- (dat$attr$condom_user[dat$discord_coital_df$sus_id] + 
                                    dat$attr$condom_user[dat$discord_coital_df$inf_id])/2
  condom_prob <- condom_prob*condom_user_modifier_non_users
                          
}

#update condom prob vec based on the average age of the partners
# idea is that condom usage will decline with age from the age 16 (or age 18) baseline given by dat$param$condom_prob
if(dat$param$condom_use_age){
  
    average_age <- (dat$attr$age[dat$discord_coital_df$sus_id] + dat$attr$age[dat$discord_coital_df$inf_id])/2
    
    # For age_condom_use_halves = 50, average age = 16, min_age = 16, modifier below will be (50-16)/(50+16-2*16) = (50-16)/(50-16+16-16) = 1.0
    # For age_condom_use_halves = 50, average age = 50, min_age = 16, modifier below will be (50-16)/(50+50-2*16) = (50-16)/(50-16+50-16) = 0.5
    condom_user_modifier_age <-  (dat$param$age_condom_use_halves - dat$param$min_age)/( dat$param$age_condom_use_halves + average_age - 2*dat$param$min_age)
    
    condom_prob <- condom_prob*condom_user_modifier_age
    
}

  #fill in "condom" column in table "discord_coital_df" (0s or 1s)
  dat$discord_coital_df$condom <- rbinom(n = nrow(dat$discord_coital_df),
                                         size = 1,
                                         prob = condom_prob)
    
  #update condom_prob based on agent level condom use probability
  if(dat$param$individual_condom_prob){
    condom_use_vec <- c(dat$attr$individual_condom_prob[dat$discord_coital_df$agent1], dat$attr$individual_condom_prob[dat$discord_coital_df$agent2])
    condom_use_matrix <- matrix(condom_use_vec,ncol=2,byrow = F)
    if(dat$param$individual_condom_prob_compromise_method == "mean"){ 
      condom_use_prob <- rowMeans(condom_use_matrix,na.rm=T)
      condom_use <- list()
      condom_use <- sapply(1:length(condom_use_prob), function(x) rbinom(n = 1,size = 1, prob = condom_use_prob[[x]]))
      condom_use[condom_use < 0] = 0
      condom_use[condom_use > 1] = 1
      dat$discord_coital_df$condom <- condom_use}
    if(dat$param$individual_condom_prob_compromise_method == "max"){ 
      condom_use_prob <- apply(condom_use_matrix, 1, max)
      condom_use <- list()
      condom_use <- sapply(1:length(condom_use_prob), function(x) rbinom(n = 1,size = 1, prob = condom_use_prob[[x]]))
      condom_use[condom_use < 0] = 0
      condom_use[condom_use > 1] = 1
      dat$discord_coital_df$condom <- condom_use}
    if(dat$param$individual_condom_prob_compromise_method == "min"){ 
      condom_use_prob <- apply(condom_use_matrix, 1, min)
      condom_use <- list()
      condom_use <- sapply(1:length(condom_use_prob), function(x) rbinom(n = 1,size = 1, prob = condom_use_prob[[x]]))
      condom_use[condom_use < 0] = 0
      condom_use[condom_use > 1] = 1
      dat$discord_coital_df$condom <- condom_use}
  }  
  # if(max_condom_use_prob< 0) max_condom_use_prob <- 0
  
    if(dat$param$condom_prob_change == T) {
    dat$param$condom_prob <- ((dat$param$condom_prob_max * (at^dat$param$condom_prob_pow)) /
                             ((dat$param$condom_prob_inflect^dat$param$condom_prob_pow) + (at^dat$param$condom_prob_pow))) + 0.04
  }
  
  return(dat)
  
}
