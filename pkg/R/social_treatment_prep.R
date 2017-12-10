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
social_treatement_prep<-function(dat,at){
  
  #partner list code doesn't work on first timestep
  if(at<3){return(dat)}
  
   #convert fast edgelist indices to evonet indices
   aa<- dat$el[[1]]
   col1=dat$attr$id[aa[,1]]
   col2=dat$attr$id[aa[,2]]
   
   #---------------------------------------------
   # keep track of duration of relationship with positive partner
   #for hiv- agents
   
   status1= dat$pop$Status[col1]
   status2= dat$pop$Status[col2]
   diag1 <- dat$pop$diag_status[col1]
   diag2 <- dat$pop$diag_status[col2]
   
   ix1=which(status1==1 & status2==0 )
   ix2=which(status1==0 & status2==1 )
   ix3=which(status1==0 & status2==0 )
   
   #discordonant couples
   dat$pop$pos_partner_duration[col1[ix2]] <- dat$pop$pos_partner_duration[col1[ix2]]+1
   dat$pop$pos_partner_duration[col2[ix1]] <- dat$pop$pos_partner_duration[col2[ix1]]+1
   
   #both partners hiv-
   ix3b <- which(!is.element(ix3, c(ix1,ix2)))
   dat$pop$pos_partner_duration[ix3[ix3b]] <- 0
   
   #isolates (not in partnership)
   ix4=1:length(dat$pop$Status)
   ix5=which(!is.element(ix4,unique(c(col1,col2))))
   dat$pop$pos_partner_duration[ix5] <- 0
   
   #end of tracking duration of relationship with positive partner for hiv- agents
   #---------------------------------------------
   
   #---------------------------------------------
   #keep track of number of partners for given time period
   partnerlist <- summary_partner_list(dat)
   length(dat$pop$Status)
   partner_thresh <- at-dat$param$no_past_partners_time_prep
   
   new_part_list<- lapply(1:length(partnerlist),
                          function(x){ length(which(partnerlist[[x]]>partner_thresh))})
   new_part_vec <- unlist(new_part_list)
   #qaqc here
   if(length(new_part_vec)!=length(dat$pop$Status)){browser()}
   
   dat$pop$no_partners_past_prep <- new_part_vec
   #end of tracking number of past partners given value of dat$param$no_past_partners_time_prep
   #---------------------------------------------
   
   
   #---------------------------------------------
   #keep track of current number of partners
   parnter_count_now <- table(c(col1,col2))
   partner_count_vec <- rep(0, length(dat$pop$Status))
   partner_count_vec[as.numeric(names(parnter_count_now))] <- as.numeric(parnter_count_now)
   dat$pop$no_partners_now_prep <- partner_count_vec
   #end of keeping track of current number of partners
   #---------------------------------------------
   
   #---------------------------------------------
   #track whether at least one diagnosed hiv+ partner
   d1=which(status1==0 & status2==1 & diag2==1)
   d2=which(status1==1 & status2==0 & diag1==1)
   dat$pop$have_diag_partner[col1[d1]] <- 1
   dat$pop$have_diag_partner[col2[d2]] <- 1
   #end of tracking whether at least one diagnosed hiv+ partner
   #---------------------------------------------
   
   prep_ix <-  which(dat$pop$pos_partner_duration>= dat$param$min_pos_partner_duration &
         dat$pop$no_partners_past_prep >= dat$param$min_past_partners_prep &
         dat$pop$no_partners_now_prep >= dat$param$min_current_partners_prep &
         dat$pop$have_diag_partner == 1)
   
   dat$pop$on_prep[prep_ix] <- 1
   dat$pop$prep_decrease[prep_ix]<- sample(c(0.0,0.31,0.81,0.95),
                                           length(prep_ix),
                                           replace=T,
                                           prob=c(0.21,0.07,0.10,0.62))
   #if(at<900){return(dat)}else{browser()}
   return(dat)
}