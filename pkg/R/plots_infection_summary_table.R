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
#not exported as it is not in use now...maybe later
infection_summary_table_fxn <- function(pop,partners,dframe,ind)
{
  par(mfrow=c(1,1))
  
# #next table:
# #1) time of arrival
# #2) time of death
# #3) founder?
# #4) time of infection
# #5) number of partnerships
# #5b) number of unique partners
# #6) number of disc. partnershps
# #7) number of disc acts
# #8) number of infectees
# 
if(is.na(pop$arrival_time[ind]))
{
  arrival_time=1
}else{
  arrival_time = pop$arrival_time[ind]    
}

if(is.na(pop$Time_Death[ind]))
{
  time_death = NA
}else{
  time_death = pop$Time_Death[ind]    
}

if(is.na(pop$Time_Inf[ind]))
{
  time_inf = NA
  founder = FALSE
}else{
  time_inf = pop$Time_Inf[ind]    
  founder = ifelse(time_inf<=0, TRUE,FALSE)
}


total_partners  = nrow(dframe)
unique_partners = length(partners)

no_disc_partnerships = length(which(dframe$disc_dur!=0))
no_disc_acts = sum(dframe$total_sex)

no_infectees = length(which(dframe$"agent_inf_partner?"))



summary_table=data.frame(arrival=arrival_time,
                         death = time_death,
                         founder = founder,
                         inf_time= time_inf,
                         totPart = total_partners,
                         unqPart = unique_partners,
                         discPart = no_disc_partnerships,
                         discActs = no_disc_acts,
                         infectees = no_infectees)

     plot(1:10,1:10,type='n',axes=F,ylab=NA,xlab=NA)
     plotrix::addtable2plot(1,10,summary_table,cex=1,yjust=1,ypad=.4,hlines=T,vlines=T,xpad=.2)
     title(paste("summary data for agent",ind),adj=.1)
     invisible(NULL)
}
