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
#removed export as it is not being called now...maybe later
coital_acts_table_fxn <- function(model,dframe,pop,ind_inf_time,ind_inf_donor,ind,
                                  partners){
  par(mfrow=c(1,1))
  
caltemp=(model$coital_acts_list[[1]])
cal=do.call("rbind",caltemp)

for(ii in 1:nrow(dframe))
{
  
  ix <- which( ((cal$sus_id==ind & cal$inf_id==dframe$partner[ii]) |
                  (cal$sus_id==dframe$partner[ii] &  cal$inf_id==ind)) &
                 cal$timestep >= dframe$begin[ii] &
                 cal$timestep <= dframe$ending[ii])  
  
  if(length(ix)>0)
  {
    aa <- cal[ix,]
    range(aa$timestep,na.rm=T)
    no_insertive <- length(which(aa$insert_id == ind))
    no_recept    <- length(which(aa$recept_id == ind))
    total_sex    <- no_insertive+no_recept
  }else{
    total_sex    <- 0
    no_insertive <- NA
    no_recept    <- NA }
  
  dframe$total_sex[ii]       <- total_sex
  dframe$total_insertive[ii] <- no_insertive
  dframe$total_receptive[ii] <- no_recept
  
  dframe$duration[ii]        <- length(dframe$begin[ii]:dframe$ending[ii])
  #is agent pos,neg,both during partnership
  if(!is.na(pop$V[ind]))
  {
    if(pop$Time_Inf[ind] < dframe$begin[ii]  ) 
    {
      dframe$agent_status[ii]="pos"
    }
    if(pop$Time_Inf[ind] >= dframe$begin[ii] & pop$Time_Inf[ind]<=dframe$ending[ii] ) 
    {
      dframe$agent_status[ii]="both"        
    }
    if(pop$Time_Inf[ind] > dframe$ending[ii] ) 
    {
      dframe$agent_status[ii]="neg"        
    }    
  }else{
    dframe$agent_status[ii]="neg"
  }
  
  #is partner pos,neg,both during partnership
  ixp = dframe$partner[ii]  
  
  if(!is.na(pop$V[ixp]))
  {      
    
    if(is.na(pop$Time_Inf[ixp])){browser()}
    
    if(pop$Time_Inf[ixp] < dframe$begin[ii])
    {
      dframe$partner_status[ii]="pos"
    }
    
    if(pop$Time_Inf[ixp] >= dframe$begin[ii] & pop$Time_Inf[ixp]<=dframe$ending[ii] ) 
    {
      dframe$partner_status[ii]="both"        
    }
    if(pop$Time_Inf[ixp] > dframe$ending[ii] ) 
    {
      dframe$partner_status[ii]="neg"        
    }    
  }else{
    dframe$partner_status[ii]="neg"
  }
  
  
  
  if( is.na(dframe$partner_donor[ii]) | ind!=dframe$partner_donor[ii]){
    dframe$"agent_inf_partner?"[ii]=FALSE}
  if(!is.na(dframe$partner_donor[ii])){if( dframe$partner_donor[ii]==ind){
    dframe$"agent_inf_partner?"[ii]=TRUE
  }}
  
  
  if(dframe$"agent_inf_partner?"[ii]==TRUE ){
    dframe$"agent_inf_partner_time"[ii]= dframe$Time_Inf[ii]
  }
  
  if(!is.na(ind_inf_time))
  {
    if(!ind_inf_time<0)
    {
      if(ind_inf_donor==dframe$partner[ii] &&
           pop$Time_Inf[ind]>= dframe$begin[ii] &&
           pop$Time_Inf[ind]<= dframe$ending[ii])
      {
        dframe$"partner_inf_agent?"[ii]=TRUE
      }else{
        dframe$"partner_inf_agent?"[ii]=FALSE
      }
    }else{dframe$"partner_inf_agent?"[ii]=FALSE}
  }
  
  
  if(dframe$"partner_inf_agent?"[ii]==FALSE){dframe$partner_inf_agent_time[ii]=NA}
  if(dframe$"partner_inf_agent?"[ii]==TRUE){dframe$partner_inf_agent_time[ii]=ind_inf_time}
  
  #print(paste(ind, ii, dframe$agent_status[ii], dframe$partner_status[ii]))
  
  if( (dframe$agent_status[ii]=="neg" && dframe$partner_status[ii]=="pos") ||
        (dframe$agent_status[ii]=="pos" && dframe$partner_status[ii]=="neg") )
  {
    dframe$disc_dur[ii]= dframe$duration[ii]
  }
  
  if( (dframe$agent_status[ii]=="neg" && dframe$partner_status[ii]=="neg") ||
        (dframe$agent_status[ii]=="pos" && dframe$partner_status[ii]=="pos") )
  {
    dframe$disc_dur[ii]= 0
  }
  
  if( dframe$agent_status[ii]=="both" && dframe$partner_status[ii]=="pos")
  {
    dframe$disc_dur[ii]= pop$Time_Inf[ind] -  dframe$begin[ii]
  }
  
  if( dframe$agent_status[ii]=="both" && dframe$partner_status[ii]=="neg")
  {  
    dframe$disc_dur[ii]= dframe$ending[ii] - pop$Time_Inf[ind] 
  }
  
  if( dframe$agent_status[ii]=="pos" && dframe$partner_status[ii]=="both")
  {
    dframe$disc_dur[ii]= pop$Time_Inf[dframe$partner[ii]] -  dframe$begin[ii]
  }
  
  if( dframe$agent_status[ii]=="neg" && dframe$partner_status[ii]=="both")
  {
    dframe$disc_dur[ii]= dframe$ending[ii] - pop$Time_Inf[dframe$partner[ii]] 
  }
  
  if( dframe$agent_status[ii]=="both" && dframe$partner_status[ii]=="both")
  {
    dframe$disc_dur[ii]= abs(pop$Time_Inf[ind] - pop$Time_Inf[dframe$partner[ii]]) 
  }
  
}# end of ii loop
#######################    

dframe_final<-data.frame( agent   = rep(ind,nrow(dframe)),
                          partner = dframe$partner,
                          start   = dframe$begin,
                          end     = dframe$ending,
                          duration = dframe$duration,
                          agent_status =dframe$agent_status,
                          partner_status = dframe$partner_status,
                          disc_dur  =  dframe$disc_dur,
                          total_acts = dframe$total_sex,
                          insertive_acts = dframe$total_insertive,
                          receptive_acts = dframe$total_receptive,
                          "partner_inf_agent?" = dframe$"partner_inf_agent?",
                          partner_inf_agent_time = dframe$partner_inf_agent_time,
                          "agent_inf_partner?" = dframe$"agent_inf_partner?",
                          partner_inf_time = dframe$agent_inf_partner_time)

names(dframe_final)=c("agent", "partner", "start", "end", "durtn","Stat","partStat", 
                      "DscDur", "DscActs", "ins", 
                      "rec", "infected?", "time", 
                      "infector?", "time")

    plot(1:10,1:10,type='n',axes=F,ylab=NA,xlab=NA)
    plotrix::addtable2plot(1,2,dframe_final,cex=1,yjust=1,ypad=.4,hlines=T,vlines=T,xpad=.2)
    title(paste("partnership data for agent",ind),adj=.1)

}