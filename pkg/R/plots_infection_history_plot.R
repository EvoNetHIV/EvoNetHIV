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
infection_history_plot<- function(ind,pop,params,nw){
  
ind_inf_time   <- pop$Time_Inf[ind]
ind_inf_donor  <- pop$Donors_Index[ind]
ind_time_death <- pop$Time_Death[ind]

founder <- ifelse(ind_inf_time<=0,TRUE,FALSE)


dframe_temp  = networkDynamic::get.edge.activity(nw, as.spellList = TRUE, active.default = F)

dframe = dframe_temp[which(dframe_temp$head == ind | dframe_temp$tail==ind),]
if(nrow(dframe)==0){return(invisible(NULL))}

temp_vec = NULL
for(i in 1:nrow(dframe))
{
  temp=c(dframe$tail[i],dframe$head[i])  
  temp_vec[i] <- temp[which(temp!=ind)]
}

dframe$agent     <-  rep(ind,nrow(dframe))
dframe$partner   <-  temp_vec
begin        <-  dframe$onset
begin[is.infinite(begin)] = 1
ending       <-  dframe$terminus
ending[is.infinite(ending)]= params$n_steps
dframe$begin     <-  begin
dframe$ending    <-  ending
dframe$Time_Inf  <-  pop$Time_Inf[dframe$partner]

dframe$partner_donor     <- rep(NA,nrow(dframe))    
#--------------------------------------
#which partners were infected during agents existence
temp_ix <- which(pop$Time_Inf[dframe$partner] >= dframe$begin &
                   pop$Time_Inf[dframe$partner] <= dframe$ending )  
dframe$partner_donor[temp_ix]     <- pop$Donors_Index[dframe$partner[temp_ix]]    
#----------------------------------------
dframe$total_sex <- rep(NA,nrow(dframe))
dframe$total_insertive      <-  rep(NA,nrow(dframe))
dframe$total_receptive      <-  rep(NA,nrow(dframe))
dframe$"agent_inf_partner?" <-  rep(NA,nrow(dframe))
dframe$"agent_inf_partner_time" <-  rep(NA,nrow(dframe))
dframe$"partner_inf_agent?" <-  rep(NA,nrow(dframe))
dframe$"partner_inf_agent_time" <-  rep(NA,nrow(dframe))
dframe$duration                 <-  rep(NA,nrow(dframe))
dframe$agent_status             <-    rep(NA,nrow(dframe))
dframe$partner_status             <-  rep(NA,nrow(dframe))
dframe$disc_dur  <-  rep(NA,nrow(dframe))
partners  <- sort(unique(dframe$partner))

#################################################
#combined graphs
#par(mfrow=c(2,1),mar=c(1,3,1,3),oma=c(7,3,5,3))
par(mfrow=c(2,1),oma=c(4,3,3,3))
#yaxis ticks = number of partners + 1 (for agent)

yax_vec <- seq(0,length(partners))
plot(seq(1,params$n_steps,length=10), seq(0,length(partners+1),length=10),
     type ='n', axes =F, ylab ="", xlab ="'",bty="o")
axis(1, mgp =c(.5,.25,0), tcl =-.2)
axis(2, at = yax_vec, labels =c(ind,partners), las =2)
axis(4, at = yax_vec, labels =c(ind,partners), las =2)
box("plot")
abline(h= yax_vec,col="grey",lty=2)

#if agents doesn't die, final timestep is final timestep of model
if(is.na(ind_time_death)){xvalue =params$n_steps }else{xvalue=ind_time_death}

#arrival time: if NA, then 1 (original popn),else arrival time = arrival time (eg, time of birth)
if(is.na(pop$arrival_time[ind])){arrival=1}else{arrival=pop$arrival_time[ind]}

#if founder, infected whole time
if(founder){
  lines(arrival:xvalue,rep(0,length(arrival:xvalue)), col=2,lwd=3)
}


#if original pop but never infected
if(is.na(ind_inf_time)){
  lines(arrival:xvalue,rep(0,length(arrival:xvalue)), col=1,lwd=3)
}

#if not founder but infected
#plot time not infected, and time infected until death/end of model
if(ind_inf_time>0)
{
  lines(arrival:ind_inf_time, rep(0,length(arrival:ind_inf_time)), col=1,lwd=3 )
  lines(ind_inf_time:xvalue, rep(0,length(ind_inf_time:xvalue)), col=2,lwd=3 )
  points(ind_inf_time,0,col=2,pch=1,cex=2)
  match_donor <- match(ind_inf_donor,partners)
  points(ind_inf_time,match_donor,col="blue",pch=16,cex=2)
  
}      


no_partner_vec <- rep(0,params$n_steps)
for(i in 1:nrow(dframe))
{
  
  temp_match        <- match(dframe$partner[i],partners)
  temp_match_length <- length(temp_match)
  
  aa=try(timevec <- dframe$begin[i]:dframe$end[i])
  if(class(aa)=="try-error"){browser()}
  
  no_partner_vec[timevec] <- no_partner_vec[timevec] +1
  yvalues <- temp_match
  lines(timevec,rep(yvalues,length(timevec)),lwd=3,col=1)
  if(!is.na(dframe$Time_Inf[i]))
  {
    if(dframe$Time_Inf[i]< dframe$begin[i])
    {
      lines(timevec,rep(yvalues,length(timevec)),lwd=3,col=2)
    }
    if(dframe$Time_Inf[i] >=  dframe$begin[i] &&  dframe$Time_Inf[i]<dframe$end[i])
    {
      timevec2=dframe$Time_Inf[i]:dframe$end[i]
      lines(timevec2,rep(yvalues,length(timevec2)),lwd=3,col=2)
    }
  }
  
  if(dframe$partner_donor[i]==ind && !is.na(dframe$partner_donor[i]))
  {
    points(dframe$Time_Inf[i],yvalues,pch=16,cex=2,col=2)
  }
  if(dframe$partner_donor[i]!=ind && !is.na(dframe$partner_donor[i]))
  {
    points(dframe$Time_Inf[i],yvalues,pch=2,cex=2,col="blue")
  }
}

if(founder){charvec="(founder)"}else{charvec=""}
mtext(paste("Partner/Infection dynamics for agent:",ind,charvec),
      cex=2,side=3,line=1,outer=T)
mtext("partner ID", side=2,line=0,outer=T,adj=.8,cex=1.4)

mtext("timesteps", side=1,line=1,outer=T,cex=1.4)
legend("bottom",legend=c("Infected","Susceptible") ,col=c("red","black"),
       lty=c(1,1), horiz=T,lwd=c(2,2),
       xpd=TRUE,inset=c(0,-0.30),bty='n')

legend("bottom",legend=c(paste("Inf.Time (by agent",ind,")"),"Inf.Time (by other)",
                         "Inf.Time of agent","Inf Time of agent (infector)"),pch=c(16,2,1,16),
       col=c("red","blue","red","blue"),
       xpd=TRUE,inset=c(0,-.40),bty='n',horiz=T)


#no partners graph
ymax=max(no_partner_vec)
plot(1:params$n_steps,no_partner_vec,axes=F,ylab=NA,xlab=NA,type='l',col="blue",
     ylim=c(0,ymax),lwd=2)
box("plot")
abline(h=0:max(no_partner_vec),col="grey",lty=2)
lines(1:params$n_steps,no_partner_vec,col="blue",lwd=2)
axis(4,at=0:max(no_partner_vec),labels=0:max(no_partner_vec),las=2)
axis(2,at=0:max(no_partner_vec),labels=0:max(no_partner_vec),las=2)
axis(1)

mtext("no. partners", side=2,line=3,outer=F,adj=.5,cex=1.4)

invisible(list(dframe=dframe,ind_inf_time=ind_inf_time,ind_inf_donor=ind_inf_donor,
               partners=partners))

}#end of plot fxn