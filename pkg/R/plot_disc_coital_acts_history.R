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
plot_disc_coital_acts_history <- function(model,outpath,sim=1,name="evomodel"){ 
  #note 10/15/15: function in draft form now (it works)
  #but needs to be re-formatted with better variable names
  #and comments
  
  #description:
  # for each agent in model, plots time and partner id for discordant
  #acts when
  #1) agent insertive and susceptible (black circle)
  #2) agent insertive and infective  (red circle)
  #3) agent receptive and infective  (green circle)
  #4) agent receptive and susceptible (blue circle)

 outname=paste(name,"_coital_acts_history.pdf",sep="")
  
pdf(file.path(outpath,outname))
par(mfrow=c(2,2))
aa=model$coital_acts_list[[sim]]
if(is.null(aa)){
  cat("\n coital acts history was not saved; if desired set 'save_coital_acts=TRUE'")
}
nsteps=model$param[[sim]]$n_steps
bb=do.call("rbind",aa)
cc=unique(c(bb$sus_id,bb$inf_id))
for(ii in 1:length(cc))
{
  
  dd=which(bb$sus_id==cc[ii])
  ee=bb$inf_id[dd]
  
  ff=which(bb$inf_id==cc[ii])
  gg=bb$sus_id[ff]

  jj=which(bb$insert_id==cc[ii])
  kk=which(bb$recept_id==cc[ii])
  
  hh=unique(c(ee,gg))
  if(length(hh)>0){
    hh=sort(hh)
    times=sort(bb$timestep[c(dd,ff)])
    xvec= 1:nsteps
    yvec=seq(1,length(hh),length=nsteps)
    par(mgp=c(.5,.25,0),tcl=-.1)
    plot(xvec,yvec,type='n',axes=F,ylab=NA,xlab=NA)
    abline(h=1:(length(hh)+1),lty=2,col="grey")
    axis(1)
    axis(2,at=1:length(hh),labels=hh)
    box()
    mtext(paste("discord coit acts history for agent",cc[ii]),side=3,line=1,cex=1)
    mtext("time step",side=1,line=1.3)
    mtext("partners",side=2,line=2)
    
    #inf and insert
   index=which(bb$inf_id==cc[ii] & bb$insert_id==cc[ii])
   if(length(index)>0){
     points(bb$timestep[index],match(bb$sus_id[index],hh),pch=16,col=1,cex=1)  
   }
   #inf and recept
   index=which(bb$inf_id==cc[ii] & bb$recept_id==cc[ii])
   if(length(index)>0){
     points(bb$timestep[index],match(bb$sus_id[index],hh),pch=16,col=2,cex=1)
   }
   #sus and insert
   index=which(bb$sus_id==cc[ii] & bb$insert_id==cc[ii])
   if(length(index)>0){
     points(bb$timestep[index],match(bb$inf_id[index],hh),pch=16,col=3,cex=1)  
   }
   #sus and recept
   index=which(bb$sus_id==cc[ii] & bb$recept_id==cc[ii])
   if(length(index)>0){
     points(bb$timestep[index],match(bb$inf_id[index],hh),pch=16,col=4,cex=1)  
   }
   legend("bottom",legend=c("inf+insert","inf+recept"),
          pch=16,
          col=1:2,
          xpd=TRUE,inset=c(0,-.36),bty='n',horiz=T,x.intersp=0.4,
          cex=1.2)
   
   legend("bottom",legend=c("sus+insert","sus+recept"),
          pch=16,
          col=3:4,
          xpd=TRUE,inset=c(0,-.45),bty='n',horiz=T,x.intersp=0.4,
          cex=1.2)
   
  }
}
dev.off()
}