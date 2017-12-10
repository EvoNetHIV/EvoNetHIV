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
plot_vl_trajectories_aim3<- function(model,sim,Therapy_Type,outpath,name)
{
  
  if(is.null(model$vl_list[[sim]])){
    cat("\n viral load list not saved; set evoparams$save_vl_list=TRUE to plot VL trajectories")
    return(invisible(NULL))
  }
  #cat("toggle breakpoint here\n")
  vl_df <- do.call(rbind,model$vl_list[[sim]])
  
  name=paste(name,"_vl_traj.pdf",sep="")    
  
  pdf(file.path(outpath,name),width=12,height=10)
  
  par(mfrow=c(2,2))
  
  agents <- unique(vl_df[,1]) 
  
  for(ind in agents)
  {  
    vl_ix  <- which(vl_df[,1]==ind)
    if(length(vl_ix>0)){
      seq1<- seq(1,model$param[[sim]]$n_steps,length=10)
      seq2<- seq(-5,8,length=10)
      
      #---------------------------------
      
      plot(seq1,seq2,type='n', xlab="Time (Days)",ylab="Log Viral Load")
      lines(vl_df[vl_ix,5],log10(vl_df[vl_ix,6]),lwd=2,col="yellow")
      lines(vl_df[vl_ix,5],log10(vl_df[vl_ix,2]),lwd=2,col="blue")
      lines(vl_df[vl_ix,5],log10(vl_df[vl_ix,7]),lwd=2,col="orange")
      lines(vl_df[vl_ix,5],log10(vl_df[vl_ix,8]),lwd=2,col="red")
      lines(vl_df[vl_ix,5],log10(vl_df[vl_ix,9]),lwd=2,col="purple")
      lines(vl_df[vl_ix,5],log10(vl_df[vl_ix,10]),lwd=2,col="black")
      lines(vl_df[vl_ix,5],log10(vl_df[vl_ix,11]),lwd=4,col="black")
      
      axis(2,col="blue")
      par(new=T)
      plot(seq1,seq(0,6,length=length(seq1)),type='n', xlab=NA,ylab=NA,axes=F)
      lines(vl_df[vl_ix,5],(vl_df[vl_ix,3]),lwd=2,col="green")
      lines(vl_df[vl_ix,5],(vl_df[vl_ix,4]),lwd=2,col="darkgreen")
      axis(4,col="green")
      mtext(paste("VL (Blue) & CD4 (Green) for Agent",ind,"\n(Yel, Gld, Red, Pur, Blk: 0 to 4 muts)"),side=3)
      
      #---------------------------------
      #Infected cell sub-populations with a patient
      seq1<- seq(1,model$param[[sim]]$n_steps,length=10)
      seq2<- seq(-5,8,length=10)
      plot(seq1,seq2,type='n', xlab="Time (Days)",ylab="viral load")
      lines(vl_df[vl_ix,5],log10(vl_df[vl_ix,2]),lwd=2,col="black")
      lines(vl_df[vl_ix,5],log10(vl_df[vl_ix,15]),lwd=2,col="gold")
      lines(vl_df[vl_ix,5],log10(vl_df[vl_ix,16]),lwd=2,col="grey")
      lines(vl_df[vl_ix,5],log10(vl_df[vl_ix,17]),lwd=2,col="lightblue")
      
      mtext(paste("Agent",ind,": Free virus and infected cells \n(Blk = V, Gold = I, Gry = M, Blue = L)"),side=3)
      #---------------------------------
      # Concentations of viruses with key drug resistance mutations
      seq1<- seq(1,model$param[[sim]]$n_steps,length=10)
      seq2<- seq(-4,0,length=10)
      plot(seq1,seq2,type='n', xlab="Time (Days)",ylab="Log Frequency")
      lines(vl_df[vl_ix,5],log10(vl_df[vl_ix,18]),lwd=2,col="blue")      # Mut1 (K65R in original model)
      lines(vl_df[vl_ix,5],log10(vl_df[vl_ix,19]),lwd=2,col="red")       # Mut2 (M184V in original model)
      lines(vl_df[vl_ix,5],log10(vl_df[vl_ix,20]),lwd=2,col="green")     # Mut3 (K103N in original model)
      lines(vl_df[vl_ix,5],log10(vl_df[vl_ix,21]),lwd=2,col="darkgreen") # Muts35 (K103N + gEFV)
      lines(vl_df[vl_ix,5],log10(vl_df[vl_ix,22]),lwd=2,col="purple")    # Muts12 (K65R + M184V)
      lines(vl_df[vl_ix,5],log10(vl_df[vl_ix,23]),lwd=2,col="gray")      # Mut4 (gTDF)
      lines(vl_df[vl_ix,5],log10(vl_df[vl_ix,24]),lwd=2,col="orange")    # Mut5 (gEFV)
      lines(vl_df[vl_ix,5],log10(vl_df[vl_ix,6]),lwd=1,col="black")      # WT
      
      #mtext(paste("Blue=K65R(TDF), Red=M184V(3TC), Green=K103(EFV), Orange= GenEFV\n,Grey = Generic TDF,DarkGreen = K103N/gEFV (EFV+), Purple = M184V/K65R (TDF+, 3TC+)"),side=3)       
      mtext(paste("Black=WT, Blue=Mut1, Red=Mut2, Green=Mut3, Orange= Mut4\n,Grey = Mut5, Purple = Muts1and2, DarkGreen = Muts3and4"),side=3)       
      
      #---------------------------------
      #drug concentration plot for same agent
      tempvec<- c(as.numeric(vl_df[vl_ix,12]),
                  as.numeric(vl_df[vl_ix,13]),
                  as.numeric(vl_df[vl_ix,14]))
      make_dummy_graph <- FALSE
      if(any(is.na(tempvec))){make_dummy_graph <- TRUE} else {
        if(all(tempvec==0)){make_dummy_graph <- TRUE}
      }
      if (make_dummy_graph == FALSE) {
        #mindrug=(min(tempvec,na.rm=T))
        mindrug=-4.5
        maxdrug=log10(max(tempvec,na.rm=T))
        seq1<- seq(1,model$param[[sim]]$n_steps,length=10)
        seq2<- seq(mindrug*.9,maxdrug*1.1,length=length(seq1))
        plot(seq1,seq2,type='n', xlab="Time (Days)",ylab="Log10 Drug Conc (nM)")
        lines(vl_df[vl_ix,5],log10(vl_df[vl_ix,12]),lwd=2,col="blue")
        lines(vl_df[vl_ix,5],log10(vl_df[vl_ix,13]),lwd=2,col="red")
        lines(vl_df[vl_ix,5],log10(vl_df[vl_ix,14]),lwd=2,col="green")
        lines(vl_df[vl_ix,5],log10(vl_df[vl_ix,25]),lwd=2,col="black")
        ad1=round(model$pop[[sim]]$Adherence1[ind],2)
        ad2=round(model$pop[[sim]]$Adherence2[ind],2)
        ad3=round(model$pop[[sim]]$Adherence3[ind],2)
        ad4=round(model$pop[[sim]]$Adherence4[ind],2)
        if (Therapy_Type == 2) { # First pill contains drugs 1 and 2, Second pill contains drug 3
          ad2 = ad1    # Assume adherence to drug 2 is set by adherence to drug 1 (i.e., ignore Adhernce2)
        }
        if (Therapy_Type == 3) { # A single pill contains all three drugs (used )
          ad2 = ad1  # Assume adherence to all three drugs is set by adherence to drug 1 
          ad3 = ad1  #  (i.e., ignore Adhernce2 and Adherence3)
        }
        templab=paste("Agent ",ind,
                      ": [Drug] (Adherence):\n Blue=Drug1(",ad1,")",
                      "Red=Drug2(",ad2,")", 
                      "Gr=Drug3(",ad3,")" ,
                      "Blck=Drug4(",ad4,")",sep="")
        mtext(templab,side=3)
      } else { # Remake part of first graph so all patient agent graphs remain on one page
          seq1<- seq(1,model$param[[sim]]$n_steps,length=10)
          seq2<- seq(-5,8,length=10)
          plot(seq1,seq(0,6,length=length(seq1)),type='n', xlab=NA,ylab=NA,axes=F)
          lines(vl_df[vl_ix,5],(vl_df[vl_ix,3]),lwd=2,col="green")
          lines(vl_df[vl_ix,5],(vl_df[vl_ix,4]),lwd=2,col="darkgreen")
          axis(4,col="green")
          mtext(paste("VL (Blue) & CD4 (Green) for Agent",ind,"\n(Yel, Gld, Red, Pur, Blk: 0 to 4 muts)"),side=3)
  
      } # Remake Third graph if not possible to make a fourth
    } # agent list not empty
  } # Agents
  dev.off()
} # Function
