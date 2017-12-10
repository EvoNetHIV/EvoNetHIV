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
plot_aim3_drug_muts<-function(model){

for(vv in 1:2){
if(vv==1){
varvec1=c("Perc_0_drug_muts","Perc_1_drug_muts","Perc_3_drug_muts",
          "Perc_4_drug_muts","Perc_All_5_drug_muts")

varvec2=c("Perc_0_drug_muts","Perc_1+_drug_muts","Perc_3+_drug_muts",
"Perc_4+_drug_muts","Perc_All_5_drug_muts")

title1=c("% agents (inf-detect) with 0,1,2,3,4,5 mutations\n (black,red,green,dark blue,aqua)")
title2=c("% agents (inf-detect) with 0,1+,2+,3+,4+,5 mutations\n (black,red,green,dark blue,aqua)")
}
if(vv==2){
    varvec1=c("Perc_0_drug_muts_total_pop","Perc_1_drug_muts_total_pop","Perc_3_drug_muts_total_pop",
              "Perc_4_drug_muts_total_pop","Perc_All_5_drug_muts_total_pop")
    
    varvec2=c("Perc_0_drug_muts_total_pop","Perc_1+_drug_muts_total_pop","Perc_3+_drug_muts_total_pop",
              "Perc_4+_drug_muts_total_pop","Perc_All_5_drug_muts_total_pop")
    
    title1=c("% of population  with 0,1,2,3,4,5 mutations\n (black,red,green,dark blue,aqua)")
    title2=c("% of population  with 0,1+,2+,3+,4+,5 mutations\n (black,red,green,dark blue,aqua)")
  } 
  
   
varlist=list()
varlist[[1]]=varvec1
varlist[[2]]=varvec2
titlelist=list()
titlelist[[1]]=title1
titlelist[[2]]=title2


#model=evomodel
nsteps=model$param[[1]]$n_steps
nsims=model$param[[1]]$nsims

ll=length(model$popsumm[[1]][[1]])
for(jj in 1:length(varlist)){

vars=varlist[[jj]]  
for(ii in 1:length(vars)){

if(nsims>1){
aa=lapply(1:nsims,function(x)model$popsumm[[x]][[vars[ii]]])
bb=do.call(rbind,aa)
cc=colMeans(bb)
}else{
cc=model$popsumm[[1]][[vars[ii]]]
}
  
#head(cc)
if(ii==1){
  plot(seq(0,(nsteps/365),length=ll),
     seq(0,1,length=ll),type='n',xlab="",ylab="")
  mtext("percent mutant type",side=2,line=2.4)
  mtext("years",side=1,line=2)
  mtext(titlelist[[jj]],side=3,col="blue")
  abline(h=seq(0,1,by=.2),col="grey",lty=1)
  
}  
lines(seq(0,(nsteps/365),length=ll),cc,col=ii,lwd=2,lty=1)
}#end of individual plot loop (ii)
}#end of outer loop (jj)
}#end of vv loop
}
