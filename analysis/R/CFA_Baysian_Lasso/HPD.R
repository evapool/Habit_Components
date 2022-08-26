c<-numeric(NY*(NY+1)/2)
temp.sig<-array(0,dim=c(NY,NY))

inter<-HPDinterval(mcmc(chainpsx))   # 95% HPD interval for PSX
for(i in 1:(NY*(NY+1)/2)){
    if(inter[i,1]<0 && inter[i,2]>0) c[i]<-0
    else c[i]<-1;
}  # 1 indicates significance  

k<-1
for(i in 1:NY){
   for(j in 1:NY){
      if(i>=j){temp.sig[i,j]<-c[k];k<-k+1}
   }
}     
position.sig<-which(temp.sig==1,arr.ind=T)
write(t(inter),"Result/Est/HPD_PSX.txt", ncolumns=2,append=TRUE,sep="\t")

temp.interval<-array(0,dim=c(NY,NY,2))
k<-1
for(i in 1:NY){
   for(j in 1:NY){
      if(i>=j){temp.interval[i,j,]<-inter[k,];k<-k+1}
   }
}     

temp.length<-length(position.sig[,1])
for(i in 1:temp.length){
   a<-position.sig[i,]
   write(t(temp.interval[a[1],a[2],]),"Result/Est/HPD_PSX_sig.txt", ncolumns=2,append=TRUE,sep="\t")             
}


c<-numeric(NLY)
temp.sig<-array(0,dim=c(NLY))
inter<-HPDinterval(mcmc(ELY))   # 95% HPD interval for LY
write(t(inter),"Result/Est/HPD_LY.txt", ncolumns=2,append=TRUE,sep="\t")


c<-numeric(NY)
temp.sig<-array(0,dim=c(NY))
inter<-HPDinterval(mcmc(EMU))   # 95% HPD interval for MU
write(t(inter),"Result/Est/HPD_MU.txt", ncolumns=2,append=TRUE,sep="\t")

c<-numeric(NZ*NZ)
temp.sig<-array(0,dim=c(NZ*NZ))
inter<-HPDinterval(mcmc(EPHI))   # 95% HPD interval for PHI
write(t(inter),"Result/Est/HPD_PHI.txt", ncolumns=2,append=TRUE,sep="\t")





