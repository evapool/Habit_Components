###################    update MU  ################################################################# 
if(IDMUA==T){
	   
   calsm<-chol2inv(chol(N*inv.PSX+diag(rep(sigmu,NY)))) # inv[sigma0^(-1)+N*inv.PSX]
   Ycen<-Y-LY%*%Omega		       
   temp<-rowSums(Ycen)
   mumu<-calsm%*%(inv.PSX%*%temp+rep(sigmu,NY)*PMU)
   MU<-mvrnorm(1,mumu,Sig=calsm)
	      	 	   
} 
###################    end of update MU  ##########################################################	