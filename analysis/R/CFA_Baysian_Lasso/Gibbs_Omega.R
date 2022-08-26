##################  update Omega ##############################################################
    
ISG<-crossprod(inv.sqrt.PSX%*%LY)+inv.PHI 
SIG<-chol2inv(chol(ISG)) 
Ycen<-Y   
if(IDMUA==T) Ycen<-Y-MU   
Mean<-SIG%*%t(LY)%*%inv.PSX%*%Ycen  
for(i in 1:N) Omega[,i]<-xi[,i]<-mvrnorm(1, Mean[,i], Sigma=SIG) 



##################  end of update Omega #######################################################
