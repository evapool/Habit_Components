##################  update LY ##############################################################

count.n<-1 
for(j in 1:NY){
            
    subs<-(IDY[j,]==1)
    len<-length(LY[j,subs])
          
    Ycen<-Y[j,]-MU[j]  # 1*N	  
    #Ycen<-Ycen-matrix(LY[j,(!subs),drop=F],nrow=1)%*%matrix(Omega[(!subs),,drop=F],ncol=N) # 1*N
    temp1<-chol2inv(chol(PSX[-j,-j]))  
    Ycen<-Ycen-matrix(LY[j,(!subs)],nrow=1)%*%matrix(Omega[(!subs),],ncol=N)-PSX[j,-j]%*%temp1%*%(Y[-j,]-MU[-j]-LY[-j,]%*%Omega) # 1*N 
    Ycen<-as.vector(Ycen) # vector
	
    if(len>0){
			     
	if(len==1){omesub<-matrix(Omega[subs,],nrow=1)}
	if(len>1){omesub<-Omega[subs,]}
	PSiginv<-diag(len)
      diag(PSiginv)<-rep(sigly,len)
	Pmean<-PLY[j,subs]
      convar<-PSX[j,j]-PSX[j,-j]%*%chol2inv(chol(PSX[-j,-j]))%*%PSX[-j,j]
      invconvar<-chol2inv(chol(convar))
	calsmnpsx<-chol2inv(chol(invconvar%*%tcrossprod(omesub)+PSiginv))
	temp<-(omesub%*%Ycen%*%invconvar+PSiginv*Pmean)
	LYnpsx<-calsmnpsx%*%temp
      LY[j,subs]<-mvrnorm(1,LYnpsx,Sig=(calsmnpsx))
	if((gm>0)&&(gm%%nthin==0)){ELY[gm/nthin,count.n:(count.n+len-1)]<-LY[j,subs]}
      count.n<-count.n+len
		        
    } # end len>0
					
} # end of NY
		     		
    		
				 
			
##################  end of update LY ########################################################