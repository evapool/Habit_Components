###################    update PSX  #################################################################   


temp<-Y-MU-LY%*%Omega  # NY*N
S<-temp%*%t(temp)      # NY*NY 


apost<-a_lambda+NY*(NY+1)/2;

#sample lambda
bpost<-b_lambda + sum(abs(inv.PSX))/2  # C is the presicion matrix
lambda<- rgamma(1, shape=apost, rate=bpost)



#sample tau off-diagonal
Cadjust<-pmax(abs(inv.PSX[upperind]),10^(-6))
mu_prime<-pmin(lambda/Cadjust, 10^12)
lambda_prime<-lambda^2
tau_temp<-rep(0,length(mu_prime))
for(i in 1:length(mu_prime)){
   tau_temp[i]<-1/rinvgauss(1, mean=mu_prime[i], dispersion=1/lambda_prime)
}
tau[upperind]<-tau_temp
tau[lowerind]<-tau_temp

#sample PSX and inv(PSX)
for(i in 1:NY){
  
   ind_noi<-ind_noi_all[,i]
   tau_temp1<-tau[ind_noi,i]
   Sig11<-PSX[ind_noi, ind_noi]
   Sig12<-PSX[ind_noi,i]      
   invC11<-Sig11-Sig12%*%t(Sig12)/PSX[i,i]
   Ci<-(S[i,i]+lambda)*invC11+diag(1/tau_temp1)
   Sigma<-chol2inv(chol(Ci))  
   mu_i<--Sigma%*%S[ind_noi,i]
   beta<-mvrnorm(1,mu_i,Sigma)
   inv.PSX[ind_noi,i]<-beta
   inv.PSX[i,ind_noi]<-beta
   gam<-rgamma(1, shape=N/2+1, rate=(S[i,i]+lambda)/2)
   inv.PSX[i,i]<-gam+t(beta)%*%invC11%*%beta

   # below updating covariance matrix according to one-column change of precision matrix
    invC11beta<-invC11%*%beta
    PSX[ind_noi,ind_noi]<-invC11+invC11beta%*%t(invC11beta)/gam
    Sig12<--invC11beta/gam
    PSX[ind_noi, i]<-Sig12
    PSX[i,ind_noi]<-t(Sig12)
    PSX[i,i]<-1/gam

} # end of i, sample Sig and C=inv(Sig)

inv.sqrt.PSX<-chol(inv.PSX)
     
##################  end of update PSX ##########################################################
