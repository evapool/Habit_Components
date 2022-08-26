postp1<-0.0
postp2<-0.0
Y.temp<-array(0, dim=c(NY,N))
Y.cen<-Y-MU-LY%*%Omega # NY*N
for(i in 1:N){
   postp1<-postp1+t(Y.cen[,i])%*%inv.PSX%*%Y.cen[,i]
}
       
xi.temp<-t(mvrnorm(N,mu=rep(0,NZ),Sigma=PHI))  
theta.temp<-MU+LY%*%xi.temp  # NY*N
for(i in 1:N) Y.temp[,i]<-mvrnorm(1, theta.temp[,i], Sigma=PSX)
Y.cen<-Y.temp-MU-LY%*%xi.temp  # NY*N
for(i in 1:N){
    postp2<-postp2+t(Y.cen[,i])%*%inv.PSX%*%Y.cen[,i]
}
if(postp1<=postp2) Epostp[gm]<-1.0;

