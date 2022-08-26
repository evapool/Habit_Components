#####Parameter in measurement equation

#initial value of Lambda, (either 1 (for prior mean = 1) or 1.5 (for prior mean = 0.0))
LY<-matrix(c(
  0,1.5, #1
  0,1.5, #2
  1.5,0, #3
  0,1.5, #4
  1.5,0, #5
  0,1.5, #6
  0,1.5, #7
  1.5,0, #8
  1.5,0, #9
  0,1.5, #10
  1.5,0, #11
  0,1.5, #12
  0,1.5, #13
  0,1.5, #14
  0,1.5, #15
  1.5,0, #16
  0,1.5, #17
  0,1.5, #18
  1.5,0, #19
  0,1.5, #20
  1.5,0, #21
  0,1.5, #22
  1.5,0, #23
  0,1.5, #24
  1.5,0, #25
  1.5,0, #26
  0,1.5),ncol=NK,byr=T)

#initial value of MU
MU<-rep(1.0,NY)

#initial value of PHI
PHI<-matrix(0.0,nrow=NZ,ncol=NZ)
diag(PHI[,])<-1.0								
	
#initial value of PSX
xi<-t(mvrnorm(N,mu=rep(0,NZ),Sigma=PHI)) # NZ*N
PSX<-matrix(0.0,nrow=NY,ncol=NY)
diag(PSX)<-1.0
	 
#initial value of PHI^(-1)
inv.PHI<-chol2inv(chol(PHI))		

#initial value of PHI^(-1/2)
c.inv.PHI<-chol(inv.PHI)			

#initial value of PSX^(-1)
inv.PSX<-chol2inv(chol(PSX))

#initial value of PSX^(-1/2)
inv.sqrt.PSX<-chol(inv.PSX)
					
	
if(IDMUA==F) MU<-rep(0,NY)

# initial values for missing data in Y
for(i in 1:NY)
   for(j in 1:N)
      if(is.na(Y[i,j])) Y[i,j]<-rnorm(1)