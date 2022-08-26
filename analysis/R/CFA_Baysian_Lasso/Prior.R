######Hyperparameter of prior distribution.

#Measurement equation.

#Prior mean of Lambda, NY*NZ (either 0.0 or 1)
PLY<-matrix(c(
  0,0.0, #1
  0,0.0, #2
  0.0,0, #3
  0,0.0, #4
  0.0,0, #5
  0,0.0, #6
  0,0.0, #7
  0.0,0, #8
  0.0,0, #9
  0,0.0, #10
  0.0,0, #11
  0,0.0, #12
  0,0.0, #13
  0,0.0, #14
  0,0.0, #15
  0.0,0, #16
  0,0.0, #17
  0,0.0, #18
  0.0,0, #19
  0,0.0, #20
  0.0,0, #21
  0,0.0, #22
  0.0,0, #23
  0,0.0, #24
  0.0,0, #25
  0.0,0, #26
  0,0.0),ncol=NZ,byr=T)

#Prior mean of MU, NY*1
PMU<-rep(0.0,NY)		    
		   
#Inverse prior variance of unknown parameters in factor loading matrix
sigly<-0.25

#Inverse prior variance of unknown parameters in intercept
sigmu<-0.25

rou.scale<-6.0

#rho_0, hyperparameters of Wishart distribution			
rou.zero<-rou.scale+NZ+1	

#Matrix R_0, hyperparameters of Wishart distribution
R.zero<-rou.scale*diag(1,NZ)

#Hyperparameters of Gamma distribution for the shrinkage parameter
a_lambda<-1
b_lambda<-0.01


