NM<-0	             #dimension of eta (q_1)
NK<-NM+NZ	       #dimension of latent variables (eta+xi);  number of factors

############################## Automatic done
Y<-array(0,dim=c(NY,N))			#observed data Y

xi<-array(dim=c(NZ,N))			#independent latent variable xi
eta<-array(dim=c(NM,N))			#dependent latent variable eta
TUE<-Omega<-array(dim=c(NK,N))	#latent variable omega

NMU<-sum(IDMU)			      #number of Mu in measurement equation.
NLY<-sum(IDY)				#number of free lambda need to be estimated in Lambda.
Nrec<-(MCMAX-N.burn)/nthin		#number of samples after burn-in.

EMU<-array(0,dim=c(Nrec,NMU))		#Store retained trace of MU
ELY<-array(0,dim=c(Nrec,NLY))		#Store retained trace of Lambda
EPSX<-array(0,dim=c(Nrec,NY,NY))	#Store retained trace of PSX
EinvPSX<-array(0,dim=c(Nrec,NY,NY)) #Store retained trace of inv(PSX)
EPHI<-array(0,dim=c(Nrec,(NZ*NZ)))	#Store retained trace of PHI
EXI<-array(0,dim=c(Nrec,NZ,N))	#Store retained trace of xi
Elambda<-array(0,dim=c(Nrec,1))     #Store retained trace of shrinkage paraemter lambda
 
EmMU<-array(0,dim=c(CNUM,NMU))		#Store estimates of MU 
EmLY<-array(0,dim=c(CNUM,NLY))		#Store estimates of Lambda 
EmPSX<-array(0,dim=c(CNUM,NY,NY))		#Store estimates of PSX
EminvPSX<-array(0,dim=c(CNUM,NY,NY))	#Store estimates of inv(PSX)
EmPHI<-array(0,dim=c(CNUM,(NZ*NZ)))		#Store estimates of PHI
Emlambda<-array(0,dim=c(CNUM,1))          #Store eetimates of shrinkage paraemter lambda


SEMU<-array(0,dim=c(CNUM,NMU))		#Store standard error of estimates of MU 
SELY<-array(0,dim=c(CNUM,NLY))		#Store standard error of estimates of Lambda
SEPSX<-array(0,dim=c(CNUM,NY,NY))		#Store standard error of estimates of PSX
SEinvPSX<-array(0,dim=c(CNUM,NY,NY))	#Store standard error of estimates of inv(PSX)
SEPHI<-array(0,dim=c(CNUM,(NZ*NZ)))       #Store standard error of estimates of PHI
SElambda<-array(0,dim=c(CNUM,1))          #Store standard error of estimates of shrinkage paraemter lambda


indmx<-matrix(1:NY^2, nrow=NY, ncol=NY)
temp<-indmx[upper.tri(indmx)]
upperind<-temp[temp>0]

indmx_t<-t(indmx)
temp<-indmx_t[upper.tri(indmx_t)]
lowerind<-temp[temp>0]

tau<-array(0,dim=c(NY,NY))

ind_noi_all<-array(0,dim=c(NY-1,NY))

for(i in 1:NY){
   
   if(i==1) {ind_noi<-2:NY}
   else if(i==NY) {ind_noi<-1:(NY-1)}
   else ind_noi<-c(1:(i-1),(i+1):NY)
   
   ind_noi_all[,i]<-ind_noi

} # end of i


Empostp<-numeric(CNUM)
chainpsx<-array(0,dim=c(Nrec,NY*(NY+1)/2))







