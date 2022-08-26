Epostp<-array(0, dim=c(Nrec,1))

for(g in 1:MCMAX){

    gm<-g-N.burn

    #Generate the latent factors from its conditinal distribution
    source("Gibbs_Omega.R") 

    #Generate the unknown parameter of intercept in CFA from its conditinal distribution
    source("Gibbs_MU.R") 
    
    #Generate the unknown parameter of covariance matrix of measurement errors in CFA from its conditinal distribution
    source("Gibbs_PSX.R")
        
    #Generate the unknown parameter of factor loading matrix in CFA from its conditinal distribution
    source("Gibbs_LY.R")
    
    #Generate the unknown parameter of covariance matrix of latent factors in CFA from its conditinal distribution
    source("Gibbs_PHI.R")

    #Generate the missing reponse in CFA from its conditinal distribution
    source("Gibbs_MISY.R")

	
    #Save results
    if((gm>0)&&(gm%%nthin==0)){
       gm<-gm/nthin
       EPHI[gm,]<-as.vector(PHI[,])
       EPSX[gm,,]<-PSX
       EinvPSX[gm,,]<-inv.PSX
       EMU[gm,]<-MU
       k<-1
       for(i in 1:NY){
          for(j in 1:NY){
            if(i>=j) {chainpsx[gm,k]<-PSX[i,j];k<-k+1}
          }
        }
       
       #Calculate PP p-value
       source("Postp.R")   

     }
	
    if(g%%1000==0)print(g)
    
   
}#end of g MCMAX

