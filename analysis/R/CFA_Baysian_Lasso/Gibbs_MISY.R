for(j in 1:NY)
   for(i in 1:N)
      if(missing_ind[j,i]==1){
        mean<-MU[j]+LY[j,]%*%Omega[,i]+PSX[j,-j]%*%chol2inv(chol(PSX[-j,-j]))%*%(Y[-j,i]-MU[-j]-LY[-j,]%*%Omega[,i])
        var<-PSX[j,j]-PSX[j,-j]%*%chol2inv(chol(PSX[-j,-j]))%*%PSX[-j,j]
        Y[j,i]<-rnorm(1, mean, var)

      }



