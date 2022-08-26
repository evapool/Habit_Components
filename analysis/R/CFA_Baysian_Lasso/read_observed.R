#Read the observed data from a text file named data.txt with dimension N*NY
Y[,]<-matrix(scan("COHS_french.txt",skip=(CIR-1)*N,nlines=N,sep="\t"),nrow=NY,ncol=N) 

#Standardized the data
Y.temp<-t(Y)   
Y<-t(scale(Y.temp)) 

#Creat the matrix of missing indicators where 1 represents missing
missing_ind<-array(0, dim=c(NY, N))
for(i in 1:NY)
   for(j in 1:N)
      if(is.na(Y[i,j])) missing_ind[i,j]<-1