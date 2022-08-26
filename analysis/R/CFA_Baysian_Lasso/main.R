#setwd("E:/R code/")

if(!require(MCMCpack)) install.packages(pkgs="MCMCpack",repos="http://cran.r-project.org")
if(!require(msm)) install.packages(pkgs="msm",repos="http://cran.r-project.org")
if(!require(statmod)) install.packages(pkgs="statmod",repos="http://cran.r-project.org")
if(!require(psychometric)) install.packages(pkgs="psychometric",repos="http://cran.r-project.org")

library(stats)
library(MASS)
library(MCMCpack)
library(msm)
library(statmod)
library(psychometric)

set.seed(1)

source("def_con.R") 
source("def_rec.R")
source("ind.R")   
source("Prior.R")    
     
for(CIR in 1:CNUM){
    
	source("read_observed.R")  
	source("init1.R")	         										
      source("Gibbs.R")          
	EmLY[CIR,]=apply(ELY,FUN=mean,MAR=c(2))
      EmMU[CIR,]=apply(EMU,FUN=mean,MAR=c(2))
      EmPSX[CIR,,]=apply(EPSX,FUN=mean,MAR=c(2,3))
      EminvPSX[CIR,,]=apply(EinvPSX,FUN=mean,MAR=c(2,3))
      EmPHI[CIR,]=apply(EPHI,FUN=mean,MAR=c(2))
      Emlambda[CIR]=mean(Elambda)
      
      SELY[CIR,]=apply(ELY,FUN=sd,MAR=c(2))
      SEMU[CIR,]=apply(EMU,FUN=sd,MAR=c(2))
      SEPSX[CIR,,]=apply(EPSX,FUN=sd,MAR=c(2,3))
      SEinvPSX[CIR,,]=apply(EinvPSX,FUN=sd,MAR=c(2,3))
      SEPHI[CIR,]=apply(EPHI,FUN=sd,MAR=c(2))
      SElambda[CIR]=sd(Elambda)

      Empostp[CIR]=mean(Epostp)
            
	source("write_result.R")   
      source("HPD.R")                  
      source("write_model fit.R")
      
      print(CIR)
	
}#end of GIBs
