
write(EmLY[CIR,],"Result/Est/EmLY.txt", ncolumns=NLY,append=TRUE,sep="\t")

write(EmMU[CIR,],"Result/Est/EmMU.txt", ncolumns=NMU,append=TRUE,sep="\t")

write(as.vector(EmPSX[CIR,,]),"Result/Est/EmPSX.txt", ncolumns=NY*NY,append=TRUE,sep="\t")

write(EmPHI[CIR,],"Result/Est/EmPHI.txt", ncolumns=NZ*NZ,append=TRUE,sep="\t")

write(SELY[CIR,],"Result/SE/SELY.txt", ncolumns=NLY,append=TRUE,sep="\t")

write(SEMU[CIR,],"Result/SE/SEMU.txt", ncolumns=NMU,append=TRUE,sep="\t")

write(as.vector(SEPSX[CIR,,]),"Result/SE/SEPSX.txt", ncolumns=NY*NY,append=TRUE,sep="\t")

write(SEPHI[CIR,],"Result/SE/SEPHI.txt", ncolumns=NZ*NZ,append=TRUE,sep="\t")
