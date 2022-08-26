#########Revise the parts below according to dimension of measurement equation.

#Matrix indicating which element in Lambda is free to estmate (1) or fixed(0) 
IDY<-matrix(c(
0,1, #1
0,1, #2
1,0, #3
0,1, #4
1,0, #5
0,1, #6
0,1, #7
1,0, #8
1,0, #9
0,1, #10
1,0, #11
0,1, #12
0,1, #13
0,1, #14
0,1, #15
1,0, #16
0,1, #17
0,1, #18
1,0, #19
0,1, #20
1,0, #21
0,1, #22
1,0, #23
0,1, #24
1,0, #25
1,0, #26
0,1),ncol=NZ,byr=T)#27

#vector indicating whether MU should be added to each measurement equation or not. 
#Added(1), remove(0).
IDMU<-rep(1,NY)  # now MU is estimated
IDMUA<-any(as.logical(IDMU))


