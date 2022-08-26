########  update PHI ########################################################

inv.PHI<-rwish(rou.zero+N, solve(tcrossprod(Omega)+R.zero)) 
PHI<-chol2inv(chol(inv.PHI))
c.inv.PHI<-chol(inv.PHI)
     	
########  end of update PHI #################################################