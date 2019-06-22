# find the Fieller interval for each k
ci.mi.each <- function(eig.imp,n.factor,alpha,N,M){
  # this function is loaded within ci.mi and will compute
  # Fieller's confidence interval for each of the components of n.factor.
  eig.imp2=eig.imp^2
  mi.cov=NULL
  for (i in 1:M){
    mi.cov[[i]]=(2/N)*diag(eig.imp2[i,])
  }
  # combining them into one single imputation
  mi.comb=combine.mi ((eig.imp),mi.cov)
  cov.lambda=mi.comb$parm.cov
  P=dim(cov.lambda)[1]
  A1=matrix(c(rep(1,n.factor),rep(0,P-n.factor)),1,P)
  A=matrix(1,1,P)
  sigma11.comb=(A1%*%cov.lambda)%*%t(A1)
  sigma22.comb=(A%*%cov.lambda)%*%t(A)
  sigma12.comb=(A1%*%cov.lambda)%*%t(A)
  var.prop.comb=sum(mi.comb$parm.est[1:n.factor])/sum(mi.comb$parm.est)
  # computing Fieller's inervals
  S=matrix(c(sigma11.comb,sigma12.comb,sigma12.comb,sigma22.comb),2,2)
  eigen.all=sum(mi.comb$parm.est)
  eigen.first=sum(mi.comb$parm.est[1:n.factor])
  s11=S[1,1]
  s22=S[2,2]
  s12=S[1,2]
  C12=s11/(eigen.first^2)
  C22=s22/(eigen.all^2)
  r=s12/sqrt(s11*s22)
  A=C12+C22-(2*r*sqrt(C12*C22))
  T.crit=qnorm(1-(alpha/2))
  B=(T.crit^2) * C12 *C22* (1-(r^2))
  fieller1=eigen.first/eigen.all
  fieller2= 1-(T.crit^2 * r *sqrt(C12*C22) )
  if ((A-B)<0){
    stop('Computing Fieller CI is not possible, use a larger number of imputations.')
  }
  fieller3=T.crit*sqrt(A-B)
  fieller4=1-((T.crit^2)*C22)
  fieller.low=fieller1*((fieller2-fieller3)/fieller4)
  fieller.up=fieller1*((fieller2+fieller3)/fieller4)
  return(c(fieller.low,fieller.up))
}
