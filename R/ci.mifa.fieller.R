## Computing parametric confidence intervals for proportion of explained variance
# using Fieller's method.
ci.mifa.fieller <- function(cov.mi,n.factor,alpha,N){
  # input variables:
  # cov.mi: a list containing the estimated covariance matrix within each imputed data.
  # one can use the outcome of 'mi.cov' with the name 'cov.mice.imp'.
  # n.factor: a vector indicating the number of factors forwhich the confidence ineterval
  # should be constructed.
  # alpha: the level of significance of the ocnfidence interval.
  # N: sample size.
  n.items=dim(cov.mi[[1]])[1]
  N.factor=length(n.factor)
  M=length(cov.mi)
  eig.imp=matrix(0,M,dim(cov.mi[[1]])[1])
  for (i in 1:M){
    eig.imp[i,]=eigen(cov.mi[[i]])$values
  }
  # computing two parameters of interest: sum of first n.factor
  # eigenvalues and sum of all P of them.
  fieller.ci=matrix(0,N.factor,2)
  for (i in 1:N.factor){
    out.ci=try(ci.mi.each (eig.imp,n.factor[i],alpha,N,M))
    if (is.character(out.ci)==FALSE){
      fieller.ci[i,]=out.ci
    }
  }
  # End of Fieller's interval
  fieller.ci=cbind(n.factor,fieller.ci)
  colnames(fieller.ci)=c('n.factor','Lower','Upper')
  return(fieller.ci)
  # output:
  # a matrix contining 100(1-alpha)% confidence inervals for
  # n.factor factors.
}
