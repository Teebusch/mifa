#' Compute bootstrap confidence interval for proportion of explained variance
#'
#' This function computes a bootstrap confidence interval for proportion of
#' explained variance for the covariance of an incomplete data imputed using
#' MICE.
#' Please make sure the 'mice' package is installed. To install it use
#' `install.packages("mice")`
#'
#' @param data.miss a matrix containing the incomplete datset with items as
#' columns.
#' @param n.factor a vector containing the number of factors should be used to
#' compute proportion of explained variance.
#' @param rep.boot the number of sub-samples to construct the bootstrap.
#' confidence interval.
#' @param method.mi the method which should be used for imputation. It can be a
#' string or a vector of strings of the size equal to number of items.
#' @param maxit.mi a scalar giving the number of iterations for each imputation,
#  for more information see R documentations for mice package.
#' @param alpha confidence level.
#'
#' @return
#' @export
#'
#' @examples
ci.mifa.bootstrap<- function(data.miss,n.factor,rep.boot=1000,method.mi,maxit.mi,alpha){
  require(mice)
  library(mice)
  N=dim(data.miss)[1]
  boot.eig=matrix(0,dim(data.miss)[2],rep.boot)
  for (i in 1:rep.boot){
    boot.data.idx=sample(1:N,N,replace = T)
    boot.data=data.miss[boot.data.idx,]
    # impute it once
    boot.imp=try(mice(boot.data,m=1,maxit=maxit.mi,method=method.mi,print=FALSE))
    # check if everything is imputed
    method.levels.mi=levels(boot.imp$loggedEvents$meth)
    while ('constant' %in% method.levels.mi){
      boot.data.idx=sample(1:N,N,replace = T)
      boot.data=data.miss[boot.data.idx,]
      # impute it once
      boot.imp=try(mice(boot.data,m=1,maxit=maxit.mi,method=method.mi,print=FALSE))
      # check if everything is imputed
      method.levels.mi=levels(boot.imp$loggedEvents$meth)
    }
    # extracting imputed datasets
    comp.mice=mice::complete(boot.imp,1)
    mi.na=sum(is.na(comp.mice))
    # implementing sequential imputations in case that some of the columns are not
    # imputed due to collinearity, etc.
    while (sum(mi.na)>0){
      imp.tmp=mice(comp.mice, m=1, maxit=maxit.mi,method=method.mi,print=FALSE)
      comp.mice=mice::complete(imp.tmp,1)
      mi.na=sum(is.na(comp.mice))
    }
    ##
    boot.cov.tmp=cov(comp.mice)
    # compute the covariance matrix and its explained variance
    boot.eig[,i]=eigen(boot.cov.tmp)$values
    #boot.exp[i]=sum(boot.eig[1:n.factor])/sum(boot.eig)
    if (is.character(boot.imp)==TRUE){
      i=i-1
    }
  }
  prop.exp=t(apply(boot.eig,2,cumsum))/apply(boot.eig,2,sum)
  boot.exp=apply(prop.exp, 2, quantile, probs=c(alpha/2,(1-alpha/2)))
  boot.exp.all=cbind(n.factor,t(boot.exp)[n.factor,])
  return(boot.exp.all)
}


