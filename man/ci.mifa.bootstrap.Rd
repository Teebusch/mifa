\name{ci.mifa.bootstrap}
\alias{ci.mifa.bootstrap}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
ci.mifa.bootstrap
}
\description{
 This function computes a bootstrap confidence interval for the proportion of explaiend variance for given numbers of factors for the incomplete data using multiple imputation.
}
\usage{
ci.mifa.bootstrap(data.miss, n.factor, rep.boot = 1000, method.mi, maxit.mi, alpha)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data.miss}{
The incomplete dataset, a matrix with the items as its columns subject as its rows. The missing values should be shown with NA.}
  \item{n.factor}{
A vector containing number of factors which should be used to compute
proportion of explained variance or construct confidence intervals.}
  \item{rep.boot}{
A scalar specifying the number of bootstrap sub-samples to construct the confidence interval. The default is 1000.}
  \item{method.mi}{
The imputation method, it can be a string
  or a vector of strings of the size equal to number of items. For more information see
  R documentations for mice package. The default is set as 'pmm', i.e., predictive mean matching.}
  \item{maxit.mi}{
A scalar specifying the number of iterations for each imputation.
  For more information see R documentations for mice package. The default is
  5.}
  \item{alpha}{
The significance level for constructing confidence intervals.
}
}
\details{
This function uses the Shao and Sitter (1996) method to combine multiple imputation and bootstrapping. The imputations are done using package mice.
}
\value{
A matrix with three columns, the first column shows the number of factors, and the two other columns show the lower and upper bounds of the estimated bootstrap confidence interval for proportion of explained variance.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Shao, Jun, and Randy R. Sitter. "Bootstrap for imputed survey data." Journal of the American Statistical Association 91.435 (1996): 1278-1288.}
\author{
Vahid Nassiri, Anikó Lovik, Geert Molenberghs, Geert Verbeke.
}
\note{
Note that by setting ci=TRUE in mifa.cov, this confidence interval can be computed as well.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{mifa.cov}}
}
\examples{
# Generating incomplete data
# defining the vector of eigenvalues
e.vals=c(50,48,45,25,20,10,5,5,1,1,0.5,0.5,0.5,0.1,0.1)
# loading eigeninv package to generate a covariance matrix with the
# eigenvalues in e.vals, if this package is not installed, one needs
# to install it first using install.packages("eigeninv")
require(eigeninv)
library(eigeninv)
# Defining the sample size, N, and number of items, P.
P = length(e.vals)
N = 100
# Generate a set of centered indepdent normal data
data.ini1 = matrix(rnorm(N*P),N,P)
mean.data.ini = apply(data.ini1,2,mean)
data.ini = t(t(data.ini1)-mean.data.ini)
# Finding the Cholesky decomposition of the cov.mat
chol.cov=t(chol(cov.mat))
# Using col.cov to generate multivariate normal data
# with the given covariance matrix.
data=matrix(0,N,P)
for (i in 1:N){
  data[i,]=chol.cov\%*\%data.ini[i,]
}
# Here we create 5-percent missing data with
# missing completely at random mechanism
data.miss=data
mcar.n.miss=0.05
for (i in 1:P){
  for (j in 1:N){
    rand.u=runif(1)
    if (rand.u<=mcar.n.miss){
      data.miss[j,i]=NA
    }
  }
}
ci.mifa.bootstrap(data.miss,n.factor=1:10,rep.boot=100,method.mi='pmm',maxit.mi=5,alpha=0.05)
      n.factor      2.5\%     97.5\%
 [1,]        1 0.2503306 0.3466675
 [2,]        2 0.4714291 0.5687062
 [3,]        3 0.6500003 0.7333300
 [4,]        4 0.7889355 0.8430495
 [5,]        5 0.8769695 0.9115910
 [6,]        6 0.9309796 0.9523302
 [7,]        7 0.9612759 0.9750707
 [8,]        8 0.9847418 0.9892099
 [9,]        9 0.9900835 0.9928978
[10,]       10 0.9936964 0.9956437
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
