\name{mifa.cov}
\alias{mifa.cov}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
mifa.cov
}
\description{
This function estimates the covariance matrix of an incomplete dataset using multiple imputation.
}
\usage{
mifa.cov(data.miss,n.factor,M,maxit.mi = 5,method.mi='pmm',alpha = 0.05,rep.boot=NULL,ci=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data.miss}{
%%     ~~Describe \code{x} here~~
The incomplete dataset, a matrix with the items as its columns subject as its rows. The missing values should be shown with NA.
}
\item{n.factor}{
%%     ~~Describe \code{x} here~~
A vector containing numbers of factors which should be used to compute
proportion of explained variance or construct confidence intervals.
}
\item{M}{
%%     ~~Describe \code{x} here~~
A scalar specifying number of multiple imputations.
}
\item{maxit.mi}{
%%     ~~Describe \code{x} here~~
A scalar specifying number of iterations for each imputation.
  For more information see R documentation for mice package. The default is
  5.
}
\item{method.mi}{
%%     ~~Describe \code{x} here~~
The imputation method, it can be a string
  or a vector of strings of the size equal to number of items. For more information see
  R documentation for mice package. The default is set as 'pmm', i.e., predictive mean matching.
}
\item{alpha}{
%%     ~~Describe \code{x} here~~
The significance level for constructing confidence intervals. The dafault if 0.05.
}
\item{rep.boot}{
%%     ~~Describe \code{x} here~~
A scalar specifying number of bootstrap sub-samples to construct the confidence interval. If ci=TRUE rep.boot should be specified.
}
\item{ci}{
%%     ~~Describe \code{x} here~~
A logical variable indicating whether confidence intervals should be
  constructed for proportion of explianed variance or not. The default value is FALSE.
}
}
\details{
Note that one needs to install the package 'mice' before using this function. This can be done use the command: install.packages("mice")}

\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
\item{cov.mice }{The estimatied covariance matrix of the incomplete data using multiple imputations.}
\item{cov.mice.imp }{A list containing th estimated covariance matrix for each of M imputed data.}
\item{exp.var.mice}{A vector containing the estimated proportions of explained variance for each of specified n.factor components.}
\item{ci.mice.fieller}{A matrix containing the estimated Fieller's confidence interval for proportion of explained variance for each of specified n.factor components.}
\item{ci.mice.bootstrap}{A matrix containing the estimated bootstrap confidence interval for proportion of explained variance for each of specified n.factor components.}
%% ...
}
\references{
The mice documentation: https://cran.r-project.org/web/packages/mice/mice.pdf
}
\author{
Vahid Nassiri, Anikó Lovik, Geert Molenberghs, Geert Verbeke.
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
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
cov.mat = eiginv(evals=e.vals, symmetric=TRUE)
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
result.mi=mifa.cov (data.miss,n.factor=1:10,M=10,maxit.mi = 5,method.mi='pmm',
                  alpha = 0.05,rep.boot=500,ci=TRUE)

}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{mifa.cov}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
