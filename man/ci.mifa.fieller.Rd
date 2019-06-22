\name{ci.mifa.fieller}
\alias{ci.mifa.fieller}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
ci.mifa.fieller
}
\description{
This function computes Fieller's confidence interval for the proportion of explained variance for given numbers of factors.
}
\usage{
ci.mifa.fieller(cov.mi, n.factor, alpha, N)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cov.mi}{
A list containing the estimated covariance matrix for each imputed data.
   One can use the outcome of 'mi.cov' with the name 'cov.mice.imp'.
}
  \item{n.factor}{
A vector containing the numbers of factors that should be used to compute the proportion of explained variance or construct confidence intervals. The minimum length of this vector is 1 and its maximum length is the number of items.
}
  \item{alpha}{
The significance level for constructing confidence intervals.
}
  \item{N}{
An scalar specifying the sample size.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A matrix with three columns, the first column shows the number of factor, and the two other columns show the lower and uppor bound of the estimated Fieller's confidence interval for proportion of explained variance.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Fieller, Edgar C. "Some problems in interval estimation." Journal of the Royal Statistical Society. Series B (Methodological) (1954): 175-185.}
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
  data[i,]=chol.cov%*%data.ini[i,]
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
                  alpha = 0.05,rep.boot=500,ci=FALSE)
ci.mifa.fieller (result.mi$cov.mice.imp,1:10,0.05,N=100)
      n.factor     Lower     Upper
 [1,]        1 0.2190723 0.3496561
 [2,]        2 0.4317744 0.5553530
 [3,]        3 0.6440130 0.7350537
 [4,]        4 0.7576282 0.8270035
 [5,]        5 0.8511427 0.8970771
 [6,]        6 0.9188874 0.9448734
 [7,]        7 0.9540987 0.9698366
 [8,]        8 0.9788137 0.9851842
 [9,]        9 0.9852719 0.9899236
[10,]       10 0.9906949 0.9937464

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
