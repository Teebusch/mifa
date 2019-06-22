\name{ci.mi.each}
\alias{ci.mi.each}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
ci.mi.each
}
\description{
This function is used inside ci.mifa.fieller to compute the Fieller's interval for each of specified number of factors.
}
\usage{
ci.mi.each(eig.imp, n.factor, alpha, N, M)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{eig.imp}{
A matrix with each of its columns the eigenvalues of the estimated covariance matrix for each imputed data.
}
  \item{n.factor}{
A scalar specifying the number of factors.
}
  \item{alpha}{
The significance level for constructing confidence intervals.
}
  \item{N}{
A scalar specifying sample size}
  \item{M}{
A scalar specifying number of multiple imputations.}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A vector of length 2, containing the lower and upper bouds of estimated Fieller's interval.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Vahid Nassiri, Anikó Lovik, Geert Molenberghs, Geert Verbeke.
}
\note{
Note that one can directly use ci.mifa.fieller. This function will be called in there for internal computations.}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{ci.mifa.fieller}}
}
