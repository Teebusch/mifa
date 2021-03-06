% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mifa-ci.R
\name{get_fieller_ci}
\alias{get_fieller_ci}
\title{Find the Fieller interval for each k}
\usage{
get_fieller_ci(eig_imp, n_pc, conf, N, m)
}
\arguments{
\item{eig_imp}{A matrix with each of its columns the eigenvalues of the
estimated covariance matrix for each imputed data.}

\item{n_pc}{Integer or integer vector indicating number of principal
components (eigenvectors) for which explained variance (eigenvalues) should
be obtained and for which confidence intervals should be computed.
Defaults to all principal components, i.e., the number of variables in the
data.}

\item{conf}{Confidence level for constructing confidence intervals. The
default is \code{.95} that is, 95\% confidence intervals.}

\item{N}{A scalar specifying sample size.}

\item{m}{A scalar specifying number of multiple imputations.}
}
\value{
A vector of length 2, containing the lower and upper bounds of
estimated Fieller's interval.
}
\description{
This function is used by \code{\link[=mifa_ci_fieller]{mifa_ci_fieller()}} to compute Fieller's confidence
intervals for each of the components of n_pc.
}
\keyword{internal}
