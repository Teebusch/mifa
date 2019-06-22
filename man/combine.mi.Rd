\name{combine.mi}
\alias{combine.mi}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
combine.mi}
\description{
This function applies Rubin's rules (Rubin, 2004) to combine eastimates and variance-covariance matrices from different imputations.}
\usage{
combine.mi(mi.parm.est, mi.parm.cov)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mi.parm.est}{
A matrix containing estimated parameters in each imputation as its rows.}
  \item{mi.parm.cov}{
A list contaning the covariance matrix estimated within each imputation.}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
\item{parm.est }{Combined estimates.}
\item{parm.cov }{Combined variance-covariance matrix.}
\item{between.cov }{Between imputations variance-covariance matrix.}

%% ...
}
\references{
Rubin D. B. Multiple imputation for nonresponse in surveys. John Wiley & Sons; 2004.
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
