#' Combine results from different imputations using Rubin's rules
#'
#' Applies Rubin's rules to combine estimates and
#' variance-covariance matrices from different imputations.
#'
#' @references
#' Rubin D. B. Multiple imputation for nonresponse in surveys (2004).
#' John Wiley & Sons.
#'
#' @param mi.parm.est Matrix containing estimated parameters in each imputation
#' as its rows.
#' @param mi.parm.cov List containing the covariance matrix estimated within
#' each imputation.
#'
#' @return A list:
#' \describe{
#'   \item{parm.est}{Combined estimates.}
#'   \item{parm.cov}{Combined variance-covariance matrix.}
#'   \item{between.cov}{Between imputations variance-covariance matrix.}
#' }
#' @export
#'
#' @examples
combine.mi <- function(mi.parm.est, mi.parm.cov) {
  M                <- length(mi.parm.cov)
  parm.est         <- apply(mi.parm.est, 2, mean)
  est.diff         <- scale(mi.parm.est, center = TRUE, scale = FALSE)
  Sample.cov.parm1 <- NULL

  for (i in 1:M) {
    Sample.cov.parm1[[i]] <- (est.diff[i, ]) %*% t(est.diff[i, ])
  }

  sample.cov.parm1 <- Reduce("+", Sample.cov.parm1) / (M - 1)
  Factor           <- (M + 1) / M
  sample.cov.parm  <- Factor * sample.cov.parm1
  mean.cov.parm    <- Reduce(`+`, mi.parm.cov) / M
  parm.cov         <- mean.cov.parm + sample.cov.parm

  return(list(
    parm.est    = parm.est,
    parm.cov    = parm.cov,
    between.cov = sample.cov.parm1
  ))
}
