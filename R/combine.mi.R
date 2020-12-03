#' Combine results from different imputations
#'
#' This function applies Rubin's rule to combine results from different
#' imputations
#'
#' @param mi.parm.est Matrix containing estimated parameters in each imputation
#' as its rows.
#' @param mi.parm.cov List containing the covariance matrix estimated within
#' each imputation.
#'
#' @return
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
