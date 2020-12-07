#' Combine results from different imputations using Rubin's rules
#'
#' Applies Rubin's rules to combine estimates and
#' variance-covariance matrices from different imputations.
#'
#' @references
#' Rubin D. B. Multiple imputation for nonresponse in surveys (2004).
#' John Wiley & Sons.
#'
#' @param param_imps Matrix containing estimated parameters in each imputation
#' as its rows.
#' @param cov_imps List of estimated covariance matrices for each imputation.
#'
#' @return A list:
#' \describe{
#'   \item{param_est}{Vector of combined parameter estimates with the same length
#'   as columns in `param_imps`.}
#'   \item{cov_param}{Combined variance-covariance matrix of size n x n, where n
#'   is the number of columns in `param_imps`.}
#'   \item{between_cov}{Between imputations variance-covariance matrix of size
#'   n x n, where n is the number of columns in `param_imps`.}
#' }
#' @keywords internal
combine_rubin <- function(param_imps, cov_imps) {

  m <- length(cov_imps)

  est_diff          <- scale(param_imps, center = TRUE, scale = FALSE)
  cov_param_sample1 <- lapply(1:m, function(i) est_diff[i, ] %*% t(est_diff[i, ]))
  cov_param_sample1 <- Reduce("+", cov_param_sample1) / (m - 1)

  cov_param_sample  <- cov_param_sample1 * ((m + 1) / m)
  cov_param_mean    <- Reduce(`+`, cov_imps) / m
  cov_param         <- cov_param_mean + cov_param_sample

  list(
    param_est    = apply(param_imps, 2, mean),
    cov_param    = cov_param,
    cov_between  = cov_param_sample1
  )
}
