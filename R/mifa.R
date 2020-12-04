#' Compute covariance matrix of incomplete data using multiple imputation
#'
#' Compute covariance matrix of incomplete data using multiple imputation.
#' For multiple imputation, Multivariate Imputation by Chained Equations
#' (MICE) from the [mice] package is used.
#' Also provides variance explained by different numbers of factors with
#' Fieller (parametric) or bootstrap (nonparametric) confidence intervals.
#'
#' @references
#' Nassiri, V., Lovik, A., Molenberghs, G., & Verbeke, G. (2018).
#' On using multiple imputation for exploratory factor analysis of incomplete
#' data. Behavioral Research Methods 50, 501–517.
#' <https://doi.org/10.3758/s13428-017-1013-4>
#'
#' @param data A data frame with missing values coded as `NA`.
#' @param n_factors Vector indicating number of factors to be used to compute
#' proportion of explained variance and construct confidence intervals.
#' The minimum length of this vector is 1 and its maximum length is
#' the number of columns in data. Defaults to all possible numbers of factors.
#' @param conf Confidence level for constructing confidence intervals. The
#' default is `.95`, i.e., a 95% condifence interval.
#' @param n_boot number of bootstrap samples to use for the bootstrap confidence
#' intervals. The default is 1000.
#' @param ci A character string or character vector indicating which types of
#' confidence intervals should be constructed. If `"bootstrap"`, `"fieller"`,
#' or `"both"`, the corresponding intervals are computed. If `FALSE`
#' (the default) no confidence intervals will be computed.
#' @param ... Named arguments that will be passed to [mice::mice()], such
#' as
#' * `m` Number of generated imputations. The default is 5.
#' * `maxit` Number of iterations for each imputation. The default is 5.
#' * `method` Method used for imputation.
#' * `printFlag` Whether to print diagnostic information to console. The
#'    default is TRUE).
#' See [mice::mice()] for all options.
#'
#' @seealso [mifa_ci_boot()], [mifa_ci_fieller()], [mice::mice()]
#'
#' @return A list:
#' \describe{
#'   \item{cov_combined}{The estimated covariance matrix of the
#'   incomplete data, based on the combined covariance matrices of imputed
#'   data sets.}
#'   \item{cov_imputations}{A list containing the estimated covariance matrixes
#'   for all imputed data sets.}
#'   \item{var_explained}{A data frame containing the estimated proportions of
#'   explained variance for each of specified `n.factor` components. Depending o
#'   n `ci`, it will also contain the estimated Fieller's (parametric) and/or
#'   bootstrap (nonparametric) confidence interval for the proportion of
#'   variance explained by the different numbers of factors defined by
#'   `n_factors`.}
#' }
#' @export
mifa <- function(data, n_factors = 1:ncol(data), ci = FALSE, conf = .95,
                 n_boot = 1000, ...) {

  imp <- stop_constants(mice::mice(data, ...))

  # extract imputed datasets, make sure no NAs are left
  data_imps <- mice::complete(imp, "all")
  data_imps <- lapply(data_imps, function(x) mice_impute_all_NA(x, ...))

  # estimate covariance matrix of imputed values and combine estimates
  cov_imps <- lapply(data_imps, stats::cov)
  cov_comb <- Reduce("+", cov_imps) / length(cov_imps)

  # get proportion of explained variance for different number of factors
  cov_comb_eigen <- eigen(cov_comb)$values

  var_expl <- data.frame(
    n_factors = n_factors,
    var_explained = (cumsum(cov_comb_eigen) / sum(cov_comb_eigen))[n_factors]
  )

  # add confidence intervals for variance explained
  if ("boot" %in% ci || "both" %in% ci) {
    ci_boot <- try(mifa_ci_boot(data, n_factors, conf, n_boot, ...))
    var_expl <- cbind(
      var_expl,
      ci_boot_lower = ci_boot$lower,
      ci_boot_upper = ci_boot$upper
    )
  }
  if ("fieller" %in% ci || "both" %in% ci) {
    ci_fieller <- try(mifa_ci_fieller(cov_imps, n_factors, conf, nrow(data)))
    var_expl <- cbind(
      var_expl,
      ci_fieller_lower = ci_fieller$lower,
      ci_fieller_upper = ci_fieller$upper
    )
  }

  list(
    cov_combined    = cov_comb,
    cov_imputations = cov_imps,
    var_explained   = var_expl
  )
}
