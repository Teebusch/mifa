#' Get covariance matrix of incomplete data using multiple imputation
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
#' @param cov_vars Variables in `data` to calculate the covariance
#' matrix for. Supports (tidy selection)[dplyr::select()]. This allows to
#' select variables that are used for the imputations of missing values, but not
#' the calculations of the covariance matrix. This is especially useful when
#' there are categorical predictors that can improve the imputation of the
#' response variables, but for which covariance cannot be calculated.
#' By default, all variables in `data` are used for both, the imputation and
#' the covariance matrix. Note: Variables and rows used for the imputation
#' can be configured using the `...`. See also [mice::mice()].
#' @param n_factors Integer or integer vector indicating number of factors for
#' which proportion of explained variance (and optionally confidence intervals)
#' should be indicated. Defaults to all possible numbers of factors.
#' @param conf Confidence level for constructing confidence intervals. The
#' default is `.95`, i.e., a 95% confidence interval.
#' @param n_boot number of bootstrap samples to use for the bootstrap confidence
#' intervals. The default is 1000.
#' @param ci A character string or character vector indicating which types of
#' confidence intervals should be constructed. If `"boot"`, `"fieller"`,
#' or `"both"`, the corresponding intervals are computed. If `FALSE`
#' (the default) no confidence intervals will be computed.
#' @inheritDotParams mice::mice
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
#'   \item{mids}{Object of type (mids)[mids-class]. This is the results of
#'   the multiple imputation step for the covariance matrix. Can be useful for
#'   diagnosing the multiple imputations.}
#' }
#' @export
#' @examples
#' \dontrun{
#' data <- psych::bfi
#' mifa(data, cov_vars = -c(age, education, gender), ci = "fieller", print = FALSE)
#' }
mifa <- function(data, cov_vars, n_factors, ci = FALSE, conf = .95,
                 n_boot = 1000, ...) {

  imp <- stop_constants(mice::mice(data, ...))

  # extract imputed datasets, make sure no NAs are left
  data_imps <- mice::complete(imp, "all")
  data_imps <- lapply(data_imps, function(x) mice_impute_all_NA(x, ...))

  # Select variables for calculating covariance matrix
  if(!missing(cov_vars)) {
    data_imps <- lapply(data_imps, function(d) dplyr::select(d, {{ cov_vars }}))
  }

  # estimate covariance matrix of imputed values and combine estimates
  cov_imps  <- lapply(data_imps, stats::cov)
  cov_comb  <- Reduce("+", cov_imps) / length(cov_imps)

  # get proportion of explained variance for different number of factors
  cov_comb_eigen <- eigen(cov_comb)$values

  if (missing(n_factors)) {
    n_factors <- 1:length(cov_comb_eigen)
  }

  var_expl <- data.frame(
    n_factors = n_factors,
    var_explained = (cumsum(cov_comb_eigen) / sum(cov_comb_eigen))[n_factors]
  )

  # add confidence intervals for variance explained
  if ("boot" %in% ci || "both" %in% ci) {
    ci_boot <- mifa_ci_boot(data, cov_vars, n_factors, conf, n_boot, ...)
    var_expl <- dplyr::bind_cols(
      var_expl,
      ci_boot_lower = ci_boot$lower,
      ci_boot_upper = ci_boot$upper
    )
  }
  if ("fieller" %in% ci || "both" %in% ci) {
    ci_fieller <- mifa_ci_fieller(cov_imps, n_factors, conf, nrow(data))
    var_expl <- dplyr::bind_cols(
      var_expl,
      ci_fieller_lower = ci_fieller$lower,
      ci_fieller_upper = ci_fieller$upper
    )
  }

  list(
    cov_combined    = cov_comb,
    cov_imputations = cov_imps,
    var_explained   = var_expl,
    mids            = imp
  )
}
