#' Get covariance matrix of incomplete data using multiple imputation
#'
#' Compute covariance matrix of incomplete data using multiple imputation.
#' For multiple imputation, *Multivariate Imputation by Chained Equations*
#' (MICE) from the [mice] package is used. The covariance matrices of the
#' imputed data sets are combined using Rubin's rules.
#'
#' The function also computes the variance explained by different numbers of
#' principal components and the corresponding Fieller (parametric) or bootstrap
#' (nonparametric) confidence intervals.
#'
#' @references
#' Nassiri, V., Lovik, A., Molenberghs, G., & Verbeke, G. (2018).
#' On using multiple imputation for exploratory factor analysis of incomplete
#' data. Behavioral Research Methods 50, 501–517.
#' <https://doi.org/10.3758/s13428-017-1013-4>
#'
#' @param data A data frame with missing values coded as `NA`.
#' @param cov_vars Variables in `data` for which to calculate the covariance
#' matrix. Supports (tidy selection)[dplyr::select()]. This allows to
#' select variables that are used for the imputations of missing values, but not
#' the calculations of the covariance matrix. This is especially useful when
#' there are categorical predictors that can improve the imputation of the
#' response variables, but for which covariance cannot be calculated.
#' By default, all variables in `data` are used for both, the imputation and
#' the covariance matrix. Note: Variables and rows used for the imputation, as
#' well as the method for imputation can be configured using the `...`.
#' See also [mice::mice()].
#' @param n_pc Integer or integer vector indicating number of principal
#' components (eigenvectors) for which explained variance (eigenvalues) should
#' be obtained and for which confidence intervals should be computed.
#' Defaults to all principal components, i.e., the number of variables in the
#' data.
#' @param conf Confidence level for constructing confidence intervals. The
#' default is `.95` that is, 95% confidence intervals.
#' @param n_boot Number of bootstrap samples to use for bootstrapped confidence
#' intervals. The default is 1000.
#' @param ci A character string indicating which types of confidence intervals
#' should be constructed for the variance explained by the principal
#' components. If `"boot"`, `"fieller"`, or `"both"`, the corresponding
#' intervals are computed. If `FALSE` (the default) no confidence intervals will
#' be computed. The components for which confidence intervals should be computed
#' can be set with `n_pc`. See [mifa_ci_boot()] and [mifa_ci_fieller()] for
#' details about the two methods.
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
#'   explained variance for each of specified `n_pc` components. Depending o
#'   n `ci`, it will also contain the estimated Fieller's (parametric) and/or
#'   bootstrap (nonparametric) confidence interval for the proportion of
#'   variance explained by the different numbers of principal components defined
#'   by `n_pc`.}
#'   \item{mids}{Object of type [mids](mids-class). This is the results of
#'   the multiple imputation step for the covariance matrix. Can be useful for
#'   diagnosing the multiple imputations.}
#' }
#' @export
#' @examples
#' \dontrun{
#' data <- psych::bfi
#' mifa(data, cov_vars = -c(age, education, gender), ci = "fieller", print = FALSE)
#' }
mifa <- function(data, cov_vars = dplyr::everything(), n_pc, ci = FALSE,
                 conf = .95, n_boot = 1000, ...) {
  # check and clean arguments
  checkmate::assert_data_frame(data, min.rows = 1, min.cols = 2)
  checkmate::assert_data_frame(dplyr::select(data, {{ cov_vars }}), min.cols = 2)
  if (ci != FALSE) {
    checkmate::assert_choice(ci, c(FALSE, "both", "boot", "fieller"))
  }
  checkmate::assert_number(conf, lower = 0, upper = 1)
  checkmate::assert_count(n_boot, positive = TRUE)

  n_cov_vars <- ncol(dplyr::select(data, {{ cov_vars }}))
  n_pc <- clean_n_pc(n_pc, n_cov_vars)

  # create imputed datasets, make sure no NAs are left
  imp <- stop_constants(mice::mice(data, ...))
  data_imps <- mice::complete(imp, "all")
  data_imps <- lapply(data_imps, function(x) mice_impute_all_NA(x, ...))

  # Select variables for calculating covariance matrix
  data_imps <- lapply(data_imps, function(d) dplyr::select(d, {{ cov_vars }}))

  # estimate covariance matrix of imputed values and combine estimates
  cov_imps <- lapply(data_imps, stats::cov)
  cov_comb <- Reduce("+", cov_imps) / length(cov_imps)

  # get proportion of explained variance for different number of pr. components
  cov_comb_eigen <- eigen(cov_comb)$values

  var_expl <- data.frame(
    n_pc = n_pc,
    var_explained = (cumsum(cov_comb_eigen) / sum(cov_comb_eigen))[n_pc]
  )

  # add confidence intervals for variance explained
  if ("boot" %in% ci || "both" %in% ci) {
    ci_boot <- mifa_ci_boot(data, {{ cov_vars }}, n_pc, conf, n_boot, ...)
    var_expl <- dplyr::bind_cols(
      var_expl,
      ci_boot_lower = ci_boot$lower,
      ci_boot_upper = ci_boot$upper
    )
  }
  if ("fieller" %in% ci || "both" %in% ci) {
    ci_fieller <- mifa_ci_fieller(cov_imps, n_pc, conf, nrow(data))
    var_expl <- dplyr::bind_cols(
      var_expl,
      ci_fieller_lower = ci_fieller$lower,
      ci_fieller_upper = ci_fieller$upper
    )
  }

  structure(
    list(
      cov_combined = cov_comb,
      cov_imputations = cov_imps,
      var_explained = var_expl,
      mids = imp
    ),
    class = "mifa"
  )
}



#' @export
summary.mifa <- function(object, ...) {
  mi <- object
  cat(sprintf(
    "Imputed covariance matrix of %i variables\n\n",
    ncol(mi$cov_combined)
  ))

  last_to_print <- function(x, max_width = getOption("width")) {
    widths <- pmax(nchar(names(x)), nchar(x))
    Position(function(x) x > max_width, cumsum(widths), nomatch = length(x))
  }

  # Details about data

  vars_cov <- colnames(mi$cov_combined)
  n_missing <- colSums(is.na(mi$mids$data[, vars_cov]))
  widths <- pmax(nchar(names(n_missing)), nchar(n_missing))
  n_print <- last_to_print(n_missing)

  cat("Variable:  ", sprintf("%*s", widths[1:n_print], names(n_missing)[1:n_print]), "\n")
  cat("N Imputed: ", sprintf("%*i", widths[1:n_print], n_missing[1:n_print]))

  if (n_print < length(vars_cov)) {
    cat(sprintf(" ...\n...and %i more variables", length(vars_cov) - n_print))
  }

  # Details on imputation

  cat(sprintf("\n\nNumber of MICE imputations: %i", length(mi$cov_imputations)))

  vars_imp <- colnames(mi$mids$data)
  vars_diff <- setdiff(vars_imp, vars_cov)

  if (length(vars_diff) > 0) {
    cat(sprintf("\nAdditional variables used for imputations:\n"))

    n_print <- last_to_print(vars_diff)

    cat(sprintf("%s", vars_diff[1:n_print]))
    if (n_print < length(vars_diff)) {
      cat(sprintf("... \n...and %i more variables", length(vars_diff) - n_print))
    }
  }

  # variance expplained

  if (is.null(mi$var_explained$ci_fieller_lower)) {
    ci_fieller <- NULL
  } else {
    ci_fieller <- sprintf(
      "[%.2f, %.2f]",
      mi$var_explained$ci_fieller_lower,
      mi$var_explained$ci_fieller_upper
    )
  }

  if (is.null(mi$var_explained$ci_boot_lower)) {
    ci_boot <- NULL
  } else {
    ci_boot <- sprintf(
      "[%.2f, %.2f]",
      mi$var_explained$ci_boot_lower,
      mi$var_explained$ci_boot_upper
    )
  }

  k_width <- max(nchar(mi$var_explained$n_pc))
  cat("\n\nCumulative proportion of variance explained by n principal components:")
  cat(sprintf("\n\n%*s  prop  ", k_width, "n"))
  if (!is.null(ci_fieller)) cat("Fieller CI    ")
  if (!is.null(ci_boot)) cat("Bootstrap CI")

  cat(
    "\n",
    sprintf(
      "%*i %5.2f  %s  %s\n",
      k_width,
      mi$var_explained$n_pc,
      mi$var_explained$var_explained,
      if (is.null(ci_fieller)) "" else ci_fieller,
      if (is.null(ci_boot)) "" else ci_boot
    ),
    "\n",
    sep = ""
  )
}



#' @export
print.mifa <- function(x, ...) {
  summary(x)
}
