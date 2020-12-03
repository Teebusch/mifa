#' Compute covariance matrix of incomplete data using multiple imputation
#'
#' Compute covariance matrix of incomplete data using multiple imputation.
#' For multiple imputation, Multivariate Imputation by Chained Equations
#' (MICE) from the [mice] package is used.
#'
#' @references
#' Nassiri, V., Lovik, A., Molenberghs, G., & Verbeke, G. (2018).
#' On using multiple imputation for exploratory factor analysis of incomplete
#' data. Behavioral Research Methods 50, 501–517.
#' <https://doi.org/10.3758/s13428-017-1013-4>
#'
#' @param data.miss Dataset with missing values coded as `NA`.
#' @param n.factor Vector indicating number of factors to be used to compute
#' proportion of explained variance or construct confidence intervals.
#' The minimum length of this vector is 1 and its maximum length is
#' the number of items.
#' @param M Number of generated imputations. See [mice::mice()].
#' @param maxit.mi A scalar giving the number of iterations for each imputation,
#' for more information see R documentations for mice package. The default is 5.
#' @param method.mi Method used for imputation. It can be a
#' string or a vector of strings of the size equal to number of items.
#' The default is `"pmm"`, i.e., predictive mean matching. See [mice::mice()].
#' @param alpha Significance level for constructing confidence intervals.
#' @param rep.boot number of bootstrap samples to use for bootstrap confidence
#' intervals. If `ci = TRUE` then `rep.boot` must be specified.
#' @param ci A logical variable indicating whether a confidence interval should
#' be constructed for proportion of explained variance or not. The default value
#' is `FALSE`.
#'
#' @seealso [ci.mifa.bootstrap()], [ci.mifa.fieller()], [mice::mice()]
#'
#' @return A list:
#' \describe{
#'   \item{cov.mice}{The estimated covariance matrix of the incomplete data
#'   using multiple imputations.}
#'   \item{cov.mice.imp}{A list containing th estimated covariance matrix for
#'   each of M imputed data.}
#'   \item{exp.var.mice}{A vector containing the estimated proportions of
#'   explained variance for each of specified n.factor components.}
#'   \item{ci.mice.fieller}{A matrix containing the estimated Fieller's
#'   confidence interval for proportion of explained variance for each of
#'   specified n.factor components. NULL, if `ci = FALSE`.}
#'   \item{ci.mice.bootstrap}{A matrix containing the estimated bootstrap
#'   confidence interval for proportion of explained variance for each of
#'   specified n.factor components. NULL, if `ci = FALSE`.}
#' }
#' @export
mifa.cov <- function(data.miss, n.factor, M, maxit.mi = 5, method.mi = "pmm",
                     alpha = 0.05, rep.boot = NULL, ci = FALSE) {
  if (ci & !is.numeric(rep.boot)) {
    stop(paste("You have set ci = TRUE, please set the number of bootstrap",
               "sub-samples with rep.boot"))
  }

  N <- dim(data.miss)[1]

  imputed_mice <- mice::mice(data.miss, m = M, maxit = maxit.mi,
                       method = method.mi, print = FALSE)

  ### Begin sequential imputation
  # checking if everything is imputed
  method.levels.mi <- levels(imputed_mice$loggedEvents$meth)
  if ("constant" %in% method.levels.mi) {
    stop("Probably at least one column with constant observed part.")
  }

  # extracting imputed datasets
  comp.mice <- NULL
  mi.na <- rep(0, M)
  for (i in 1:M) {
    comp.mice[[i]] <- mice::complete(imputed_mice, i)
    mi.na[i] <- sum(is.na(comp.mice[[i]]))
  }

  # implementing sequential imputations in case that some of the columns are not
  # imputed due to collinearity, etc.
  while (sum(mi.na) > 0) {
    for (i in 1:M) {
      imp.tmp <- mice::mice(comp.mice[[i]], m = 1, maxit = maxit.mi,
                      method = method.mi, print = FALSE)
      comp.mice[[i]] <- mice::complete(imp.tmp, 1)
      mi.na[i] <- sum(is.na(comp.mice[[i]]))
    }
  }

  ### End sequential imputation:
  # Now estimating covariance matrix based on imputed values
  cov.mice.imp <- NULL
  prop.exp <- rep(0, M)
  for (i in 1:M) {
    cov.tmp <- stats::cov(comp.mice[[i]])
    cov.mice.imp[[i]] <- cov.tmp
  }

  # Combine estimated covariance from different imputations
  cov.mice <- Reduce("+", cov.mice.imp) / M

  # Eigenvalues of combined covariance matrix
  eig.cov.mice <- eigen(cov.mice)$values

  # proportion of explained variance for n.factor factors
  exp.var.mice <- (cumsum(eig.cov.mice) / sum(eig.cov.mice))[n.factor]

  out <- list(
      cov.mice          = cov.mice,
      cov.mice.imp      = cov.mice.imp,
      exp.var.mice      = exp.var.mice,
      ci.mice.fieller   = NULL,
      ci.mice.bootstrap = NULL
  )

  if (ci == TRUE) {
    out$ci.mice.fieller <- try(ci.mifa.fieller(cov.mice.imp, n.factor, alpha, N))
    out$ci.mice.bootstrap <- try(ci.mifa.bootstrap(
      data.miss, n.factor, rep.boot, method.mi, maxit.mi, alpha
    ))
  }

  return(out)
}
