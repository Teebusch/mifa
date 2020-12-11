#' Bootstrap confidence intervals for proportion of explained variance
#'
#' Compute bootstrap confidence intervals for the proportion of explained
#' variance for the covariance of an incomplete data imputed using
#' multiple imputation.
#' For multiple imputation, Multivariate Imputation by Chained Equations
#' (MICE) from the [mice] package is used.
#'
#' This function uses the Shao and Sitter (1996) method to combine multiple
#' imputation and bootstrapping. The imputations are done using [mice::mice()].
#'
#' @references
#' Shao, J. & Sitter, R. R. (1996). Bootstrap for imputed survey data.
#' Journal of the American Statistical Association 91.435 (1996): 1278-1288.
#' <https://dx.doi.org/10.1080/01621459.1996.10476997>
#'
#' @inheritParams mifa
#' @inheritDotParams mice::mice
#'
#' @seealso [mifa()], [mice::mice()]
#' @family {mifa confidence intervals}
#'
#' @return A data frame containing bootstrapped confidence intervals for
#' variance explained by different number of factors.
#' @export
mifa_ci_boot <- function(data, cov_vars = dplyr::everything(), n_factors,
                         conf = .95, n_boot = 1000, ...) {

  n_cov_vars <- ncol(dplyr::select(data, {{ cov_vars }}))

  if (missing(n_factors)) {
    n_factors <- 1:n_cov_vars
  }

  boot_eig <- matrix(0, n_cov_vars, n_boot)

  for (i in 1:n_boot) {
    # draw bootstrap samples and impute until there are no constants
    repeat {
      boot_idx <- sample(nrow(data), replace = TRUE)
      data_boot <- data[boot_idx, ]
      try({
        imp <- stop_constants(mice_impute_once(data_boot, ...))
        break
      })
    }

    # extract imputed dataset, make sure no NAs are left
    data_imp <- mice::complete(imp)
    data_imp <- mice_impute_all_NA(data_imp, ...)

    # Select variables for calculation of covariance matrix
    data_imp <- dplyr::select(data_imp, {{ cov_vars }})

    # eigenvalues of covariance matrix
    boot_eig[, i] <- eigen(stats::cov(data_imp))$values
  }

  # variance explained and confidence intervals
  var_expl  <- t(apply(boot_eig, 2, cumsum)) / apply(boot_eig, 2, sum)
  probs_ci  <- c((1-conf)/2, 1-(1-conf)/2)
  boot_expl <- apply(var_expl, 2, stats::quantile, probs = probs_ci)
  boot_cis  <- t(boot_expl)[n_factors, ]

  data.frame(
    n_factors = n_factors,
    lower = boot_cis[, 1],
    upper = boot_cis[, 2]
  )
}



#' Fieller's confidence intervals for proportion of explained variance
#'
#' Computes parametric confidence intervals for proportion of explained
#' variance for given numbers of factors using Fieller's method.
#' Note that by setting `ci = TRUE` in [mifa()], this confidence
#' interval can be computed as well.
#'
#' @references
#' Fieller, E. C. (1954). Some problems in interval estimation.
#' Journal of the Royal Statistical Society. Series B (Methodological): 175-185.
#'
#' @param cov_imps List containing the estimated covariance matrix within
#' each imputed data. One can use `cov_imputations` returned by [mifa()].
#' @param N A scalar specifying sample size.
#'
#' @inheritParams mifa
#'
#' @seealso [mifa()]
#' @family {mifa confidence intervals}
#'
#' @return A data frame containing confidence intervals for `n_factors` factors
#' @export
mifa_ci_fieller <- function(cov_imps, n_factors, conf = .95, N) {

  m         <- length(cov_imps)
  eig_imp   <- matrix(0, m, dim(cov_imps[[1]])[1])

  for (i in 1:m) {
    eig_imp[i, ] <- eigen(cov_imps[[i]])$values
  }

  # compute Fieller confidence intervals for each number of factors
  ci_fieller <- matrix(NA, length(n_factors), 2)
  for (i in seq_along(n_factors)) {
    try({
      ci_fieller[i, ] <- get_fieller_ci(eig_imp, n_factors[i], conf, N, m)
    })
  }

  out <- data.frame(cbind(n_factors, ci_fieller))
  colnames(out) <- c("n_factors", "lower", "upper")
  return(out)
}



#' Find the Fieller interval for each k
#'
#' This function is used by [mifa_ci_fieller()] to compute Fieller's confidence
#' intervals for each of the components of n_factors.
#'
#' @param eig_imp A matrix with each of its columns the eigenvalues of the
#' estimated covariance matrix for each imputed data.
#' @param m A scalar specifying number of multiple imputations.
#'
#' @inheritParams mifa_ci_fieller
#'
#' @return A vector of length 2, containing the lower and upper bounds of
#' estimated Fieller's interval.
#' @keywords internal
get_fieller_ci <- function(eig_imp, n_factors, conf, N, m) {

  # combine imputations
  eig_imp2     <- eig_imp^2
  mi_cov       <- lapply(1:m, function(i) (2 / N) * diag(eig_imp2[i, ]))

  mi_comb      <- combine_rubin(eig_imp, mi_cov)
  cov_lambda   <- mi_comb$cov_param

  # compute Fieller's intervals
  P         <- dim(cov_lambda)[1]
  A1        <- matrix(c(rep(1, n_factors), rep(0, P - n_factors)), 1, P)
  A         <- matrix(1, 1, P)
  s11_comb  <- (A1 %*% cov_lambda) %*% t(A1)
  s22_comb  <- (A %*% cov_lambda) %*% t(A)
  s12_comb  <- (A1 %*% cov_lambda) %*% t(A)
  S         <- matrix(c(s11_comb, s12_comb, s12_comb, s22_comb), 2, 2)
  s11       <- S[1, 1]
  s22       <- S[2, 2]
  s12       <- S[1, 2]

  eigen.all   <- sum(mi_comb$param_est)
  eigen.first <- sum(mi_comb$param_est[1:n_factors])

  C12       <- s11 / (eigen.first^2)
  C22       <- s22 / (eigen.all^2)
  r         <- s12 / sqrt(s11 * s22)
  T.crit    <- stats::qnorm(1 - ((1-conf) / 2))
  A         <- C12 + C22 - (2 * r * sqrt(C12 * C22))
  B         <- (T.crit^2) * C12 * C22 * (1 - (r^2))

  if ((A - B) < 0) {
    stop("Computing Fieller CI is not possible. Try using more imputations.")
  }

  fieller1 <- eigen.first / eigen.all
  fieller2 <- 1 - (T.crit^2 * r * sqrt(C12 * C22))
  fieller3 <- T.crit * sqrt(A - B)
  fieller4 <- 1 - ((T.crit^2) * C22)

  c(
    lower = fieller1 * ((fieller2 - fieller3) / fieller4),
    upper = fieller1 * ((fieller2 + fieller3) / fieller4)
  )
}
