#' Bootstrap confidence intervals for explained variance
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
#' Normally, this function does not need to be called directly. Instead,
#' use `mifa(..., ci = "boot")`.
#'
#' @references
#' Shao, J. & Sitter, R. R. (1996). Bootstrap for imputed survey data.
#' Journal of the American Statistical Association 91.435 (1996): 1278-1288.
#' <https://dx.doi.org/10.1080/01621459.1996.10476997>
#'
#' @param progress Logical. Whether to show progress bars for computation of
#' bootstrap confidence intervals. Default is FALSE.
#' @inheritParams mifa
#' @inheritDotParams mice::mice
#'
#' @seealso [mifa()], [mice::mice()]
#' @family mifa confidence intervals
#'
#' @return A data frame containing bootstrapped confidence intervals for
#' variance explained by different number of principal components.
#' @export
#' @examples
#' \dontrun{
#' data <- psych::bfi[, 1:25]
#' mifa_ci_boot(data, n_pc = 3:8, n_boot = 50, print = FALSE)
#' }
mifa_ci_boot <- function(data, cov_vars = dplyr::everything(), n_pc,
                         conf = .95, n_boot = 1000, progress = FALSE, ...) {
  # check and clean arguments
  checkmate::assert_data_frame(data, min.rows = 1, min.cols = 2)
  checkmate::assert_data_frame(dplyr::select(data, {{ cov_vars }}), min.cols = 2)
  checkmate::assert_number(conf, lower = 0, upper = 1)
  checkmate::assert_count(n_boot, positive = TRUE)
  checkmate::assert_flag(progress)

  n_cov_vars <- ncol(dplyr::select(data, {{ cov_vars }}))
  n_pc <- clean_n_pc(n_pc, n_cov_vars)

  # bootstrap samples and compute variance explained by principal components
  # (i.e. eigenvalues of eigenvectors)
  boot_eig <- matrix(0, n_cov_vars, n_boot)

  if (progress) {
    cat("\n\n  Computing bootstrap confidence intervals...\n")
    pb <- utils::txtProgressBar(min = 0, max = n_boot, initial = 0, style = 3)
  }
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

    if (progress) utils::setTxtProgressBar(pb, i)
  }

  # get confidence intervals for variance explained by  principal components
  # (i.e., eigenvalues of eigenvectors)
  var_expl <- t(apply(boot_eig, 2, cumsum)) / apply(boot_eig, 2, sum)
  probs_ci <- c((1 - conf) / 2, 1 - (1 - conf) / 2)
  boot_expl <- apply(var_expl, 2, stats::quantile, probs = probs_ci)
  boot_cis <- t(boot_expl)[n_pc, ]

  data.frame(
    n_pc = n_pc,
    lower = boot_cis[, 1],
    upper = boot_cis[, 2]
  )
}



#' Fieller confidence intervals for explained variance
#'
#' Computes parametric confidence intervals for proportion of explained
#' variance for given numbers of principal components using Fieller's method.
#' Note that by setting `ci = TRUE` in [mifa()], this confidence
#' interval can be computed as well.
#'
#' Normally, this function does not need to be called directly. Instead,
#' use `mifa(..., ci = "fieller")`.
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
#' @family mifa confidence intervals
#'
#' @return A data frame containing confidence intervals for `n_pc` principal
#' components.
#' @export
#' @examples
#' \dontrun{
#' data <- psych::bfi[, 1:25]
#' mi <- mifa(data, print = FALSE)
#' mifa_ci_fieller(mi$cov_imputations, n_pc = 3:8, N = nrow(data))
#' }
mifa_ci_fieller <- function(cov_imps, n_pc, conf = .95, N) {
  # check and clean arguments
  checkmate::assert_list(cov_imps, "matrix", any.missing = FALSE, min.len = 1)
  lapply(cov_imps, function(x) {
    checkmate::assert_matrix(x, ncols = nrow(x), any.missing = FALSE)
  })
  checkmate::assert_number(conf, lower = 0, upper = 1)
  checkmate::assert_count(N, positive = TRUE)

  n_cov_vars <- ncol(cov_imps[[1]])
  n_pc <- clean_n_pc(n_pc, n_cov_vars)

  # get eigenvalues
  m <- length(cov_imps)
  eig_imp <- matrix(0, m, dim(cov_imps[[1]])[1])

  for (i in 1:m) {
    eig_imp[i, ] <- eigen(cov_imps[[i]])$values
  }

  # compute Fieller confidence intervals for each number of principal components
  ci_fieller <- matrix(NA, length(n_pc), 2)
  for (i in seq_along(n_pc)) {
    try({
      ci_fieller[i, ] <- get_fieller_ci(eig_imp, n_pc[i], conf, N, m)
    })
  }

  out <- data.frame(cbind(n_pc, ci_fieller))
  colnames(out) <- c("n_pc", "lower", "upper")
  return(out)
}



#' Find the Fieller interval for each k
#'
#' This function is used by [mifa_ci_fieller()] to compute Fieller's confidence
#' intervals for each of the components of n_pc.
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
get_fieller_ci <- function(eig_imp, n_pc, conf, N, m) {

  # combine imputations
  eig_imp2 <- eig_imp^2
  mi_cov <- lapply(1:m, function(i) (2 / N) * diag(eig_imp2[i, ]))

  mi_comb <- combine_rubin(eig_imp, mi_cov)
  cov_lambda <- mi_comb$cov_param

  # compute Fieller's intervals
  P <- dim(cov_lambda)[1]
  A1 <- matrix(c(rep(1, n_pc), rep(0, P - n_pc)), 1, P)
  A <- matrix(1, 1, P)
  s11_comb <- (A1 %*% cov_lambda) %*% t(A1)
  s22_comb <- (A %*% cov_lambda) %*% t(A)
  s12_comb <- (A1 %*% cov_lambda) %*% t(A)
  S <- matrix(c(s11_comb, s12_comb, s12_comb, s22_comb), 2, 2)
  s11 <- S[1, 1]
  s22 <- S[2, 2]
  s12 <- S[1, 2]

  eigen.all <- sum(mi_comb$param_est)
  eigen.first <- sum(mi_comb$param_est[1:n_pc])

  C12 <- s11 / (eigen.first^2)
  C22 <- s22 / (eigen.all^2)
  r <- s12 / sqrt(s11 * s22)
  T.crit <- stats::qnorm(1 - ((1 - conf) / 2))
  A <- C12 + C22 - (2 * r * sqrt(C12 * C22))
  B <- (T.crit^2) * C12 * C22 * (1 - (r^2))

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

  est_diff <- scale(param_imps, center = TRUE, scale = FALSE)
  cov_param_sample1 <- lapply(1:m, function(i) est_diff[i, ] %*% t(est_diff[i, ]))
  cov_param_sample1 <- Reduce("+", cov_param_sample1) / (m - 1)

  cov_param_sample <- cov_param_sample1 * ((m + 1) / m)
  cov_param_mean <- Reduce(`+`, cov_imps) / m
  cov_param <- cov_param_mean + cov_param_sample

  list(
    param_est = apply(param_imps, 2, mean),
    cov_param = cov_param,
    cov_between = cov_param_sample1
  )
}
