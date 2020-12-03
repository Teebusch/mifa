#' Fieller's confidence intervals for proportion of explained variance
#'
#' Computes parametric confidence intervals for proportion of explained
#' variance using Fieller's method.
#'
#' @param cov.mi List containing the estimated covariance matrix within each
#' imputed data. One can use the outcome of 'mi.cov' with the name
#' `cov.mice.imp`.
#' @param n.factor a vector indicating the number of factors for which the
#' confidence interval should be constructed.
#' @param alpha the level of significance of the confidence interval.
#' @param N sample size
#'
#' @return A matrix contining 100(1-alpha)% confidence inervals for n.factor
#' factors.
#' @export
#'
#' @examples
ci.mifa.fieller <- function(cov.mi, n.factor, alpha, N) {
  n.items   <- dim(cov.mi[[1]])[1]
  N.factor  <- length(n.factor)
  M         <- length(cov.mi)
  eig.imp   <- matrix(0, M, dim(cov.mi[[1]])[1])

  for (i in 1:M) {
    eig.imp[i, ] <- eigen(cov.mi[[i]])$values
  }

  # computing two parameters of interest: sum of first n.factor
  # eigenvalues and sum of all P of them.
  fieller.ci <- matrix(0, N.factor, 2)

  for (i in 1:N.factor) {
    out.ci <- try(ci.mi.each(eig.imp, n.factor[i], alpha, N, M))
    if (is.character(out.ci) == FALSE) {
      fieller.ci[i, ] <- out.ci
    }
  }

  # End of Fieller's interval
  fieller.ci           <- cbind(n.factor, fieller.ci)
  colnames(fieller.ci) <- c("n.factor", "Lower", "Upper")

  return(fieller.ci)
}
