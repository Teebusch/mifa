#' Compute bootstrap confidence interval for proportion of explained variance
#'
#' This function computes a bootstrap confidence interval for proportion of
#' explained variance for the covariance of an incomplete data imputed using
#' multiple imputation with MICE.
#' This function uses the Shao and Sitter (1996) method to combine multiple
#' imputation and bootstrapping. The imputations are done using package mice.
#'
#' @references Shao, Jun, and Randy R. Sitter. Bootstrap for imputed survey
#' data. Journal of the American Statistical Association 91.435 (1996):
#' 1278-1288.
#'
#' @inheritParams mifa.cov
#'
#' @seealso mifa.cov
#'
#' @return
#' @export
#'
#' @examples
ci.mifa.bootstrap <- function(data.miss, n.factor, rep.boot = 1000,
                              method.mi, maxit.mi, alpha) {
  N <- dim(data.miss)[1]
  boot.eig <- matrix(0, dim(data.miss)[2], rep.boot)

  for (i in 1:rep.boot) {
    boot.data.idx <- sample(1:N, N, replace = T)
    boot.data <- data.miss[boot.data.idx, ]

    # impute it once
    boot.imp <- try(mice::mice(boot.data, m = 1, maxit = maxit.mi, method = method.mi, print = FALSE))

    # check if everything is imputed
    method.levels.mi <- levels(boot.imp$loggedEvents$meth)
    while ("constant" %in% method.levels.mi) {
      boot.data.idx <- sample(1:N, N, replace = T)
      boot.data <- data.miss[boot.data.idx, ]
      # impute it once
      boot.imp <- try(mice::mice(boot.data, m = 1, maxit = maxit.mi,
                           method = method.mi, print = FALSE))
      # check if everything is imputed
      method.levels.mi <- levels(boot.imp$loggedEvents$meth)
    }

    # extracting imputed datasets
    comp.mice <- mice::complete(boot.imp, 1)
    mi.na <- sum(is.na(comp.mice))

    # implementing sequential imputations in case that some of the columns are not
    # imputed due to collinearity, etc.
    while (sum(mi.na) > 0) {
      imp.tmp <- mice::mice(comp.mice, m = 1, maxit = maxit.mi,
                      method = method.mi, print = FALSE)
      comp.mice <- mice::complete(imp.tmp, 1)
      mi.na <- sum(is.na(comp.mice))
    }

    boot.cov.tmp <- cov(comp.mice)

    # compute the covariance matrix and its explained variance
    boot.eig[, i] <- eigen(boot.cov.tmp)$values

    # boot.exp[i]=sum(boot.eig[1:n.factor])/sum(boot.eig)
    if (is.character(boot.imp) == TRUE) {
      i <- i - 1
    }
  }

  prop.exp     <- t(apply(boot.eig, 2, cumsum)) / apply(boot.eig, 2, sum)
  boot.exp     <- apply(prop.exp, 2, quantile,
                        probs = c(alpha / 2, (1 - alpha / 2)))
  boot.exp.all <- cbind(n.factor, t(boot.exp)[n.factor, ])

  return(boot.exp.all)
}



#' Fieller's confidence intervals for proportion of explained variance
#'
#' Computes parametric confidence intervals for proportion of explained
#' variance for given numbers of factors using Fieller's method.
#' Note that by setting ci = TRUE in mifa.cov, this confidence interval can be
#' computed as well.
#'
#' @references Fieller, Edgar C. "Some problems in interval estimation."
#' Journal of the Royal Statistical Society. Series B (Methodological) (1954):
#' 175-185.
#'
#' @param cov.mi A vector containing the numbers of factors that should be used
#' to compute the proportion of explained variance or construct confidence
#' intervals. The minimum length of this vector is 1 and its maximum length is
#' the number of items.List containing the estimated covariance matrix within
#' each imputed data. One can use the outcome of 'mifa.cov' with the name
#' `cov.mice.imp`.
#' @param N A scalar specifying sample size
#'
#' @inheritParams mifa.cov
#'
#' @seealso mifa.cov
#'
#' @return A matrix containing 100(1-alpha)% confidence intervals for n.factor
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



#' Find the Fieller interval for each k
#'
#' This function is loaded within ci.mifa.fieller and will compute
#' Fieller's confidence interval for each of the components of n.factor.
#' Note that one can directly use ci.mifa.fieller. This function will be called
#' in there for internal computations.
#'
#' @param eig.imp A matrix with each of its columns the eigenvalues of the
#' estimated covariance matrix for each imputed data.
#' @param M A scalar specifying number of multiple imputations.
#'
#' @inheritParams ci.mifa.fieller
#'
#' @return A vector of length 2, containing the lower and upper bounds of
#' estimated Fieller's interval.
#' @export
#'
#' @examples
ci.mi.each <- function(eig.imp, n.factor, alpha, N, M) {
  eig.imp2 <- eig.imp^2
  mi.cov   <- NULL

  for (i in 1:M) {
    mi.cov[[i]] <- (2 / N) * diag(eig.imp2[i, ])
  }

  # combining them into one single imputation
  mi.comb       <- combine.mi((eig.imp), mi.cov)
  cov.lambda    <- mi.comb$parm.cov
  P             <- dim(cov.lambda)[1]
  A1            <- matrix(c(rep(1, n.factor), rep(0, P - n.factor)), 1, P)
  A             <- matrix(1, 1, P)
  sigma11.comb  <- (A1 %*% cov.lambda) %*% t(A1)
  sigma22.comb  <- (A %*% cov.lambda) %*% t(A)
  sigma12.comb  <- (A1 %*% cov.lambda) %*% t(A)
  var.prop.comb <- sum(mi.comb$parm.est[1:n.factor]) / sum(mi.comb$parm.est)

  # computing Fieller's intervals
  S           <- matrix(c(sigma11.comb, sigma12.comb, sigma12.comb, sigma22.comb), 2, 2)
  eigen.all   <- sum(mi.comb$parm.est)
  eigen.first <- sum(mi.comb$parm.est[1:n.factor])
  s11         <- S[1, 1]
  s22         <- S[2, 2]
  s12         <- S[1, 2]
  C12         <- s11 / (eigen.first^2)
  C22         <- s22 / (eigen.all^2)
  r           <- s12 / sqrt(s11 * s22)
  A           <- C12 + C22 - (2 * r * sqrt(C12 * C22))
  T.crit      <- qnorm(1 - (alpha / 2))
  B           <- (T.crit^2) * C12 * C22 * (1 - (r^2))

  if ((A - B) < 0) {
    stop("Computing Fieller CI is not possible, use a larger number of imputations.")
  }

  fieller1 <- eigen.first / eigen.all
  fieller2 <- 1 - (T.crit^2 * r * sqrt(C12 * C22))
  fieller3 <- T.crit * sqrt(A - B)
  fieller4 <- 1 - ((T.crit^2) * C22)

  fieller.low <- fieller1 * ((fieller2 - fieller3) / fieller4)
  fieller.up  <- fieller1 * ((fieller2 + fieller3) / fieller4)

  return(c(fieller.low, fieller.up))
}
