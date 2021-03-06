library(dplyr)

# The bfi data from the psych package contains answers to 5 personality items
# and 3 additional demographics for 2800 participants. The 25 columns with
# responses are already classified into 5 personality factors, as indicated by
# their names: A (Agreeableness), C (Conscientiousness), E (Extraversion),
# N (Neuroticism), O (Openness). The data contains some missing values
data_bfi <- psych::bfi[, 1:25]


test_that("mifa() returns have the expected type and shape", {
  m <- 2
  c <- ncol(data_bfi)
  res <- mifa(data_bfi, m = m, print = FALSE)

  expect_type(res, "list")
  expect_s3_class(res, "mifa")

  # cov_combined is a single matrix of type double
  expect_type(res$cov_combined, "double")
  expect_equal(dim(res$cov_combined), c(25, 25))

  # cov_imputations is a list of matrixes of type double
  expect_type(res$cov_imputations, "list")
  expect_true(all(sapply(res$cov_imputations, typeof) == "double"))
  expect_length(res$cov_imputations, m)
  expect_true(all(
    sapply(
      res$cov_imputations,
      function(x) all(dim(x) == c(c, c))
    )
  ))

  # var_explained is a data frame
  expect_s3_class(res$var_explained, "data.frame")
  expect_length(res$var_explained$n_pc, c)
  expect_true(all(res$var_explained$var_explained >= 0 &
    res$var_explained$var_explained <= 1))

  # CIs are not computed by default and return NULL
  expect_null(res$var_explained$ci_boot_lower)
  expect_null(res$var_explained$ci_boot_upper)
  expect_null(res$var_explained$ci_fieller_lower)
  expect_null(res$var_explained$ci_fieller_upper)
})


test_that("CIs returned by mifa() have the expected type and shape", {
  m <- 2
  c <- ncol(data_bfi)
  res <- mifa(data_bfi,
    n_pc = 3:5, ci = "both", n_boot = 2, m = m,
    maxit = 2, print = FALSE
  )
  cis <- res$var_explained


  expect_type(res, "list")
  expect_s3_class(res, "mifa")
  expect_s3_class(cis, "data.frame")

  expect_type(cis$ci_boot_lower, "double")
  expect_type(cis$ci_boot_upper, "double")
  expect_type(cis$ci_fieller_lower, "double")
  expect_type(cis$ci_fieller_upper, "double")

  expect_length(cis$ci_boot_lower, 3)
  expect_length(cis$ci_boot_upper, 3)
  expect_length(cis$ci_fieller_lower, 3)
  expect_length(cis$ci_fieller_upper, 3)
})


test_that("cov_vars argument removes variables", {
  m <- 2
  c <- ncol(data_bfi)
  c_sel <- ncol(dplyr::select(data_bfi, starts_with("O")))

  res <- mifa(data_bfi, cov_var = starts_with("O"), m = m, print = FALSE)

  expect_true(is.matrix(res$cov_combined))
  expect_type(res$cov_combined, "double")
  expect_length(res$cov_combined, c_sel^2)
  expect_equal(dim(res$cov_combined), c(c_sel, c_sel))
})


test_that("output is equivalent to that of original implementation", {
  # the essence of the original mifa algorithm, without cis
  mifa_old <- function(data, n.factor, M, maxit.mi = 5, method.mi = "pmm") {
    N <- dim(data)[1]
    imputed_mice <- mice::mice(data, m = M, maxit = maxit.mi,
                               method = method.mi, print = FALSE)
    method.levels.mi <- levels(imputed_mice$loggedEvents$meth)
    if ("constant" %in% method.levels.mi) {
      stop("Probably at least one column with constant observed part.")
    }
    comp.mice <- NULL
    mi.na <- rep(0, M)
    for (i in 1:M) {
      comp.mice[[i]] <- mice::complete(imputed_mice, i)
      mi.na[i] <- sum(is.na(comp.mice[[i]]))
    }
    while (sum(mi.na) > 0) {
      for (i in 1:M) {
        imp.tmp <- mice::mice(comp.mice[[i]], m = 1, maxit = maxit.mi,
                              method = method.mi, print = FALSE)
        comp.mice[[i]] <- mice::complete(imp.tmp, 1)
        mi.na[i] <- sum(is.na(comp.mice[[i]]))
      }
    }
    cov.mice.imp <- NULL
    prop.exp <- rep(0, M)
    for (i in 1:M) {
      cov.tmp <- cov(comp.mice[[i]])
      cov.mice.imp[[i]] <- cov.tmp
    }
    cov.mice     <- Reduce("+", cov.mice.imp) / M
    eig.cov.mice <- eigen(cov.mice)$values
    exp.var.mice <- (cumsum(eig.cov.mice) / sum(eig.cov.mice))[n.factor]

    list(
      cov.mice     = cov.mice,
      cov.mice.imp = cov.mice.imp,
      exp.var.mice = exp.var.mice
    )
  }

  m <- 2
  maxit <- 3

  set.seed(111)
  old <- mifa_old(data_bfi, M = m, maxit.mi = maxit)
  # latest version
  set.seed(111)
  new <- mifa(data_bfi, m = 2, maxit = maxit, print = FALSE)

  expect_equal(old$cov.mice, new$cov_combined)
  expect_equal(old$exp.var.mice, new$var_explained$var_explained,
               tolerance = 0.0001)
})
