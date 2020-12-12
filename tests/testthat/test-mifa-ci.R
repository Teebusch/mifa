library(dplyr)

data_bfi <- psych::bfi[, 1:25]


test_that("mifa_ci_boot() returns right shape and type", {
  n_pc <- 3:5
  res <- mifa_ci_boot(data_bfi, n_pc = n_pc, n_boot = 3, maxit = 1, print = F)

  expect_s3_class(res, "data.frame")
  expect_equal(res$n_pc, n_pc)
  expect_type(res$lower, "double")
  expect_type(res$upper, "double")
  expect_equal(dim(res), c(3, 3))
})


test_that("using cov_vars argument produces different cis", {
  # can only test it indirectly, unfortunately
  m <- 2

  set.seed(123)
  res1 <- mifa_ci_boot(data_bfi, n_boot = 3, maxit = 1, print = F) # no selection

  set.seed(123) # use same seed for bootstrapping
  res2 <- mifa_ci_boot(data_bfi,
    cov_vars = starts_with("O"),
    n_boot = 3, maxit = 1, print = F
  ) # with selection

  expect_s3_class(res2, "data.frame")
  expect_false(identical(res1, res2))
  expect_equal(dim(res1), c(ncol(data_bfi), 3))
  expect_equal(dim(res2), c(ncol(dplyr::select(data_bfi, starts_with("O"))), 3))
})


test_that("mifa_ci_fieller() returns right shape and type", {
  n_pc <- 3:5
  m <- 2

  res_mifa <- mifa(data_bfi, n_pc = 2, m = m, maxit = 2, print = F)

  res <- mifa_ci_fieller(res_mifa$cov_imputations,
    n_pc = n_pc,
    N = nrow(data_bfi)
  )

  expect_s3_class(res, "data.frame")
  expect_equal(res$n_pc, n_pc)
  expect_type(res$lower, "double")
  expect_type(res$upper, "double")
  expect_equal(dim(res), c(3, 3))
})


test_that("combine_rubin() return has the right shape and type", {
  # make some input of the expected shape
  data <- na.omit(data_bfi)
  c <- ncol(data)
  param_imps <- list()
  cov_imps <- list()

  for (i in 1:5) {
    param_imps[[i]] <- eigen(cov(data))$values
    cov_imps[[i]] <- eigen(cov(data))$vectors
  }

  param_imps <- data.frame(Reduce(rbind, param_imps))

  # tests
  res <- combine_rubin(param_imps, cov_imps)

  expect_type(res, "list")
  expect_length(res, 3)

  expect_type(res$param_est, "double")
  expect_length(res$param_est, c)

  expect_type(res$cov_param, "double")
  expect_equal(dim(res$cov_param), c(c, c))

  expect_type(res$cov_between, "double")
  expect_equal(dim(res$cov_between), c(c, c))
})
