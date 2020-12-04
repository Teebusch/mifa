data_bfi <- psych::bfi[, 1:25]


test_that("mifa_ci_boot() returns right shape and type", {

  n_fct <- 3:5
  res <- mifa_ci_boot(data_bfi, n_factors = n_fct, n_boot = 3, maxit = 1, print = F)

  expect_s3_class(res, "data.frame")
  expect_equal(res$n_factors, n_fct)
  expect_type(res$lower, "double")
  expect_type(res$upper, "double")
  expect_equal(dim(res), c(3, 3))
})


test_that("mifa_ci_fieller() returns right shape and type", {
  n_fct <- 3:5
  m <- 2

  res_mifa <- mifa(data_bfi, n_factors = 2, m = m, maxit = 2, print = F)

  res <- mifa_ci_fieller(res_mifa$cov_imputations, n_factor = n_fct,
                         N = nrow(data_bfi))

  expect_s3_class(res, "data.frame")
  expect_equal(res$n_factors, n_fct)
  expect_type(res$lower, "double")
  expect_type(res$upper, "double")
  expect_equal(dim(res), c(3, 3))
})
