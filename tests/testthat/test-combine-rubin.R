data_bfi <- psych::bfi[, 1:25]


test_that("combine_rubin() return has the right shape and type", {

  # make some input of the expected shape
  data     <- na.omit(data_bfi)
  c        <- ncol(data)
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
