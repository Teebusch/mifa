data_bfi <- psych::bfi[, 1:25]


test_that("combine.mi() return has the right shape and type", {

  # make some input of the expected shape
  data     <- na.omit(data_bfi)
  c        <- ncol(data)
  parm.est <- list()
  parm.cov <- list()

  for (i in 1:5) {
    parm.est[[i]] <- eigen(cov(data))$values
    parm.cov[[i]] <- eigen(cov(data))$vectors
  }

  parm.est <- data.frame(Reduce(rbind, parm.est))

  # tests
  res <- combine.mi(parm.est, parm.cov)

  expect_type(res, "list")
  expect_length(res, 3)

  expect_type(res$parm.est, "double")
  expect_length(res$parm.est, c)

  expect_type(res$parm.cov, "double")
  expect_equal(dim(res$parm.cov), c(c, c))

  expect_type(res$between.cov, "double")
  expect_equal(dim(res$between.cov), c(c, c))

})
