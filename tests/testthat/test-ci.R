data_bfi <- psych::bfi[, 1:25]


test_that("ci.mifa.bootstrap() returns right shape and type", {

  n.fct <- 3:5
  res <- ci.mifa.bootstrap(data_bfi, n.factor = n.fct, rep.boot = 3,
                           method.mi = "pmm", maxit.mi = 1, alpha = .05)

  expect_type(res, "double")
  expect_equal(dim(res), c(3, 3))
})


test_that("ci.mifa.fieller() returns right shape and type", {
  n.fct <- 3:5
  m <- 2
  N <- nrow(data_bfi)

  res.cov <- mifa.cov(data_bfi, n.factor = 2, M = m, maxit.mi = 2,
                      method.mi = "pmm")

  res <- ci.mifa.fieller(res.cov$cov.mice.imp, n.factor = n.fct,
                         alpha = .05, N = N)

  expect_type(res, "double")
  expect_equal(dim(res), c(3, 3))
})
