# The bfi data from the psych package contains answers to 5 personality items
# and 3 additional demographics for 2800 participants
# The 25 columns with responses are already classified into 5 personality
# factors, as indicated by their names:
# A (Agreeableness), C (Conscientiousness), E (Extraversion), N (Neuroticism) ,
# O (Openness)
# The data contains some missing values

data_bfi <- psych::bfi[, 1:25]

test_that("mifa.cov() returns have the expected type and shape", {
  m <- 2
  c <- ncol(data_bfi)
  res <- mifa.cov(data_bfi, M = m)

  expect_type(res, "list")

  # cov.mice is a single matrix of type double
  expect_type(res$cov.mice, "double")
  expect_equal(dim(res$cov.mice), c(25, 25))

  # cov.mice.imp is a list of matrixes of type double
  expect_type(res$cov.mice.imp, "list")
  expect_true(all(sapply(res$cov.mice.imp, typeof) == "double"))
  expect_length(res$cov.mice.imp, m)
  expect_true(all(
    sapply(
      res$cov.mice.imp,
      function(x) all(dim(x) == c(c, c))
    )
  ))

  # exp.var.mice is a vector of doubles between 0 and 1
  expect_type(res$exp.var.mice, "double")
  expect_length(res$exp.var.mice, c)
  expect_true(all(res$exp.var.mice >= 0 & res$exp.var.mice <= 1))

  # CIs are not computed by default and return NULL
  expect_null(res$ci.mice.fieller)
  expect_null(res$ci.mice.bootstrap)
})


test_that("CIs returned by mifa.cov() have the expected type and shape", {
  m <- 2
  c <- ncol(data_bfi)
  res <- mifa.cov(data_bfi, M = m, n.factor = 3:5, maxit.mi = 2,
                  ci = TRUE, rep.boot = 2)

  expect_type(res, "list")

  expect_type(res$ci.mice.fieller, "double")
  expect_equal(dim(res$ci.mice.fieller), c(3, 3))

  expect_type(res$ci.mice.bootstrap, "double")
  expect_equal(dim(res$ci.mice.bootstrap), c(3, 3))
})



