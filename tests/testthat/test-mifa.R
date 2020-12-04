# The bfi data from the psych package contains answers to 5 personality items
# and 3 additional demographics for 2800 participants
# The 25 columns with responses are already classified into 5 personality
# factors, as indicated by their names:
# A (Agreeableness), C (Conscientiousness), E (Extraversion), N (Neuroticism),
# O (Openness)
# The data contains some missing values

data_bfi <- psych::bfi[, 1:25]

test_that("mifa() returns have the expected type and shape", {
  m <- 2
  c <- ncol(data_bfi)
  res <- mifa(data_bfi, m = m, print = FALSE)

  expect_type(res, "list")

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
  expect_length(res$var_explained$n_factors, c)
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
  res <- mifa(data_bfi, n_factors = 3:5, ci = "both", n_boot = 2, m = m,
              maxit = 2, print = FALSE)
  cis <- res$var_explained


  expect_type(res, "list")
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



