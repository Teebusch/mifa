#' Imputes a single dataset using mice
#'
#' @inheritParams mifa
#'
#' @return A data frame with missing values imputed.
#' @keywords internal
mice_impute_once <- function(data, ...) {
  args <- list(...)
  args$m <- 1
  args$data <- data
  imp <- do.call(mice::mice, args)
}


#' Sequential Imputation
#'
#' Sequential imputations in case that some of the columns are
#' not imputed due to collinearity, etc.
#'
#' This function will keep imputing the same data over and over
#' until all NA's are gone.
#'
#' @inheritParams mifa
#'
#' @return A data frame without NAs
#' @keywords internal
mice_impute_all_NA <- function(data, ...) {
  if(any(is.na(data))) {
    message(paste("There are", sum(is.na(data)), "NAs left in the data after",
                  "using MICE. Mifa will try to use sequential MICE to impute",
                  "them..."))
  }
  while (any(is.na(data))) {
    imp <- mice_impute_once(data, ...)
    data <- mice::complete(imp)
  }
  data
}


#' A wrapper for a call to mice that throws an error if there are
#' constants in the data
#'
#' @param f a call to [mice::mice()]
#' @return The result of calling the provided function or an Error if there is
#' a constant in one of the columns.
#' @keywords internal
stop_constants <- function(f) {
  imp <- eval(f)
  if ("constant" %in% levels(imp$loggedEvents$meth)) {
    stop("At least one column with constant observed part.")
  }
  imp
}

