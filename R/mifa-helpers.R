#' Check and clean n_pc argument
#'
#' @param n_pc User-supplied n_pc argument. An integer vector.
#' @param n_cov_vars Number of variables in the covariance matrix
#'
#' @return Cleaned n_pc. An integer vector. Sorted, if applicable.
#' @export
#'
#' @keywords internal
clean_n_pc <- function(n_pc, n_cov_vars) {
  if (missing(n_pc)) {
    n_pc <- 1:n_cov_vars
  } else {
    checkmate::assert_integerish(n_pc,
      lower = 1, upper = n_cov_vars, unique = TRUE,
      any.missing = FALSE
    )
    n_pc <- sort(n_pc)
  }
  n_pc
}
