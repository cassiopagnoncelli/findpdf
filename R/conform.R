#' Conform numeric vector to valid range
#'
#' Replaces NaN, Inf, -Inf, and extreme values with NA. Fast vectorized operation.
#'
#' @param x Numeric vector to conform
#' @param limit Absolute value limit (default: 1e15). Large values become NA.
#' @return Numeric vector with invalid values replaced by NA
conform <- function(x, limit = 1e5, na = NA_real_) {
  # Fast vectorized replacement: NaN, Inf, -Inf, and extreme values all become NA
  x[is.nan(x) | is.infinite(x) | abs(x) > limit] <- na
  x
}
