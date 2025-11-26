#' Summarize dataset properties
#'
#' Analyzes a numeric vector to determine completeness, discreteness, and domain.
#'
#' @param x Numeric vector to summarize
#' @return List with components:
#'   \describe{
#'     \item{is_complete}{Logical; TRUE if no NA/NaN values}
#'     \item{is_discrete}{Logical; TRUE if all values are integers}
#'     \item{domain}{Character; one of "NEGATIVE", "NONPOSITIVE", "NONNEGATIVE",
#'                   "POSITIVE", "REAL", or "NOT_IDENTIFIED"}
#'   }
#' @examples
#' data_summary(c(1, 2, 3))
#' data_summary(c(-1.5, 0, 2.3))
data_summary <- function(x) {
  # Completeness
  is_complete <- sum(is.na(x)) == 0 & sum(is.nan(x)) == 0
  x <- x[!is.na(x) && !is.nan(x)]

  # Basic summary
  min_x <- min(x)
  max_x <- max(x)

  # Type
  is_discrete <- sum(x - as.integer(x)) == 0

  # Domain
  has_negative <- min_x < 0
  has_zero <- sum(x == 0) > 0
  has_positive <- max_x > 0
  domain <- switch((2^0) * has_negative + (2^1) * has_zero + (2^2) * has_positive,
    "NEGATIVE",
    "NOT_IDENTIFIED",
    "NONPOSITIVE",
    "POSITIVE",
    "REAL",
    "NONNEGATIVE",
    "REAL"
  )

  # Return
  list(
    is_complete = is_complete,
    is_discrete = is_discrete,
    domain = domain
  )
}
