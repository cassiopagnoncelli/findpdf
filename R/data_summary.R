#' Summarize dataset properties
#'
#' Analyzes a numeric vector to determine completeness, discreteness, and domain.
#' This is an internal function used by dtools.
#'
#' @param x Numeric vector to summarize
#' @return List with components:
#'   \describe{
#'     \item{is_complete}{Logical; TRUE if no NA/NaN values}
#'     \item{is_discrete}{Logical; TRUE if all values are integers}
#'     \item{domain}{Character; one of "NEGATIVE", "NONPOSITIVE", "NONNEGATIVE",
#'                   "POSITIVE", "REAL", or "NOT_IDENTIFIED"}
#'   }
#' @importFrom stats complete.cases
#' @keywords internal
#' @noRd
data_summary <- function(x) {
  if (length(x) == 0) {
    stop("Input must be a non-empty numeric vector.")
  }
  if (sum(!is.numeric(x)) > 0 || sum(is.infinite(x)) > 0) {
    stop("Input must be a numeric vector without infinite values.")
  }
  
  # Completeness - check before filtering
  is_complete <- !any(is.na(x) | is.nan(x))
  
  # Filter out NA/NaN for further analysis
  x <- x[complete.cases(x)]
  
  if (length(x) == 0) {
    stop("Input contains only NA/NaN values.")
  }

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
    "NEGATIVE",        # 1: only negative
    "NONNEGATIVE",     # 2: only zero (0 is nonnegative)
    "NONPOSITIVE",     # 3: negative + zero
    "POSITIVE",        # 4: only positive
    "REAL",            # 5: negative + positive
    "NONNEGATIVE",     # 6: zero + positive
    "REAL"             # 7: all three
  )

  # Return
  list(
    is_complete = is_complete,
    is_discrete = is_discrete,
    domain = domain
  )
}
