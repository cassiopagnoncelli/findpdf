#' Cap distribution values at specified quantiles
#'
#' @param x Numeric vector to cap
#' @param quantiles Numeric vector of length 2 with lower and upper quantile thresholds (default: c(0.001, 0.999))
#' @return Numeric vector with values capped at the specified quantiles
#' @examples
#' cap_distribution(c(1:100, 1000), quantiles = c(0.05, 0.95))
cap_distribution <- function(x, quantiles = c(0.001, 0.999)) {
  if (length(quantiles) != 2) {
    stop("quantiles must be a vector of length 2")
  }
  if (any(quantiles < 0) || any(quantiles > 1)) {
    stop("quantiles must be between 0 and 1")
  }
  if (quantiles[1] >= quantiles[2]) {
    stop("the first quantile must be less than the second quantile")
  }
  qs <- quantile(x, probs = quantiles, na.rm = TRUE)
  x[x < qs[1]] <- qs[1]
  x[x > qs[2]] <- qs[2]
  return(x)
}
