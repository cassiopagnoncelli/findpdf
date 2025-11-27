#' Cap distribution values at specified quantiles
#'
#' @param x Numeric vector to cap
#' @param quantiles Numeric vector of length 2 with lower and upper quantile thresholds (default: c(0.001, 0.999))
#' @param policy String specifying the policy for handling outliers: "replace" (default) replaces outliers with quantile bounds, "remove" removes outliers from the dataset
#' @return Numeric vector with values either capped at or filtered by the specified quantiles, depending on the policy
#' @export
#' @examples
#' # Replace outliers with quantile bounds
#' cap(c(1:100, 1000), quantiles = c(0.05, 0.95), policy = "replace")
#'
#' # Remove outliers from the dataset
#' cap(c(1:100, 1000), quantiles = c(0.05, 0.95), policy = "remove")
cap <- function(x, quantiles = c(0.001, 0.999), policy = c("replace", "remove")) {
  policy <- match.arg(policy)

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

  if (policy == "replace") {
    x[x < qs[1]] <- qs[1]
    x[x > qs[2]] <- qs[2]
    return(x)
  } else { # policy == "remove"
    # Remove values outside the quantile bounds
    # Keep NA values in the result
    mask <- (x >= qs[1] & x <= qs[2]) | is.na(x)
    return(x[mask])
  }
}
