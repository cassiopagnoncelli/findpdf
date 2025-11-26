#' Find best-fitting probability distributions
#'
#' Identifies the probability distributions that best fit the given data by
#' testing candidates and ranking them by fitting error.
#'
#' @param x Numeric vector; the dataset to analyze
#' @param include.exotics Logical; include exotic distributions (default: FALSE)
#' @param remove.na Logical; remove NA values before fitting (default: TRUE)
#' @param search.combinations Logical; search parameter combinations (default: TRUE)
#' @return An S3 object of class 'findpdf_result' with components:
#'   \describe{
#'     \item{params}{Named list of optimal parameters for each distribution}
#'     \item{ranking}{Data frame with columns 'pf' (function name) and 'error' (RMSE),
#'                    sorted by best fit}
#'     \item{data_summary}{Summary statistics of the input data}
#'     \item{pdf}{Function; PDF/PMF of best-fitting distribution}
#'     \item{cdf}{Function; CDF of best-fitting distribution}
#'     \item{best_fit}{Character; name of best-fitting distribution}
#'     \item{elapsed_time}{Numeric; total optimization time in seconds}
#'   }
#' @importFrom stats density
#' @importFrom compiler cmpfun
#' @importFrom pso psoptim
#' @examples
#' \dontrun{
#' result <- findpdf(rnorm(100, mean = 5, sd = 2))
#' print(result) # Pretty-printed output
#' result$params$dnorm # Access fitted normal parameters
#' result$ranking # View full ranking table
#' result$pdf(5) # Evaluate PDF at x=5
#' result$cdf(5) # Evaluate CDF at x=5
#' }
#' @export
findpdf <- function(x, include.exotics = FALSE, remove.na = TRUE, search.combinations = TRUE) {
  # Start timing
  start_time <- Sys.time()
  
  # Handle NA removal
  if (remove.na) {
    x <- x[!is.na(x)]
  } else {
    if (any(is.na(x))) {
      stop("'x' contains missing values")
    }
  }
  
  # Key data information.
  ds <- data_summary(x)

  # List pmf/pdf candidates.
  candidates <- pfDB[pfDB$discrete == ds$is_discrete, ]
  if (!include.exotics) {
    candidates <- candidates[candidates$exotic == FALSE, ]
  }

  # Estimate probability density/mass function.
  if (ds$is_discrete) {
    # Use table() for discrete data - faster and handles arbitrary values
    freq_table <- table(x)
    f.x <- as.numeric(names(freq_table))
    f.y <- as.numeric(freq_table) / length(x)  # Probability mass, not density
  } else {
    f <- density(x, from = min(x), to = max(x))
    f.x <- f$x
    f.y <- f$y
  }

  # Perform optimization across candidates
  ranking <- data.frame(pf = c(), error = c())
  best_params <- list()
  for (i in seq_along(candidates)) {
    candidate <- candidates[i, ]

    params_meta <- pfParamsDB[[as.character(candidate$pf)]]

    conv.params <- cmpfun(function(params) {
      b <- as.logical(params_meta$discrete)
      for (i in 1:length(params)) {
        if (b[i]) {
          params[i] <- round(params[i])
        }
      }

      return(params)
    })

    rmse <- cmpfun(function(params) {
      tryCatch({
        result <- suppressWarnings(do.call(
          as.character(candidate$pf),
          append(list(f.x), as.list(conv.params(params)))
        ))
        # Check for non-finite values
        if (any(!is.finite(result))) {
          return(1e10)
        }
        error <- sqrt(sum((result - f.y)^2))
        # Return large penalty if error is not finite
        if (!is.finite(error)) {
          return(1e10)
        }
        return(error)
      }, error = function(e) {
        # Return large penalty on any error
        return(1e10)
      })
    })

    # Use PSO for all optimization cases
    # Convert infinite bounds to large finite values for PSO
    lower_bounds <- params_meta$min
    upper_bounds <- params_meta$max
    
    # Replace -Inf with large negative value
    lower_bounds[is.infinite(lower_bounds) & lower_bounds < 0] <- -1e6
    # Replace +Inf with large positive value
    upper_bounds[is.infinite(upper_bounds) & upper_bounds > 0] <- 1e6
    
    o <- psoptim(params_meta$initial, rmse,
      lower = lower_bounds, upper = upper_bounds,
      control = list(
        maxit = 150,
        s = 30,
        abstol = 1e-8,
        reltol = 1e-6,
        trace = 0,
        REPORT = 1
      )
    )

    ranking <- rbind(ranking, data.frame(pf = candidate$pf, error = o$value))
    best_params[[as.character(candidate$pf)]] <- conv.params(o$par)
  }

  ranking <- ranking[order(ranking$error), ]

  # Get best-fit distribution and create pdf/cdf functions
  best_dist <- as.character(ranking$pf[1])
  best_params_vals <- best_params[[best_dist]]

  # Create pdf function for best fit
  pdf_func <- function(x) {
    do.call(best_dist, c(list(x), as.list(best_params_vals)))
  }

  # Create cdf function for best fit (replace 'd' with 'p')
  cdf_name <- sub("^d", "p", best_dist)
  cdf_func <- function(x) {
    do.call(cdf_name, c(list(x), as.list(best_params_vals)))
  }

  # Calculate elapsed time
  end_time <- Sys.time()
  elapsed_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  result <- list(
    params = best_params,
    ranking = ranking,
    data_summary = ds,
    pdf = pdf_func,
    cdf = cdf_func,
    best_fit = best_dist,
    elapsed_time = elapsed_time
  )
  class(result) <- "findpdf_result"

  result
}

#' Print method for findpdf results
#'
#' @param x A findpdf_result object
#' @param n Maximum number of distributions to display (default: 10)
#' @param ... Additional arguments (unused)
#' @export
print.findpdf_result <- function(x, n = 10, ...) {
  cat("\nBest-Fitting Probability Distributions\n")
  cat("=======================================\n\n")

  cat("Data Summary:\n")
  cat(sprintf("  Type: %s\n", ifelse(x$data_summary$is_discrete, "Discrete", "Continuous")))
  cat(sprintf("  N: %d\n", x$data_summary$n))
  cat(sprintf("  Range: [%.4f, %.4f]\n", x$data_summary$min, x$data_summary$max))
  cat(sprintf("  Mean: %.4f, SD: %.4f\n", x$data_summary$mean, x$data_summary$sd))
  
  if (!is.null(x$elapsed_time)) {
    cat(sprintf("  Elapsed Time: %.3f seconds\n", x$elapsed_time))
  }
  cat("\n")

  cat("Top Distributions (by RMSE):\n")
  n_show <- min(n, nrow(x$ranking))

  for (i in 1:n_show) {
    pf_name <- as.character(x$ranking$pf[i])
    error <- x$ranking$error[i]
    params <- x$params[[pf_name]]

    cat(sprintf("%2d. %s (RMSE: %.6f)\n", i, pf_name, error))

    if (length(params) > 0) {
      param_str <- paste(sprintf("%.4f", params), collapse = ", ")
      cat(sprintf("    Parameters: [%s]\n", param_str))
    }
  }

  if (nrow(x$ranking) > n) {
    cat(sprintf("\n... and %d more distributions\n", nrow(x$ranking) - n))
  }

  cat("\nBest-fit functions: $pdf(x), $cdf(x)\n")
  cat("Access components: $params, $ranking, $data_summary, $best_fit, $elapsed_time\n")
  invisible(x)
}
