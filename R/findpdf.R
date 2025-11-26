#' Find best-fitting probability distributions
#'
#' Identifies the probability distributions that best fit the given data by
#' testing candidates and ranking them by fitting error.
#'
#' @param x Numeric vector; the dataset to analyze
#' @param include.exotics Logical; include exotic distributions (default: FALSE)
#' @param remove.na Logical; remove NA values before fitting (default: TRUE)
#' @param search.combinations Logical; search parameter combinations (default: TRUE)
#' @return List with components:
#'   \describe{
#'     \item{params}{Named list of optimal parameters for each distribution}
#'     \item{ranking}{Data frame with columns 'pf' (function name) and 'error' (RMSE),
#'                    sorted by best fit}
#'   }
#' @examples
#' \dontrun{
#' result <- findpdf(rnorm(100))
#' head(result$ranking)  # View top distributions
#' result$params$dnorm   # Get fitted normal parameters
#' }
#' @export
findpdf <- function(x, include.exotics = FALSE, remove.na = TRUE, search.combinations = TRUE) {
  # Key data information.
  ds <- data_summary(x)

  # List pmf/pdf candidates.
  candidates <- pfDB[pfDB$discrete == ds$is_discrete, ]
  if (!include.exotics) {
    candidates <- candidates[candidates$exotic == FALSE, ]
  }

  # Estimate probability density/mass function.
  if (ds$is_discrete) { ################ FIXME ##############
    f <- hist(x, breaks = (min(x) - 0.5):(max(x) + 0.5), plot = F)
    f.x <- f$mids
    f.y <- f$density
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

    params_meta <- get(paste("pfParamsDB$", candidate$pf, sep = ""))

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
      sqrt(sum((do.call(
        as.character(candidate$pf),
        append(list(f.x), as.list(conv.params(params)))
      ) - f.y)^2))
    })

    if (sum(params_meta$discrete) > 0) {
      o <- GenSA(params_meta$initial, rmse,
        lower = params_meta$min, upper = params_meta$max,
        control = list(maxit = 3000)
      )
    } else {
      o <- optim(params_meta$initial, rmse,
        lower = params_meta$min, upper = params_meta$max, method = "L-BFGS-B",
        control = list()
      )
    }

    ranking <- rbind(ranking, data.frame(pf = candidate$pf, error = o$value))
    best_params[[as.character(candidate$pf)]] <- conv.params(o$par)

    if (o$counts[1] <= 2) {
      print(o)
    }
  }

  ranking <- ranking[order(ranking$error), ]

  list(
    params = best_params,
    ranking = ranking
  )
}
