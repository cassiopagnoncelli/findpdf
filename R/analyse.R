#' Analyse distribution statistics by groups
#'
#' @param data Numeric vector or data frame with numeric column
#' @param groups Numeric vector of threshold values to split data into groups (default: c(0))
#' @param extreme_threshold Threshold for identifying extreme values (default: 1e15)
#' @return List with overall_results (summary statistics and special value counts) and group_results (statistics per group)
#' @importFrom stats quantile sd
#' @export
#' @examples
#' analyse(rnorm(1000), groups = c(-1, 0, 1))
analyse <- function(data, groups = c(0), extreme_threshold = 1e15) {
  # Handle different input types
  if (is.vector(data) || is.atomic(data)) {
    # If data is a vector, convert to tibble
    data <- tibble::tibble(value = data)
  } else if (is.data.frame(data)) {
    # If data is a tibble/data.frame, find first numeric column
    numeric_cols <- sapply(data, is.numeric)
    if (!any(numeric_cols)) {
      stop("No numeric columns found in the data")
    }
    first_numeric <- names(data)[which(numeric_cols)[1]]
    data <- tibble::tibble(value = data[[first_numeric]])
  }

  # Count special values before processing
  na_count <- sum(is.na(data$value))
  inf_count <- sum(is.infinite(data$value), na.rm = TRUE)
  nan_count <- sum(is.nan(data$value))
  extreme_count <- sum(abs(data$value) > extreme_threshold, na.rm = TRUE)

  # Sort groups to ensure proper ordering
  groups <- sort(groups)

  # Create group labels and split data
  n_groups <- length(groups) + 1
  group_labels <- paste0("g", 1:n_groups)

  # Assign each value to a group
  data <- data |>
    dplyr::mutate(
      group = dplyr::case_when(
        value < groups[1] ~ group_labels[1],
        TRUE ~ group_labels[n_groups]
      )
    )

  # Handle intermediate groups if there are multiple thresholds
  if (length(groups) > 1) {
    for (i in 2:length(groups)) {
      data <- data |>
        dplyr::mutate(
          group = dplyr::if_else(
            value >= groups[i - 1] & value < groups[i],
            group_labels[i],
            group
          )
        )
    }
  }

  total_n <- nrow(data)
  group_results <- data |>
    dplyr::group_by(group) |>
    dplyr::summarise(
      count = dplyr::n(),
      prob = dplyr::n() / total_n,
      mean = mean(value, na.rm = TRUE),
      expected = prob * mean,
      sd = sd(value, na.rm = TRUE),
      min = quantile(value, probs = 0, na.rm = TRUE),
      q_0.05 = quantile(value, probs = 0.05, na.rm = TRUE),
      q_0.32 = quantile(value, probs = 0.32, na.rm = TRUE),
      median = quantile(value, probs = 0.5, na.rm = TRUE),
      q_0.68 = quantile(value, probs = 0.68, na.rm = TRUE),
      q_0.95 = quantile(value, probs = 0.95, na.rm = TRUE),
      max = quantile(value, probs = 1, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(group = factor(group, levels = group_labels)) |>
    dplyr::arrange(group)

  # Calculate dataset metrics
  overall_results <- data |>
    dplyr::summarise(
      expected_value = group_results$expected |> sum(na.rm = TRUE),
      mean = mean(value, na.rm = TRUE),
      median = quantile(value, probs = 0.5, na.rm = TRUE),
      sd = sd(value, na.rm = TRUE),
      n = nrow(data),
      NA_count = na_count,
      Inf_count = inf_count,
      NaN_count = nan_count,
      extreme_count = extreme_count
    )

  return(
    list(
      overall_results = overall_results,
      group_results = group_results
    )
  )
}
