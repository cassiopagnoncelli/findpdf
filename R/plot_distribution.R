#' Plot distribution with histogram and density overlay
#'
#' @param data Numeric vector or data frame with numeric column
#' @param bins Number of histogram bins (default: NULL, uses Sturges' rule)
#' @param vline X-coordinate for vertical reference line (default: 0)
#' @param title Plot title (default: "Distribution")
#' @return ggplot2 object showing histogram with density curve and statistical markers
#' @examples
#' plot_distribution(rnorm(1000))
#' plot_distribution(data.frame(x = rnorm(500)), vline = 1, title = "My Distribution")
plot_distribution <- function(data, bins = NULL, vline = 0, title = "Distribution") {
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

  # Calculate number of bins using Sturges' rule if not specified
  if (is.null(bins)) {
    n <- nrow(data)
    bins <- ceiling(3.3 * log10(n) + 1)
  }

  # Calculate statistics for labels
  mean_val <- mean(data$value, na.rm = TRUE)
  sd_val <- sd(data$value, na.rm = TRUE)
  
  # Create histogram with density scale and smoothing curve
  ggplot2::ggplot(data, ggplot2::aes(x = value)) +
    ggplot2::geom_histogram(
      aes(y = ggplot2::after_stat(density)),
      bins = bins,
      fill = "#851d91",
      # color = "#676cce",
      alpha = 0.7
    ) +
    ggplot2::geom_density(
      aes(y = ggplot2::after_stat(density)),
      fill = "#1d9146",
      # color = "#2a965a",
      alpha = .55,
      linewidth = .2
    ) +
    ggplot2::geom_vline(
      xintercept = vline,
      color = "#ff0000",
      linetype = "solid",
      linewidth = .4,
      alpha = .85
    ) +
    ggplot2::geom_vline(
      xintercept = mean_val,
      color = "#000000",
      linetype = "dashed",
      linewidth = .8
    ) +
    ggplot2::geom_vline(
      xintercept = mean_val + sd_val,
      color = "#4e4e4e",
      linetype = "dashed",
      linewidth = .5
    ) +
    ggplot2::geom_vline(
      xintercept = mean_val - sd_val,
      color = "#4e4e4e",
      linetype = "dashed",
      linewidth = .5
    ) +
    ggplot2::geom_vline(
      xintercept = mean_val + 2 * sd_val,
      color = "#a3a3a3",
      linetype = "dashed",
      linewidth = .35
    ) +
    ggplot2::geom_vline(
      xintercept = mean_val - 2 * sd_val,
      color = "#a3a3a3",
      linetype = "dashed",
      linewidth = .35
    ) +
    ggplot2::annotate(
      "text", x = mean_val, y = Inf,
      label = sprintf("μ=%.2f", mean_val),
      vjust = 1.5, hjust = 0.5, size = 5, color = "#000000"
    ) +
    ggplot2::annotate(
      "text", x = mean_val + sd_val, y = Inf,
      label = sprintf("+1σ=%.2f", mean_val + sd_val),
      vjust = 1.5, hjust = 0.5, size = 4.5, color = "#4e4e4e"
    ) +
    ggplot2::annotate(
      "text", x = mean_val - sd_val, y = Inf,
      label = sprintf("-1σ=%.2f", mean_val - sd_val),
      vjust = 1.5, hjust = 0.5, size = 4.5, color = "#4e4e4e"
    ) +
    ggplot2::annotate(
      "text", x = mean_val + 2 * sd_val, y = Inf,
      label = sprintf("+2σ=%.2f", mean_val + 2 * sd_val),
      vjust = 1.5, hjust = 0.5, size = 4.5, color = "#a3a3a3"
    ) +
    ggplot2::annotate(
      "text", x = mean_val - 2 * sd_val, y = Inf,
      label = sprintf("-2σ=%.2f", mean_val - 2 * sd_val),
      vjust = 1.5, hjust = 0.5, size = 4.5, color = "#a3a3a3"
    ) +
    ggplot2::labs(title = title, x = "Value", y = "Density") +
    ggplot2::theme_minimal() +
    ggplot2::theme(panel.grid = ggplot2::element_blank())
}
