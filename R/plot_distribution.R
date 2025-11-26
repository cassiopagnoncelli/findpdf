#' Plot distribution with histogram and density overlay
#'
#' @param data Numeric vector or data frame with numeric column
#' @param bins Number of histogram bins (default: NULL, no histogram). Use "Sturges" for Sturges' rule or a numeric value.
#' @param groups Numeric vector of threshold values to split data into groups (default: NULL, no grouping). When provided, splits data at thresholds and plots each group with its own color.
#' @param title Plot title (default: "Distribution")
#' @return ggplot2 object showing histogram with density curve and statistical markers
#' @export
#' @examples
#' plot_distribution(rnorm(1000))
#' plot_distribution(rnorm(1000), groups = c(0), title = "Split at 0")
#' plot_distribution(rnorm(1000), groups = c(-1, 1), title = "Three groups")
plot_distribution <- function(data, bins = NULL, groups = NULL, title = "Distribution") {
  # Handle different input types
  if (is.vector(data) || is.atomic(data)) {
    data <- tibble::tibble(value = data)
  } else if (is.data.frame(data)) {
    numeric_cols <- sapply(data, is.numeric)
    if (!any(numeric_cols)) {
      stop("No numeric columns found in the data")
    }
    first_numeric <- names(data)[which(numeric_cols)[1]]
    data <- tibble::tibble(value = data[[first_numeric]])
  }

  # Calculate number of bins using Sturges' rule if specified
  if (!is.null(bins) && bins == "Sturges") {
    n <- nrow(data)
    bins <- 1 + ceiling(3.3322 * log10(n))
  }

  # Define color palette for groups
  group_colors <- c("#329f32", "#d62728", "#b8860b", "#1f77b4", "#9467bd", "#8c564b", "#e377c2")
  
  # If no groups specified, plot as single distribution
  if (is.null(groups)) {
    mean_val <- mean(data$value, na.rm = TRUE)
    sd_val <- sd(data$value, na.rm = TRUE)
    
    p <- ggplot2::ggplot(data, ggplot2::aes(x = value))
    
    if (!is.null(bins)) {
      p <- p +
        ggplot2::geom_histogram(
          ggplot2::aes(y = ggplot2::after_stat(density)),
          bins = bins,
          fill = group_colors[1],
          alpha = 0.35
        )
    }
    
    p <- p +
      ggplot2::geom_density(
        ggplot2::aes(y = ggplot2::after_stat(density)),
        fill = group_colors[1],
        alpha = .45,
        linewidth = .1
      ) +
      ggplot2::geom_vline(xintercept = mean_val, color = "#000000", linetype = "dashed", linewidth = .8) +
      ggplot2::geom_vline(xintercept = mean_val + sd_val, color = "#4e4e4e", linetype = "dashed", linewidth = .5) +
      ggplot2::geom_vline(xintercept = mean_val - sd_val, color = "#4e4e4e", linetype = "dashed", linewidth = .5) +
      ggplot2::geom_vline(xintercept = mean_val + 2 * sd_val, color = "#a3a3a3", linetype = "dashed", linewidth = .35) +
      ggplot2::geom_vline(xintercept = mean_val - 2 * sd_val, color = "#a3a3a3", linetype = "dashed", linewidth = .35) +
      ggplot2::annotate("text", x = mean_val, y = Inf, label = sprintf("μ=%.2f", mean_val), vjust = -0.3, hjust = 0.5, size = 5, color = "#000000") +
      ggplot2::annotate("text", x = mean_val + sd_val, y = Inf, label = sprintf("+1σ=%.2f", mean_val + sd_val), vjust = -0.3, hjust = 0.5, size = 3.5, color = "#7e7e7e") +
      ggplot2::annotate("text", x = mean_val - sd_val, y = Inf, label = sprintf("-1σ=%.2f", mean_val - sd_val), vjust = -0.3, hjust = 0.5, size = 3.5, color = "#7e7e7e") +
      ggplot2::annotate("text", x = mean_val + 2 * sd_val, y = Inf, label = sprintf("+2σ=%.2f", mean_val + 2 * sd_val), vjust = -0.3, hjust = 0.5, size = 3, color = "#c0c0c0") +
      ggplot2::annotate("text", x = mean_val - 2 * sd_val, y = Inf, label = sprintf("-2σ=%.2f", mean_val - 2 * sd_val), vjust = -0.3, hjust = 0.5, size = 3, color = "#c0c0c0") +
      ggplot2::labs(title = title, x = "Value", y = "Density") +
      ggplot2::theme_minimal() +
      ggplot2::theme(panel.grid = ggplot2::element_blank())
    
    return(p)
  }
  
  # Groups specified - split data and plot by group
  groups <- sort(groups)
  n_groups <- length(groups) + 1
  group_labels <- paste0("g", 1:n_groups)
  
  # Assign each value to a group (similar to analyse())
  data <- data |>
    dplyr::mutate(
      group = dplyr::case_when(
        value < groups[1] ~ group_labels[1],
        TRUE ~ group_labels[n_groups]
      )
    )
  
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
  
  # Create base plot
  p <- ggplot2::ggplot(data, ggplot2::aes(x = value))
  
  # Add histogram if specified (combined for all groups)
  if (!is.null(bins)) {
    p <- p +
      ggplot2::geom_histogram(
        ggplot2::aes(y = ggplot2::after_stat(density), fill = group),
        bins = bins,
        alpha = 0.35,
        position = "identity"
      ) +
      ggplot2::scale_fill_manual(values = group_colors[1:n_groups])
  }
  
  # Add density curves and statistics for each group
  for (i in 1:n_groups) {
    group_data <- data |> dplyr::filter(group == group_labels[i])
    if (nrow(group_data) > 0) {
      group_color <- group_colors[i]
      mean_val <- mean(group_data$value, na.rm = TRUE)
      sd_val <- sd(group_data$value, na.rm = TRUE)
      
      # Add density curve for this group
      p <- p +
        ggplot2::geom_density(
          data = group_data,
          ggplot2::aes(y = ggplot2::after_stat(density)),
          fill = group_color,
          alpha = .45,
          linewidth = .1
        )
      
      # Add mean and SD lines for this group
      p <- p +
        ggplot2::geom_vline(xintercept = mean_val, color = group_color, linetype = "dashed", linewidth = .8, alpha = 0.8) +
        ggplot2::geom_vline(xintercept = mean_val + sd_val, color = group_color, linetype = "dashed", linewidth = .5, alpha = 0.6) +
        ggplot2::geom_vline(xintercept = mean_val - sd_val, color = group_color, linetype = "dashed", linewidth = .5, alpha = 0.6) +
        ggplot2::geom_vline(xintercept = mean_val + 2 * sd_val, color = group_color, linetype = "dashed", linewidth = .35, alpha = 0.4) +
        ggplot2::geom_vline(xintercept = mean_val - 2 * sd_val, color = group_color, linetype = "dashed", linewidth = .35, alpha = 0.4)
      
      # Add annotations
      p <- p +
        ggplot2::annotate("text", x = mean_val, y = Inf, label = sprintf("μ%d=%.2f", i, mean_val), vjust = -0.3 - (i - 1) * 0.8, hjust = 0.5, size = 4, color = group_color) +
        ggplot2::annotate("text", x = mean_val + sd_val, y = Inf, label = sprintf("+1σ%d", i), vjust = -0.3 - (i - 1) * 0.8, hjust = 0.5, size = 3, color = group_color) +
        ggplot2::annotate("text", x = mean_val - sd_val, y = Inf, label = sprintf("-1σ%d", i), vjust = -0.3 - (i - 1) * 0.8, hjust = 0.5, size = 3, color = group_color)
    }
  }
  
  # Add vertical lines at group boundaries with next group's color
  for (i in 1:length(groups)) {
    boundary_color <- group_colors[i + 1]  # Use next group's color
    p <- p +
      ggplot2::geom_vline(
        xintercept = groups[i],
        color = boundary_color,
        linetype = "solid",
        linewidth = .6,
        alpha = .9
      )
  }
  
  p <- p +
    ggplot2::labs(title = title, x = "Value", y = "Density") +
    ggplot2::theme_minimal() +
    ggplot2::theme(panel.grid = ggplot2::element_blank(), legend.position = "none")
  
  return(p)
}
