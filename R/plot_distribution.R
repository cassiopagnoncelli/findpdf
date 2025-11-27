#' Plot distribution with histogram and density overlay
#'
#' @param data Numeric vector or data frame with numeric column
#' @param bins Number of histogram bins (default: NULL, no histogram). Use "Sturges" for
#'   Sturges' rule or a numeric value.
#' @param groups Numeric vector of threshold values to split data into groups (default: NULL,
#'   no grouping). When provided, splits data at thresholds and plots each group with its own color.
#' @param title Plot title (default: "Distribution")
#' @return ggplot2 object showing histogram with density curve and statistical markers
#' @importFrom stats sd
#' @export
#' @examples
#' \donttest{
#' plot_distribution(rnorm(1000))
#' plot_distribution(rnorm(1000), groups = c(0), title = "Split at 0")
#' plot_distribution(rnorm(1000), groups = c(-1, 1), title = "Three groups")
#' }
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
  group_colors <- c("#329f32", "#f03b3b", "#eaaf17", "#1f77b4", "#9467bd", "#8c564b", "#e377c2")

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
        linewidth = 2
      ) +
      ggplot2::geom_vline(xintercept = mean_val, color = "#000000", linetype = "dashed", linewidth = .8) +
      ggplot2::geom_vline(xintercept = mean_val + sd_val, color = "#4e4e4e", linetype = "dashed", linewidth = .5) +
      ggplot2::geom_vline(xintercept = mean_val - sd_val, color = "#4e4e4e", linetype = "dashed", linewidth = .5) +
      ggplot2::geom_vline(xintercept = mean_val + 2 * sd_val, color = "#a3a3a3", linetype = "dashed", linewidth = .35) +
      ggplot2::geom_vline(xintercept = mean_val - 2 * sd_val, color = "#a3a3a3", linetype = "dashed", linewidth = .35)

    # Create legend text
    legend_text <- sprintf("mu=%.2f  sigma=%.2f", mean_val, sd_val)

    p <- p +
      ggplot2::annotate("label",
        x = Inf, y = Inf, label = legend_text,
        hjust = 1.05, vjust = 1.1, size = 3.5,
        fill = "white", alpha = 0.9, label.padding = ggplot2::unit(0.25, "lines")
      ) +
      ggplot2::labs(title = title, x = "Value", y = "Density") +
      ggplot2::theme_minimal() +
      ggplot2::theme(panel.grid = ggplot2::element_blank())

    return(p)
  }

  # Groups specified - single density with colored areas
  groups <- sort(groups)
  n_groups <- length(groups) + 1

  # Reverse colors so rightmost group gets first color (green), leftmost gets last
  reversed_colors <- rev(group_colors[1:n_groups])

  # Create base plot
  p <- ggplot2::ggplot(data, ggplot2::aes(x = value))

  # Add histogram if specified (for all data)
  if (!is.null(bins)) {
    p <- p +
      ggplot2::geom_histogram(
        ggplot2::aes(y = ggplot2::after_stat(density)),
        bins = bins,
        fill = "gray",
        alpha = 0.3
      )
  }

  # Compute density for all data
  dens <- density(data$value, n = 512)
  dens_df <- data.frame(x = dens$x, y = dens$y)

  # Color the area under density curve by group
  # Create boundaries including -Inf and Inf
  boundaries <- c(-Inf, groups, Inf)

  # Plot each colored segment and calculate group means
  group_means <- list()
  for (i in 1:n_groups) {
    segment_df <- dens_df |>
      dplyr::filter(x >= boundaries[i] & x < boundaries[i + 1])

    if (nrow(segment_df) > 0) {
      group_color <- reversed_colors[i]

      # Calculate mean for this group
      group_data_values <- data$value[data$value >= boundaries[i] & data$value < boundaries[i + 1]]
      if (length(group_data_values) > 0) {
        group_mean <- mean(group_data_values, na.rm = TRUE)
        group_means[[i]] <- list(mean = group_mean, color = group_color)
      }

      p <- p +
        ggplot2::geom_ribbon(
          data = segment_df,
          ggplot2::aes(x = x, ymin = 0, ymax = y),
          fill = group_color,
          alpha = 0.6
        )
    }
  }

  # Add density line on top (very thin)
  p <- p +
    ggplot2::geom_line(
      data = dens_df,
      ggplot2::aes(x = x, y = y),
      linewidth = 0.08
    )

  # Add dashed lines for group means with annotations on top
  for (i in seq_along(group_means)) {
    if (!is.null(group_means[[i]])) {
      mean_val <- group_means[[i]]$mean
      mean_color <- group_means[[i]]$color

      p <- p +
        ggplot2::geom_vline(
          xintercept = mean_val,
          color = mean_color,
          linetype = "dashed",
          linewidth = 0.3,
          alpha = 0.8
        ) +
        ggplot2::annotate("text",
          x = mean_val, y = Inf,
          label = sprintf("mu=%.2f", mean_val),
          hjust = 0.5, vjust = 1.2,
          size = 3, fontface = "bold",
          color = mean_color
        )
    }
  }

  # Add vertical lines at group boundaries with the color of the group to the right (thinner)
  for (i in seq_along(groups)) {
    # Group i+1 is to the right of boundary i, and uses reversed_colors[i+1]
    boundary_color <- reversed_colors[i + 1]
    p <- p +
      ggplot2::geom_vline(
        xintercept = groups[i],
        color = boundary_color,
        linetype = "solid",
        linewidth = 0.25,
        alpha = 0.8
      ) +
      ggplot2::annotate("text",
        x = groups[i], y = -Inf,
        label = sprintf("%.2f", groups[i]),
        hjust = -0.1, vjust = -0.5,
        size = 3.5, fontface = "bold",
        color = boundary_color
      )
  }

  p <- p +
    ggplot2::labs(title = title, x = "Value", y = "Density") +
    ggplot2::theme_minimal() +
    ggplot2::theme(panel.grid = ggplot2::element_blank(), legend.position = "none")

  return(p)
}
