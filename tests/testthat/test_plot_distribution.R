test_that("plot_distribution handles numeric vectors with custom bins", {
  set.seed(123)
  x <- rnorm(50)
  p <- dtools::plot_distribution(x, bins = 12, title = "Numeric Plot")

  expect_s3_class(p, "ggplot")
  expect_equal(p$labels$title, "Numeric Plot")
  expect_equal(p$layers[[1]]$stat_params$bins, 12)
})

test_that("plot_distribution computes bins with Sturges rule", {
  set.seed(321)
  x <- rnorm(30)
  p <- dtools::plot_distribution(x, bins = "Sturges", title = "Auto Bins")
  expected_bins <- 1 + ceiling(3.3322 * log10(length(x)))

  expect_s3_class(p, "ggplot")
  expect_equal(p$layers[[1]]$stat_params$bins, expected_bins)
})

test_that("plot_distribution works with data frames and errors without numerics", {
  df <- data.frame(label = letters[1:5], value = rnorm(5), other = rnorm(5))
  p <- dtools::plot_distribution(df, bins = 5)

  expect_s3_class(p, "ggplot")
  expect_equal(p$layers[[1]]$stat_params$bins, 5)
  expect_error(dtools::plot_distribution(data.frame(a = letters[1:3], b = c("x", "y", "z"))))
})

test_that("plot_distribution handles single group split", {
  set.seed(42)
  x <- rnorm(100, mean = 0, sd = 1)
  p <- dtools::plot_distribution(x, groups = c(0), title = "Split at 0")

  expect_s3_class(p, "ggplot")
  expect_equal(p$labels$title, "Split at 0")
  # Should have colored ribbon areas (one per group) and a density line
  ribbon_layers <- sum(sapply(p$layers, function(l) inherits(l$geom, "GeomRibbon")))
  expect_equal(ribbon_layers, 2) # Two groups
  line_layers <- sum(sapply(p$layers, function(l) inherits(l$geom, "GeomLine")))
  expect_equal(line_layers, 1) # One density line
})

test_that("plot_distribution handles multiple groups", {
  set.seed(123)
  x <- rnorm(200, mean = 0, sd = 2)
  p <- dtools::plot_distribution(x, groups = c(-1, 1), title = "Three groups")

  expect_s3_class(p, "ggplot")
  expect_equal(p$labels$title, "Three groups")
  # Should have three colored ribbon areas and one density line
  ribbon_layers <- sum(sapply(p$layers, function(l) inherits(l$geom, "GeomRibbon")))
  expect_equal(ribbon_layers, 3) # Three groups
  line_layers <- sum(sapply(p$layers, function(l) inherits(l$geom, "GeomLine")))
  expect_equal(line_layers, 1) # One density line
})

test_that("plot_distribution groups are properly sorted", {
  set.seed(456)
  x <- rnorm(100)
  # Provide groups in unsorted order - should be sorted internally
  p1 <- dtools::plot_distribution(x, groups = c(1, -1))
  p2 <- dtools::plot_distribution(x, groups = c(-1, 1))

  expect_s3_class(p1, "ggplot")
  expect_s3_class(p2, "ggplot")
  # Both should produce valid plots (internal sorting should handle it)
})

test_that("plot_distribution without groups uses single color scheme", {
  set.seed(789)
  x <- rnorm(50)
  p <- dtools::plot_distribution(x, title = "No groups")

  expect_s3_class(p, "ggplot")
  # Should have exactly one density layer when no groups specified
  density_layers <- sum(sapply(p$layers, function(l) inherits(l$geom, "GeomDensity")))
  expect_equal(density_layers, 1)
})

test_that("plot_distribution groups work with histogram", {
  set.seed(321)
  x <- rnorm(150)
  p <- dtools::plot_distribution(x, bins = 20, groups = c(0), title = "Grouped with histogram")

  expect_s3_class(p, "ggplot")
  # Should have histogram, colored ribbons, and density line
  has_histogram <- any(sapply(p$layers, function(l) inherits(l$geom, "GeomBar")))
  has_ribbon <- any(sapply(p$layers, function(l) inherits(l$geom, "GeomRibbon")))
  has_line <- any(sapply(p$layers, function(l) inherits(l$geom, "GeomLine")))
  expect_true(has_histogram)
  expect_true(has_ribbon)
  expect_true(has_line)
})

test_that("plot_distribution handles edge case with many groups", {
  set.seed(111)
  x <- rnorm(500, sd = 3)
  # Test with 5 groups (uses extended color palette)
  p <- dtools::plot_distribution(x, groups = c(-2, -1, 0, 1, 2), title = "Five groups")

  expect_s3_class(p, "ggplot")
  # Should have 6 colored ribbon areas (5 boundaries = 6 groups) and one density line
  ribbon_layers <- sum(sapply(p$layers, function(l) inherits(l$geom, "GeomRibbon")))
  expect_equal(ribbon_layers, 6)
  line_layers <- sum(sapply(p$layers, function(l) inherits(l$geom, "GeomLine")))
  expect_equal(line_layers, 1)
})

test_that("plot_distribution groups work with data frames", {
  set.seed(222)
  df <- data.frame(returns = rnorm(100))
  p <- dtools::plot_distribution(df, groups = c(0), title = "DataFrame with groups")

  expect_s3_class(p, "ggplot")
  ribbon_layers <- sum(sapply(p$layers, function(l) inherits(l$geom, "GeomRibbon")))
  expect_equal(ribbon_layers, 2)
  line_layers <- sum(sapply(p$layers, function(l) inherits(l$geom, "GeomLine")))
  expect_equal(line_layers, 1)
})

test_that("plot_distribution bins=NULL with groups shows only densities", {
  set.seed(333)
  x <- rnorm(100)
  p <- dtools::plot_distribution(x, bins = NULL, groups = c(0))

  expect_s3_class(p, "ggplot")
  # Should have no histogram layers when bins=NULL
  has_histogram <- any(sapply(p$layers, function(l) inherits(l$geom, "GeomBar")))
  expect_false(has_histogram)
  # But should still have colored ribbons and density line
  ribbon_layers <- sum(sapply(p$layers, function(l) inherits(l$geom, "GeomRibbon")))
  expect_equal(ribbon_layers, 2)
  line_layers <- sum(sapply(p$layers, function(l) inherits(l$geom, "GeomLine")))
  expect_equal(line_layers, 1)
})
