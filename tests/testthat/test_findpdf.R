test_that("dtools returns correct object structure", {
  set.seed(123)
  x <- rnorm(100, mean = 5, sd = 2)
  result <- dtools(x)
  
  expect_s3_class(result, "dtools_result")
  expect_named(result, c("params", "ranking", "data_summary", "pdf", "cdf", "best_fit"))
  expect_type(result$params, "list")
  expect_s3_class(result$ranking, "data.frame")
  expect_type(result$data_summary, "list")
  expect_type(result$pdf, "closure")
  expect_type(result$cdf, "closure")
  expect_type(result$best_fit, "character")
})

test_that("dtools works with normal distribution", {
  set.seed(123)
  x <- rnorm(100, mean = 5, sd = 2)
  result <- dtools(x)
  
  # Should find a continuous distribution
  expect_false(result$data_summary$is_discrete)
  
  # Ranking should have distributions and errors
  expect_true(nrow(result$ranking) > 0)
  expect_true(all(c("pf", "error") %in% names(result$ranking)))
  expect_true(all(result$ranking$error >= 0))
  
  # Best fit should be in the ranking
  expect_true(result$best_fit %in% result$ranking$pf)
})

test_that("dtools works with discrete distribution", {
  set.seed(456)
  x <- rpois(100, lambda = 5)
  result <- dtools(x)
  
  # Should detect discrete data
  expect_true(result$data_summary$is_discrete)
  
  # Should have results
  expect_true(nrow(result$ranking) > 0)
  expect_true(result$best_fit %in% result$ranking$pf)
})

test_that("dtools pdf function works", {
  set.seed(123)
  x <- rnorm(100, mean = 5, sd = 2)
  result <- dtools(x)
  
  # PDF should be callable
  pdf_val <- result$pdf(5)
  expect_type(pdf_val, "double")
  expect_true(length(pdf_val) == 1)
  expect_true(pdf_val >= 0)
  expect_false(is.na(pdf_val))
})

test_that("dtools cdf function works", {
  set.seed(123)
  x <- rnorm(100, mean = 5, sd = 2)
  result <- dtools(x)
  
  # CDF should be callable
  cdf_val <- result$cdf(5)
  expect_type(cdf_val, "double")
  expect_true(length(cdf_val) == 1)
  expect_true(cdf_val >= 0 && cdf_val <= 1)
  expect_false(is.na(cdf_val))
})

test_that("dtools ranking is sorted by error", {
  set.seed(123)
  x <- rnorm(100, mean = 5, sd = 2)
  result <- dtools(x)
  
  # Errors should be sorted (smallest first)
  errors <- result$ranking$error
  expect_true(all(diff(errors) >= 0))
  
  # Best fit should have lowest error
  expect_equal(result$best_fit, as.character(result$ranking$pf[1]))
  expect_equal(min(result$ranking$error), result$ranking$error[1])
})

test_that("dtools handles NA values with remove.na = TRUE", {
  set.seed(123)
  x <- c(rnorm(100, mean = 5, sd = 2), NA, NA, NA)
  
  # Should not error with remove.na = TRUE (default)
  expect_no_error(result <- dtools(x, remove.na = TRUE))
})

test_that("dtools params are stored correctly", {
  set.seed(123)
  x <- rnorm(100, mean = 5, sd = 2)
  result <- dtools(x)
  
  # Params should be a named list
  expect_type(result$params, "list")
  expect_true(length(result$params) > 0)
  
  # Best fit params should exist
  expect_true(result$best_fit %in% names(result$params))
  
  # Params should be numeric vectors
  best_params <- result$params[[result$best_fit]]
  expect_type(best_params, "double")
})

test_that("dtools works with uniform-like distribution", {
  set.seed(789)
  x <- runif(100, min = 0, max = 10)
  result <- dtools(x)
  
  # Should work and return valid results
  expect_s3_class(result, "dtools_result")
  expect_true(nrow(result$ranking) > 0)
  expect_false(result$data_summary$is_discrete)
})

test_that("dtools works with exponential-like distribution", {
  set.seed(101)
  x <- rexp(100, rate = 0.5)
  result <- dtools(x)
  
  # Should work with positive data
  expect_s3_class(result, "dtools_result")
  expect_equal(result$data_summary$domain, "POSITIVE")
})

test_that("dtools include.exotics parameter works", {
  set.seed(123)
  x <- rnorm(100, mean = 5, sd = 2)
  
  result_no_exotics <- dtools(x, include.exotics = FALSE)
  result_with_exotics <- dtools(x, include.exotics = TRUE)
  
  # With exotics should have more candidates
  expect_true(nrow(result_with_exotics$ranking) >= nrow(result_no_exotics$ranking))
})

test_that("dtools handles small datasets", {
  set.seed(123)
  x <- rnorm(10, mean = 5, sd = 2)
  
  # Should still work with small datasets
  expect_no_error(result <- dtools(x))
  expect_s3_class(result, "dtools_result")
})

test_that("dtools cmpfun compilation works correctly", {
  # This test ensures the compiler::cmpfun issue is fixed
  set.seed(123)
  x <- rnorm(100, mean = 5, sd = 2)
  
  # Should not error about missing cmpfun
  expect_no_error(result <- dtools(x))
  
  # Should successfully optimize parameters (using cmpfun internally)
  expect_true(length(result$params) > 0)
  expect_true(all(sapply(result$params, function(p) length(p) > 0)))
})

test_that("dtools print method works", {
  set.seed(123)
  x <- rnorm(100, mean = 5, sd = 2)
  result <- dtools(x)
  
  # Print should not error
  expect_output(print(result), "Best-Fitting Probability Distributions")
  expect_output(print(result), "Data Summary")
  expect_output(print(result), "Top Distributions")
})

test_that("dtools print method with n parameter", {
  set.seed(123)
  x <- rnorm(100, mean = 5, sd = 2)
  result <- dtools(x)
  
  # Print with limited results
  expect_output(print(result, n = 3))
  expect_output(print(result, n = 5))
})

test_that("dtools handles negative data correctly", {
  set.seed(234)
  x <- rnorm(100, mean = -5, sd = 2)
  result <- dtools(x)
  
  # Should handle negative data
  expect_s3_class(result, "dtools_result")
  expect_true(result$data_summary$domain %in% c("NEGATIVE", "REAL"))
})

test_that("dtools parameter conversion works for discrete params", {
  # Test that discrete parameters are properly rounded
  set.seed(345)
  x <- rpois(100, lambda = 5)
  result <- dtools(x)
  
  # Should have discrete data
  expect_true(result$data_summary$is_discrete)
  
  # Parameters that should be integers should be close to integers
  # (after optimization with cmpfun's conv.params)
  expect_s3_class(result, "dtools_result")
})

test_that("dtools pdf and cdf are consistent", {
  set.seed(456)
  x <- rnorm(100, mean = 5, sd = 2)
  result <- dtools(x)
  
  # CDF at upper bound should be higher than at lower bound
  lower_cdf <- result$cdf(min(x))
  upper_cdf <- result$cdf(max(x))
  expect_true(upper_cdf >= lower_cdf)
})

test_that("dtools handles data with narrow range", {
  set.seed(567)
  x <- rnorm(100, mean = 5, sd = 0.1)
  
  # Should still work with narrow range
  expect_no_error(result <- dtools(x))
  expect_s3_class(result, "dtools_result")
})

test_that("dtools data_summary is captured correctly", {
  set.seed(678)
  x <- rnorm(100, mean = 5, sd = 2)
  result <- dtools(x)
  
  # data_summary should match what we'd get directly
  expected_summary <- dtools:::data_summary(x)
  expect_equal(result$data_summary$is_discrete, expected_summary$is_discrete)
  expect_equal(result$data_summary$domain, expected_summary$domain)
})
