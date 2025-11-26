test_that("data_summary detects complete data", {
  x <- c(1, 2, 3, 4, 5)
  result <- data_summary(x)
  expect_true(result$is_complete)
})

test_that("data_summary detects incomplete data with NA", {
  x <- c(1, 2, NA, 4, 5)
  result <- data_summary(x)
  expect_false(result$is_complete)
})

test_that("data_summary detects incomplete data with NaN", {
  x <- c(1, 2, NaN, 4, 5)
  result <- data_summary(x)
  expect_false(result$is_complete)
})

test_that("data_summary detects discrete data", {
  x <- c(1, 2, 3, 4, 5)
  result <- data_summary(x)
  expect_true(result$is_discrete)
})

test_that("data_summary detects continuous data", {
  x <- c(1.1, 2.2, 3.3, 4.4, 5.5)
  result <- data_summary(x)
  expect_false(result$is_discrete)
})

test_that("data_summary correctly identifies POSITIVE domain", {
  x <- c(1, 2, 3, 4, 5)
  result <- data_summary(x)
  expect_equal(result$domain, "POSITIVE")
})

test_that("data_summary correctly identifies NEGATIVE domain", {
  x <- c(-5, -4, -3, -2, -1)
  result <- data_summary(x)
  expect_equal(result$domain, "NEGATIVE")
})

test_that("data_summary correctly identifies NONNEGATIVE domain", {
  x <- c(0, 1, 2, 3, 4, 5)
  result <- data_summary(x)
  expect_equal(result$domain, "NONNEGATIVE")
})

test_that("data_summary correctly identifies NONPOSITIVE domain", {
  x <- c(-5, -4, -3, -2, -1, 0)
  result <- data_summary(x)
  expect_equal(result$domain, "NONPOSITIVE")
})

test_that("data_summary correctly identifies REAL domain", {
  x <- c(-5, -2, 0, 2, 5)
  result <- data_summary(x)
  expect_equal(result$domain, "REAL")
})

test_that("data_summary handles single value", {
  x <- c(5)
  result <- data_summary(x)
  expect_equal(result$domain, "POSITIVE")
  expect_true(result$is_discrete)
  expect_true(result$is_complete)
})

test_that("data_summary handles all zeros", {
  x <- c(0, 0, 0)
  result <- data_summary(x)
  expect_equal(result$domain, "NONNEGATIVE")
  expect_true(result$is_discrete)
})

test_that("data_summary handles mixed integer and float values correctly", {
  # All integer values should be discrete even if stored as numeric
  x <- c(1.0, 2.0, 3.0)
  result <- data_summary(x)
  expect_true(result$is_discrete)
  
  # Mix of integers and floats should be continuous
  x <- c(1.0, 2.5, 3.0)
  result <- data_summary(x)
  expect_false(result$is_discrete)
})

test_that("data_summary handles negative floats", {
  x <- c(-3.5, -2.1, -1.7)
  result <- data_summary(x)
  expect_equal(result$domain, "NEGATIVE")
  expect_false(result$is_discrete)
})

test_that("data_summary handles large numbers", {
  x <- c(1e6, 2e6, 3e6)
  result <- data_summary(x)
  expect_equal(result$domain, "POSITIVE")
  expect_true(result$is_discrete)
})

test_that("data_summary handles very small positive numbers", {
  x <- c(1e-6, 2e-6, 3e-6)
  result <- data_summary(x)
  expect_equal(result$domain, "POSITIVE")
  expect_false(result$is_discrete)
})

test_that("data_summary result has expected structure", {
  x <- c(1, 2, 3)
  result <- data_summary(x)
  
  expect_type(result, "list")
  expect_named(result, c("is_complete", "is_discrete", "domain"))
  expect_type(result$is_complete, "logical")
  expect_type(result$is_discrete, "logical")
  expect_type(result$domain, "character")
})
