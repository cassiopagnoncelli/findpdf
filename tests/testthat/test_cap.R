test_that("cap caps values to quantile bounds", {
  x <- c(1, 2, 3, 100)
  result <- dtools::cap(x, quantiles = c(0.25, 0.75))
  qs <- quantile(x, probs = c(0.25, 0.75), na.rm = TRUE)
  expected <- x
  expected[expected < qs[1]] <- qs[1]
  expected[expected > qs[2]] <- qs[2]

  expect_equal(result, expected)
})

test_that("cap keeps missing values and validates inputs", {
  x <- c(1, NA, 10)

  expect_true(is.na(dtools::cap(x)[2]))
  expect_error(dtools::cap(x, quantiles = 0.5))
  expect_error(dtools::cap(x, quantiles = c(-0.1, 0.5)))
  expect_error(dtools::cap(x, quantiles = c(0.8, 0.2)))
})

test_that("cap with replace policy replaces outliers", {
  x <- c(1, 2, 3, 100)
  result <- dtools::cap(x, quantiles = c(0.25, 0.75), policy = "replace")
  qs <- quantile(x, probs = c(0.25, 0.75), na.rm = TRUE)

  # Values should be capped at quantile bounds
  expect_true(all(result >= qs[1] & result <= qs[2]))
  expect_equal(length(result), length(x))
})

test_that("cap with remove policy removes outliers", {
  x <- c(1, 2, 3, 100)
  result <- dtools::cap(x, quantiles = c(0.25, 0.75), policy = "remove")
  qs <- quantile(x, probs = c(0.25, 0.75), na.rm = TRUE)

  # All remaining values should be within bounds
  expect_true(all(result >= qs[1] & result <= qs[2], na.rm = TRUE))
  # Result should be shorter since outliers are removed
  expect_lt(length(result), length(x))
  # Specifically, should contain only values 2 and 3
  expect_equal(result, c(2, 3))
})

test_that("cap with remove policy keeps NA values", {
  x <- c(1, NA, 2, 3, NA, 100)
  result <- dtools::cap(x, quantiles = c(0.25, 0.75), policy = "remove")

  # Should keep NA values
  expect_equal(sum(is.na(result)), 2)
  # Should remove extreme values (1 and 100)
  expect_equal(length(result), 4) # 2, 3, and two NAs
})

test_that("cap policy argument validation works", {
  x <- c(1, 2, 3, 100)

  # Valid policies should work
  expect_no_error(dtools::cap(x, policy = "replace"))
  expect_no_error(dtools::cap(x, policy = "remove"))

  # Invalid policy should error
  expect_error(dtools::cap(x, policy = "invalid"))
})
