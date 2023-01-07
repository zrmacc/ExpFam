test_that("Test quantile function.", {
  
  # Exponential distribution.
  b <- c(-2, log(2))
  probs <- c(0.1, 0.3, 0.5, 0.7, 0.9)
  obs <- suppressMessages({GetQuantile(b, probs = probs)})
  exp <- stats::qexp(probs, rate = 2.0)
  expect_equal(obs, exp, tolerance = 0.0001)
  
  # Standard normal distribution.
  b <- c(-0.5, 0, -0.5 * log(2 * pi))
  obs <- suppressMessages({GetQuantile(b, probs = probs)})
  exp <- stats::qnorm(probs)
  expect_equal(obs, exp, tolerance = 0.0001)
  
})

