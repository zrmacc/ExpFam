test_that("Test density.", {
  
  # Exponential distribution.
  b <- c(-2, log(2))
  f <- suppressMessages({GetDensity(b)})
  obs <- f(1.0)
  exp <- 2 * exp(-2)
  expect_equal(obs, exp, tolerance = 0.0001)
  
  # Standard normal distribution.
  b <- c(-0.5, 0, -0.5 * log(2 * pi))
  f <- suppressMessages({GetDensity(b)})
  obs <- f(1.0)
  exp <- stats::dnorm(1.0)
  expect_equal(obs, exp, tolerance = 0.0001)
  
})


test_that("Test density derivative.", {
  
  # Exponential distribution.
  b <- c(-2, log(2))
  f <- suppressMessages({GetDensDeriv(b)})
  obs <- f(1.0)
  exp <- -4 * exp(-2)
  expect_equal(obs, exp, tolerance = 0.0001)
  
  # Standard normal distribution.
  b <- c(-0.5, 0, -0.5 * log(2 * pi))
  f <- suppressMessages({GetDensDeriv(b)})
  obs <- f(1.0)
  exp <- -1 * stats::dnorm(1.0)
  expect_equal(obs, exp, tolerance = 0.0001)
  
})


test_that("Test cumulative distribution.", {

  # Exponential distribution.
  b <- c(-2, log(2))
  f <- suppressMessages(GetCDF(b))
  obs <- f(1.0)
  exp <- stats::pexp(1.0, rate = 2.0)
  expect_equal(obs, exp, tolerance = 0.0001)

  # Standard normal distribution.
  b <- c(-0.5, 0, -0.5 * log(2 * pi))
  f <- suppressMessages(GetCDF(b))
  obs <- f(1.0)
  exp <- stats::pnorm(1.0)
  expect_equal(obs, exp, tolerance = 0.0001)

})

test_that("GetDensity and GetCDF error when distribution not normalized.", {

  b_unnorm <- c(-0.5, 0, 0)
  expect_error(
    suppressWarnings(suppressMessages(GetDensity(b_unnorm))),
    "Verify the distribution is normalized"
  )
  expect_error(
    suppressWarnings(suppressMessages(GetCDF(b_unnorm))),
    "Verify the distribution is normalized"
  )

})