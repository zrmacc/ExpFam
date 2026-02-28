test_that("Test normalization.", {
  
  # Exponential distribution.
  b_in <- c(-2)
  b_out <- suppressMessages({NormalizingConstant(b_in)})
  
  expect_equal(b_out[2], log(2), tolerance = 0.0001)
  
  # Standard normal distribution.
  b_in <- c(-0.5, 0)
  b_out <- suppressMessages({NormalizingConstant(b_in)})
  
  expect_equal(b_out[3], -0.5 * log(2 * pi), tolerance = 0.0001)
  
})

test_that("Test normalization checker.", {
  
  # Standard normal distribution.
  b_in <- c(-0.5, 0, -0.5 * log(2 * pi))
  suppressMessages({
    expect_true(IsNormalized(b_in))
  })
  
  b_in <- c(-0.5, 0, 1)
  suppressMessages({
    expect_warning(IsNormalized(b_in))
  })

})

test_that("NormalizingConstant errors on non-negative leading coefficient.", {

  expect_error(NormalizingConstant(c(0, 1)), "leading coefficient must be negative")
  expect_error(NormalizingConstant(c(1, 0)), "leading coefficient must be negative")

})
