test_that("Test quantile function.", {
  
  n <- 10
  
  # Exponential distribution.
  b <- c(-2)
  withr::local_seed(101)
  obs <- suppressMessages({rExpFam(b, n = n, is_normalized = FALSE)})
  ref <- stats::rexp(n = n, rate = 2)
  p <- stats::ks.test(obs, ref)$p.value
  expect_gt(p, 0.05)
  
  # Exponential distribution.
  b <- c(-0.5, 0)
  withr::local_seed(101)
  obs <- suppressMessages({rExpFam(b, n = n, is_normalized = FALSE)})
  ref <- stats::rnorm(n = n)
  p <- stats::ks.test(obs, ref)$p.value
  expect_gt(p, 0.05)
  
})

