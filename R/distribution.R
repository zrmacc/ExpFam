#' Density
#'
#' Returns a function to evaluate the density of an exponential family distribution of
#' the form \eqn{\exp(\sum_{k=1}^{K}\beta_{k}x^{k})}.
#' 
#' @param b Vector of coefficients for (x^k, x^k-1, ..., x, 1). Note a coefficient
#'   for 1, i.e. the normalizing constant, *should be* included.
#' @return Function that evalutes the distribution.
#' @export
GetDensity <- function(b) {
  
  is_normalized <- IsNormalized(b)
  if (!is_normalized) {
    stop("Verify the distribution is normalized.")
  }
  
  f <- function(x) {exp(pracma::horner(b, x)$y)}
  return(f)
}


#' Density Derivative
#'
#' Returns a function to evaluate the *derivative of* the density of an
#' exponential family distribution of the form
#' \eqn{\exp(\sum_{k=1}^{K}\beta_{k}x^{k})}.
#' 
#' @param b Vector of coefficients for (x^k, x^k-1, ..., x, 1). Note a coefficient
#'   for 1, i.e. the normalizing constant, *should be* included.
#' @return Function that evalutes the distribution.
GetDensDeriv <- function(b) {
  
  is_normalized <- IsNormalized(b)
  if (!is_normalized) {
    stop("Verify the distribution is normalized.")
  }
  
  f <- function(x) {
    res <- pracma::horner(b, x)
    return(exp(res$y) * res$dy)
  }
  return(f)
}


# -----------------------------------------------------------------------------


#' Cumulative Distribution Function
#'
#' Returns a function to evaluate the cumulative distribution function of an
#' exponential family distribution of the form
#' \eqn{\exp(\sum_{k=1}^{K}\beta_{k}x^{k})}.
#' 
#' @param b Vector of coefficients for (x^k, x^k-1, ..., x, 1). Note a coefficient
#'   for 1, i.e. the normalizing constant, *should be* included.
#' @param lower_tail Logical. If TRUE, integrates (-Inf, x). If FALSE,
#'   integrates (x, Inf).
#' @return Function that evalutes the distribution.
#' @export
GetCDF <- function(b, lower_tail = TRUE) {
  
  is_normalized <- IsNormalized(b)
  if (!is_normalized) {
    stop("Verify the distribution is normalized.")
  }

  len_b <- length(b)
  if (len_b %% 2 == 0) {
    xa <- 0
  } else {
    xa <- -Inf
  }
  
  f <- function(x) {exp(pracma::horner(b, x)$y)}
  if (lower_tail) {
    out <- function(x) {pracma::quadinf(f = f, xa = xa, xb = x)$Q}
  } else {
    out <- function(x) {pracma::quadinf(f = f, xa = x, xb = Inf)$Q}
  }

  return(out)
}
