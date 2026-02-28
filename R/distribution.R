#' Density
#'
#' Returns a function to evaluate the density of an exponential family distribution of
#' the form \eqn{\exp(\sum_{k=1}^{K}\beta_{k}x^{k})}.
#' 
#' @param b Vector of coefficients for (x^k, x^k-1, ..., x, 1). Note a coefficient
#'   for 1, i.e. the normalizing constant, *should be* included.
#' @param check_normalization If TRUE, verify that the density integrates to 1
#'   before returning (costs one numerical integration). Set FALSE if \code{b}
#'   was obtained from \code{NormalizingConstant}.
#' @return Function that evaluates the distribution.
#' @export
GetDensity <- function(b, check_normalization = TRUE) {

  if (check_normalization) {
    is_normalized <- IsNormalized(b, quiet = TRUE)
    if (!is_normalized) {
      stop("Verify the distribution is normalized.")
    }
  }

  f <- function(x) {
    return(exp(pracma::horner(b, x)$y))
  }
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
#' @param check_normalization If TRUE, verify that the density integrates to 1.
GetDensDeriv <- function(b, check_normalization = TRUE) {

  if (check_normalization) {
    is_normalized <- IsNormalized(b, quiet = TRUE)
    if (!is_normalized) {
      stop("Verify the distribution is normalized.")
    }
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
#' @param check_normalization If TRUE, verify that the density integrates to 1.
#' @return Function that evaluates the distribution.
#' @export
GetCDF <- function(b, lower_tail = TRUE, check_normalization = TRUE) {

  if (check_normalization) {
    is_normalized <- IsNormalized(b, quiet = TRUE)
    if (!is_normalized) {
      stop("Verify the distribution is normalized.")
    }
  }

  xa <- support_lower(b, has_constant = TRUE)

  f <- function(x) {
    return(exp(pracma::horner(b, x)$y))
  }
  if (lower_tail) {
    out <- function(x) {
      return(pracma::quadinf(f = f, xa = xa, xb = x)$Q)
    }
  } else {
    out <- function(x) {
      return(pracma::quadinf(f = f, xa = x, xb = Inf)$Q)
    }
  }

  return(out)
}
