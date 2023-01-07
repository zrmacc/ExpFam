#' Normalizing Constant
#'
#' Determines the normalizing constant for an exponential family distribution of
#' the form \eqn{\exp(\sum_{k=1}^{K}\beta_{k}x^{k})}.
#'
#' @param b Vector of coefficients for (x^k, x^k-1, ..., x). Note a coefficient
#'   for 1, i.e. the normalizing constant, should *not* be included.
#' @return Vector augmented with the log normalizing constant in final position.
#' @examples 
#' b_in <- c(-0.5, 0)
#' b_out <- NormalizingConstant(b_in)
#' @export
NormalizingConstant <- function(b) {
  
  if (!(b[1] < 0)) {
    stop("The leading coefficient must be negative.")
  }
  
  if (length(b) %% 2 == 0) {
    message("Assuming support (-inf, inf).")
    lower <- -Inf
  } else {
    message("Assuming support (0, inf).")
    lower <- 0
  }
  
  f <- function(x) {exp(pracma::horner(c(b, 0), x)$y)}
  mass <- pracma::quadinf(f = f, xa = lower, xb = Inf)$Q
  out <- c(b, -log(mass))
  
  return(out)
}


#' Check Normalization
#' 
#' Checks if a distribution integrates to 1.
#' 
#' @param b Vector of coefficients for (x^k, x^k-1, ..., x, 1). Note a coefficient
#'   for 1, i.e. the normalizing constant, *should be* included.
#' @param tol Tolerance. 
#' @return Logical indicating whether the distribution integrates to 1. 
IsNormalized <- function(b, tol = 1e-8) {
  
  if (length(b) %% 2 == 0) {
    message("Assuming support (0, inf).")
    lower <- 0
  } else {
    message("Assuming support (-inf, inf).")
    lower <- -Inf
  }
  
  f <- function(x) {exp(pracma::horner(b, x)$y)}
  mass <- pracma::quadinf(f = f, xa = lower, xb = Inf)$Q
  is_normalized <- isTRUE(all.equal(mass, 1, tolerance = 1e-8))
  if (!is_normalized) {
    warning("Density did not integrate to 1.")
  }
  return(is_normalized)  
}
