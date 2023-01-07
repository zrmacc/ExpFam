#' Approximate Quantile Function
#' 
#' Returns a function to evaluate the approximate inverse CDF of an exponential
#' family distribution of the form \eqn{\exp(\sum_{k=1}^{K}\beta_{k}x^{k})}.
#' 
#' @param b Vector of coefficients for (x^k, x^k-1, ..., x, 1). Note a coefficient
#'   for 1, i.e. the normalizing constant, *should be* included.
#' @param x0 Expansion point.
#' @return Approximate inverse function.
ApproxQuantile <- function(b, x0 = 0.0) {
  
  suppressMessages({
    dens_deriv <- GetDensDeriv(b)
    dens <- GetDensity(b)
    cdf <- GetCDF(b)
  })
  
  f2_x0 <- dens_deriv(x0)
  f1_x0 <- dens(x0)
  f0_x0 <- cdf(x0)
  
  # Quadratic series reversion.
  b_inv <- rev(c(x0, 1 / f1_x0, -f2_x0 / (2 * f1_x0^3)))
  
  inv_cdf <- function(y) {
    return(pracma::horner(b_inv, y - f0_x0)$y)
  }
  return(inv_cdf)
}


#' Quantile Function
#' 
#' @param b Vector of coefficients for (x^k, x^k-1, ..., x, 1). Note a coefficient
#'   for 1, i.e. the normalizing constant, *should be* included.
#' @param probs Probabilities. 
#' @param xmin Lower bound on interval to search when inverting the CDF.
#' @param xmax Upper bound on interval to search when inverting the CDF.
#' @return Numeric vector of quantiles.
#' @examples 
#' # Standard normal distribution.
#' q <- GetQuantile(b = c(-0.5, 0.0, -0.5 * log(2 * pi)), probs = 0.95)
#' @export
GetQuantile <- function(b, probs, xmin = -2, xmax = 2) {
  
  len_b <- length(b)
  cdf <- GetCDF(b)
  
  if (len_b %% 2 == 0) {
    xmin <- max(0, xmin)
  } 
  
  out <- sapply(probs, function(p) {
    
    g <- function(x) {cdf(x) - p}
    q <- try(stats::uniroot(g, lower = xmin, upper = xmax, extendInt = "upX")$root)
    if (is.numeric(q)) {
      return(q)
    } else {
      return(NA)
    }
    
  })
  return(out)

}
