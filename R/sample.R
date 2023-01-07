#' Sample Exponential Family
#'
#' @param b Vector of coefficients for (x^k, x^k-1, ..., x). If normalized,
#'   include the normalization constant as well.
#' @param n Number of samples.
#' @param is_normalized Has the normalizing constant been included in the
#'   coefficient vector? Default: TRUE
#' @param xmin Lower bound on interval to search when inverting the CDF.
#' @param xmax Upper bound on interval to search when inverting the CDF.
#' @return Numeric vector of samples from the distribution. 
#' @examples 
#' # Draw from standard normal distribution.
#' x <- rExpFam(b = c(-0.5, 0.0, -0.5 * log(2 * pi)), n = 10)
#' @export
rExpFam <- function(b, n, is_normalized = TRUE, xmin = -2, xmax = 2) {
  
  # Calculate normalizing constant. 
  if (!is_normalized) {
    b <- NormalizingConstant(b)
  }
  
  # Generate sample.
  u <- stats::runif(n = n)
  out <- GetQuantile(b, probs = u, xmin = xmin, xmax = xmax)
  return(out)
  
}
