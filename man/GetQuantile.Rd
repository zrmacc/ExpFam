% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/quantile.R
\name{GetQuantile}
\alias{GetQuantile}
\title{Quantile Function}
\usage{
GetQuantile(b, probs, xmin = -2, xmax = 2)
}
\arguments{
\item{b}{Vector of coefficients for (x^k, x^k-1, ..., x, 1). Note a coefficient
for 1, i.e. the normalizing constant, \emph{should be} included.}

\item{probs}{Probabilities.}

\item{xmin}{Lower bound on interval to search when inverting the CDF.}

\item{xmax}{Upper bound on interval to search when inverting the CDF.}
}
\value{
Numeric vector of quantiles.
}
\description{
Quantile Function
}
\examples{
# Standard normal distribution.
q <- GetQuantile(b = c(-0.5, 0.0, -0.5 * log(2 * pi)), probs = 0.95)
}
