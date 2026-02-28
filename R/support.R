# Internal helper: support and integration bounds
# b is the coefficient vector; has_constant TRUE if b includes the constant term (length K+1).

#' @noRd
support_lower <- function(b, has_constant = TRUE) {

  len <- length(b)
  if (has_constant) {
    # length = K+1: even => K odd => (0, Inf); odd => K even => (-Inf, Inf)
    return(if (len %% 2 == 0) 0 else -Inf)
  } else {
    # length = K: even => K even => (-Inf, Inf); odd => K odd => (0, Inf)
    return(if (len %% 2 == 0) -Inf else 0)
  }
}
