#' @title Approximate confidence intervals for lognormal variables
#'
#' @description Calculate approximate confidence intervals for lognormal variables based on the point estimate and its CV. Used in Dan's autoreporting as the default for survey index CI calculations.
#'
#' @param x Numeric vector. Lognormal point estimate(s) on the original (non-log) scale. No default.
#' @param cv.x Numeric vector. CV of each variable in `x` (same length as `x`, or length 1). No default.
#' @param bounds Numeric. Confidence level in percent, default = 95.
#'
#' @return A data.frame with columns `lci` (lower confidence bound) and `uci` (upper confidence bound), on the same scale as `x`.
#' @export

asm.ci <- function(x, cv.x, bounds = 95) {
  # Approximate log-scale SD from the CV: s = sqrt(log(1 + CV^2))
  s <- sqrt(log(1 + cv.x^2))
  # CV of 0 or non-finite CV gives s = 0 (no interval width)
  s <- ifelse(is.finite(s), s, 0)
  # Two-sided tail probability, then the matching standard normal quantile
  p <- (1 - (bounds / 100)) / 2
  Z <- qnorm(p)
  # Bounds are multiplicative on the original scale (symmetric in log space)
  lci <- x * exp(Z * (s))
  uci <- x * exp(-Z * (s))
  return(data.frame("lci" = lci, "uci" = uci))
}
