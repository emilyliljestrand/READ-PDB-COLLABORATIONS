#' @title Calculate rho-adjusted time series
#'
#' @description Applies a Mohn's rho adjustment to a time series and its 90% confidence bounds. Each value is divided by \eqn{1 + \rho}.
#'
#' @param series A data frame containing the following columns (as produced by `calc.uncertainty()`):
#' * est - Estimated time series
#' * lo_90 - Lower 90% CI for estimates
#' * hi_90 - Upper 90% CI for estimates
#' @md
#' @param rho Numeric. Mohn's rho value associated with the time series (unitless).
#'
#' @return A data frame containing the original columns plus `est.adj`, `lo_90.adj`, and `hi_90.adj` (rho-adjusted estimate and 90% CI).
#' @export

calc.rho.adj.ests <- function(series = NULL,
                              rho = NULL) {
  # Mohn's rho retrospective adjustment: scale each value by 1 / (1 + rho)
  result <- series %>%
    as.data.frame() %>%
    mutate(
      est.adj = (1 / (1 + rho)) * est,
      lo_90.adj = (1 / (1 + rho)) * lo_90,
      hi_90.adj = (1 / (1 + rho)) * hi_90
    )

  return(result)
}
