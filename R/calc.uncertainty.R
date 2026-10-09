#' @title Calculate uncertainty
#'
#' @description Calculate the CV, 90% and 95% confidence intervals, and related quantities for a time series given log-scale estimates and standard errors (e.g., from `TMB::sdreport()`).
#'
#' @param log.est Numeric vector. Estimated time series on the log scale. No default.
#' @param log.se Numeric vector. Standard errors of `log.est` on the log scale, same length as `log.est`. No default.
#'
#' @return A data frame with columns:
#' \itemize{
#'   \item est - Time series of estimated values, `exp(log.est)`
#'   \item se - `exp(log.se)`
#'   \item CV - Lognormal CV, `sqrt(exp(log.se^2) - 1)`
#'   \item lo_90 - Lower 90% confidence bound
#'   \item hi_90 - Upper 90% confidence bound
#'   \item lo_95 - Lower 95% confidence bound
#'   \item hi_95 - Upper 95% confidence bound
#' }
#'
#' @md
#' @export

calc.uncertainty <- function(log.est = NULL,
                             log.se = NULL) {
  result <- cbind(log.est, log.se) %>%
    as.data.frame() %>%
    mutate(
      est = exp(log.est),
      se = exp(log.se),
      # Lognormal CV from the log-scale SE
      CV = sqrt(exp(log.se * log.se) - 1),
      # Confidence bounds are symmetric on the log scale, back-transformed with exp()
      lo_95 = exp(log.est - qnorm(0.975) * log.se), # 95% CI
      hi_95 = exp(log.est + qnorm(0.975) * log.se),
      lo_90 = exp(log.est - qnorm(0.95) * log.se), # 90% CI, needed for Mohn's rho adjustment
      hi_90 = exp(log.est + qnorm(0.95) * log.se)
    ) %>%
    select(est, se, CV, lo_90, hi_90, lo_95, hi_95)

  return(result)
}
