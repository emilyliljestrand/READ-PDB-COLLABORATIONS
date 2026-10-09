#' @title Calculate ABC using a lognormal distribution
#'
#' @description Calculates acceptable biological catch (ABC) from the overfishing limit (OFL), biomass relative to target (B/Bmsy), and the assumed CV of the OFL. The P* value is obtained from `calc_pstar()` and used as a quantile of a lognormal distribution centred on the OFL.
#' @author E Liljestrand, expanded code from M. Wilberg, originally 7-26-2011, updated with new MAFMC P* policy 5-10-2022
#'
#' @param OFL Numeric. Overfishing limit for the stock, in catch units (e.g., metric tons). Used as the lognormal median.
#' @param relB Numeric. Biomass relative to target, B/Bmsy (unitless). Passed to `calc_pstar()`.
#' @param CV Numeric. Coefficient of variation of the OFL (unitless, e.g., 0.6).
#'
#' @return Numeric. ABC (allowable biological catch), in the same units as `OFL`.
#'
#' @examples
#' catch <- ABC(12345, 1.11, 0.6)
#' @export

ABC <- function(OFL, relB, CV) {
  # Convert CV to the lognormal log-scale SD: sigma = sqrt(log(CV^2 + 1))
  sd <- sqrt(log(CV * CV + 1))

  # P* is the probability that the true OFL falls below the ABC
  P <- calc_pstar(relB)

  # Calculate ABC as the P* quantile of the lognormal distribution (inverse CDF)
  return(qlnorm(P, meanlog = log(OFL), sdlog = sd))
}
