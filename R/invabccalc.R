#' @title Inverse function to calculate P* from ABC and OFL
#'
#' @description Calculates the P* value implied by an ABC, given the OFL and the assumed CV of the OFL. This is the inverse of `ABC()` with respect to P*.
#' @author E Liljestrand, expanded code from M. Wilberg, originally 7-26-2011, updated with new MAFMC P* policy 5-10-2022
#'
#' @param ABC Numeric. Allowable biological catch, in the same units as `OFL`.
#' @param OFL Numeric. Overfishing limit for the stock, used as the lognormal median.
#' @param CV Numeric. Coefficient of variation of the OFL (unitless, e.g., 1.0).
#'
#' @return Numeric. P* value (probability between 0 and 1) implied by `ABC`.
#'
#' @examples
#' pstar <- inv_ABC(11456, 13234, 1.0)
#' @export

inv_ABC <- function(ABC, OFL, CV) {
  # Convert CV to the lognormal log-scale SD: sigma = sqrt(log(CV^2 + 1))
  sd <- sqrt(log(CV * CV + 1))

  # Evaluate the lognormal CDF at the ABC to recover P*
  return(plnorm(ABC, meanlog = log(OFL), sdlog = sd))
}
