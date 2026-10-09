#' @title Calculate P* from relative biomass and Council policy
#'
#' @description Calculates P* from biomass relative to target (B/Bmsy) using the MAFMC P* policy. P* is 0 at or below B/Bmsy = 0.1 (fishery closed), rises linearly to 0.45 at B/Bmsy = 1, then to 0.49 at B/Bmsy = 1.5, where it plateaus.
#' @author E Liljestrand, expanded code from M. Wilberg, originally 7-26-2011, updated with new MAFMC P* policy 5-10-2022
#'
#' @param relB Numeric. Single value of B/Bmsy (unitless). Must be a scalar, since it is used in `if` conditions.
#'
#' @return Numeric. P* value between 0 and 0.49.
#'
#' @examples
#' Pstar <- calc_pstar(1.55)
#' @export

calc_pstar <- function(relB) {
  if (relB >= 1.5) # at asymptote
    {
      P <- 0.49
    } else if (relB <= 0.1) # below level at which fisheries would be closed
    {
      P <- 0.0
    } else {
    if (relB < 1) # relative biomass between 0.1 and 1
      {
        P <- -0.05 + 0.5 * relB
      } else # relative biomass between 1 and 1.5
    {
      P <- 0.37 + 0.08 * relB
    }
  }
  return(P)
}
