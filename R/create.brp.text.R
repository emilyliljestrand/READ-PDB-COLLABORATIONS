#' @title Create BRP text
#'
#' @description Creates formatted text for a biological reference point (BRP), giving the prior assessment estimate and the current assessment estimate with its 95% confidence interval.
#'
#' @param brp.name Character. Type of BRP. Options include: "SSBproxy", "MSYproxy", and "Fproxy". No default.
#' @param round.digits Integer. Number of digits to round BRP values to, default = 3.
#' @param brp.table A data frame containing the following columns:
#' \itemize{
#'   \item BRP - Name of the BRP
#'   \item est - BRP estimates
#'   \item lo_95 - Lower 95\% confidence bound
#'   \item hi_95 - Upper 95\% confidence bound
#'   \item source - Model source, either "prior" for prior assessment or "MT" for current assessment
#' }
#'
#' @return A character vector of length 2. The first element is the rounded prior assessment estimate. The second is the current estimate formatted as "est (lo - hi)".
#' @export

create.brp.text <- function(brp.name = NULL,
                            round.digits = 3,
                            brp.table = NULL) {
  # Pull ref pt from model_prior (no CI reported for prior estimate)
  old.brp <- brp.table %>%
    filter(source == "prior", BRP == brp.name) %>%
    select(est) %>%
    round(., round.digits) %>%
    unlist()

  # Current assessment estimate with 95% CI, formatted as text
  # if(!brp.name == 'Fproxy'){ # Text for SSB and MSY proxies
  brp.ests <- brp.table %>% filter(source == "MT", BRP == brp.name) # Same formatting for all reference point text
  new.brp <- round(brp.ests$est, round.digits)
  lo.brp <- round(brp.ests$lo_95, round.digits)
  hi.brp <- round(brp.ests$hi_95, round.digits)
  output.text <- c(old.brp, paste(new.brp, " (", lo.brp, " - ", hi.brp, ")", sep = ""))
  # } else {
  #   new.brp <- brp.table %>% filter(source == "MT", BRP == brp.name) %>% select(est) %>% round(., round.digits)
  #   output.text <- as.character(c(old.brp, new.brp))
  # }

  # Return
  return(output.text)
}
