#' Calculate annual catch- and weight-at-age
#'
#' Calculates annual catch-at-age (number of fish) and weight-at-age (mean weight, kg) by combining expanded lengths from stockEff with an age-length key (ALK). Time blocking matches the stockEff blocking.
#'
#' NOTE: If you receive the following error try setting connection = NULL and rerun function to re-establish oracle connection:
#'       Error: unable to find an inherited method for function 'dbGetQuery' for signature 'conn = "NULL", statement = "character"'
#'
#' @param ALK A long-format table containing the following columns:
#' \itemize{
#'   \item YEAR
#'   \item LENGTH - Length bin
#'   \item AGE - Age bin
#'   \item PROP - proportion-at-age for a given length and YEAR
#'   \item BLOCK_ID - Block ID used to match ALK to expanded lengths from stockEff
#' }
#' @param stockEff_mode String specifying version of stockEff to query, options include: "test", "prod". Default = "prod"
#' @param stockEff_module A string indicating the module for which products will be pulled, no default. Options include:
#' \itemize{
#'   \item "survey" - Correspond to SV tab in stockEff
#'   \item "commercial" - Correspond to CF tab in stockEff
#'   \item "observer" - Correspond to OB tab in stockEff
#'   \item Nothing yet available for MRIP tab (as of 3/6/24)
#' }
#' @param species_itis Species ITIS code (numeric or character), used to filter the stockEff query.
#' @param connection A DBI or ROracle connection object to the NEFSC Oracle database.
#'        If NULL (default), the function will prompt for credentials via rstudioapi
#'        and manage the connection/disconnection automatically. If a connection
#'        is provided, the user is responsible for closing it after the function executes.
#'
#' @return A data frame with one row per `YEAR` and `AGE`, containing `TOT_NO_AT_AGE` (expanded number of fish), `TOT_WT_AT_AGE` (total weight, kg), and `AVG_WT_KG` (mean weight-at-age, kg).
#' @export

calc_caa_waa <- function(ALK = NULL,
                         stockEff_mode = "prod",
                         stockEff_module = NULL,
                         species_itis = NULL,
                         connection = NULL) {
  # ---- Setup ----
  # "test" queries the pre-production stockEff schema (suffix "_pre_prod")
  if (stockEff_mode == "test") {
    mode_abbrev <- "_pre_prod"
  } else {
    mode_abbrev <- ""
  }

  # Establish Oracle connection if not already supplied via `connection`
  created_connection <- is.null(connection)
  if (created_connection) {
    connection <- dbConnect(
      drv = dbDriver("Oracle"),
      username = rstudioapi::askForPassword("Oracle user name"),
      password = rstudioapi::askForPassword("Oracle password"),
      dbname = rstudioapi::askForPassword("Oracle database name")
    )
  }

  # ---- Pull lengths by year, NESPP4, region, and stockEff block ----
  # Result includes length-weight parameters associated with each block
  if (stockEff_module == "commercial") { # Query commercial landings
    mv_noatlen <- ROracle::dbGetQuery(connection, statement = paste0("select * from stockeff", mode_abbrev, ".mv_cf_wgt_and_no_at_length_j where species_itis = '", species_itis, "'")) %>%
      mutate(
        YEAR = as.numeric(YEAR),
        LENGTH = as.numeric(LENGTH)
      )
  } else if (stockEff_module == "observer") { # Query commercial discards
    mv_noatlen <- ROracle::dbGetQuery(connection, statement = paste0("select * from stockeff", mode_abbrev, ".MV_OB_WTG_AND_NO_AT_LENGTH_J where species_itis = '", species_itis, "'")) %>%
      mutate(
        YEAR = as.numeric(YEAR),
        LENGTH = as.numeric(LENGTH)
      )
  } else {
    stop("stockEff_module must be \"commercial\" or \"observer\"")
  }

  # Disconnect from Oracle once query complete (also runs when a user-supplied connection is passed)
  if (created_connection) ROracle::dbDisconnect(connection)


  # ---- Calculate annual catch- and weight-at-age ----
  caa_waa <- left_join(mv_noatlen, ALK, by = c("YEAR", "BLOCK_ID", "LENGTH"), relationship = "many-to-many") %>%
    # many-to-many because prop-at-age provided for each length in long form so multiple ages for each length
    # NA in AGE means there is a gap in the provided ALK (observed length in catch but no age assigned)
    #
    mutate(
      NO_AT_LENGTH_AGE = NO_AT_LENGTH * PROP, # Age expansion by stockEff block
      WT_AT_LENGTH_AGE = NO_AT_LENGTH_AGE * IND_AVG_WT_KG # Weight from length-weight relationship, per block
    ) %>%
    group_by(YEAR, AGE) %>% # Calculate annual catch- and weight-at-age
    dplyr::reframe(
      TOT_NO_AT_AGE = sum(NO_AT_LENGTH_AGE), # Annual number of fish at AGE based on ALK
      TOT_WT_AT_AGE = sum(WT_AT_LENGTH_AGE)  # Annual total weight at AGE based on ALK
    ) %>%
    mutate(AVG_WT_KG = TOT_WT_AT_AGE / TOT_NO_AT_AGE)

  # Return
  return(caa_waa)
}
