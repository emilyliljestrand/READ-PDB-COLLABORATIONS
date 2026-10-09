#' @title Calculate the amount of fish dying given M
#'
#' @description Calculates the amount (000s metric tons) of fish dying each year given the assumed or estimated M. Uses Jan-1 weight-at-age, so the result is an approximation.
#'
#' @param mod A WHAM fitted model object (the result of `fit_wham()`, typically read from an rds file). Must contain `rep` (MAA, ZAA, NAA) and `input` (data, years).
#'
#' @return A data.frame with two columns: `Year` and `Deaths` (000s metric tons), one row per model year.
#'
#' @examples
#' \dontrun{
#' mod.dir <- "m168"
#' write.dir <- paste("C:/Herring/2022 Assessment/Assessments/WHAM", mod.dir, sep = "/")
#' setwd(write.dir)
#' mod <- readRDS(paste0(mod.dir, ".rds"))
#' m168deaths <- Mdeaths(mod = mod)
#' plot(m168deaths$Year, m168deaths$Deaths, type = "o", ylab = "Dead Fish (000's mt)", xlab = "")
#' }
#' @export

Mdeaths <- function(mod = NULL) {
  MAA <- mod$rep$MAA
  ZAA <- mod$rep$ZAA
  NAA <- mod$rep$NAA
  # Jan-1 weight-at-age, selected through the WHAM weight pointer
  WAA.pt <- mod$input$data$waa_pointer_jan1
  WAA <- mod$input$data$waa[WAA.pt, , ]

  # Baranov catch equation: fraction removed by fishing from total mortality (M/Z)(1 - exp(-Z)),
  # times numbers and weight; divide by 1000 to convert to 000s of metric tons
  cons.aa <- ((MAA / ZAA) * (1 - exp(-ZAA)) * NAA * WAA) / 1000
  # Sum over ages for each year
  cons.annual <- data.frame(Year = mod$input$years, Deaths = rowSums(cons.aa))
  return(cons.annual)
}
