#' Fit hindcast model to data
#'
#' Refits a WHAM model with the terminal `peel` years of selected indices and
#' index age compositions switched off, as in a retrospective (hindcast) peel.
#'
#' @param model A WHAM model object (an rds from a WHAM fit) with random effects; the input data and parameter list are taken from it.
#' @param peel Integer. Number of years peeled from the terminal year, as in a retro.
#' @param drop A named list with `indices` and `index_paa`, where each element is a vector of index numbers (for abundance indices and age compositions, respectively) to drop in each hindcast. To not drop any indices or index_paa in a hindcast, set to `NA`.
#'
#' @return A WHAM model fit (as returned by `fit_wham()`).
#'
#' @examples
#' \dontrun{
#' fit_hindcast(model = WHAMRUN.rds, peel = 1, drop = list(indices = 1:2, index_paa = 1:2))
#' }
#' @export

fit_hindcast <- function(model, peel, drop) {
  # Start the refit from the original data, parameter values, and map;
  # random effects are kept as estimated in the original fit
  temp <- list(data = model$env$data, par = model$parList, map = model$env$map, random = unique(names(model$env$par[model$env$random])))
  nyrs <- temp$data$n_years_model
  # Turn off the dropped indices and age comps for the last `peel` years (JJD fix, Oct 31, 2023: +1 included per Tim's bug catch)
  temp$data$use_indices[(nyrs - peel + 1):nyrs, drop$indices] <- 0 ## JJD changed these 2 lines to include +1 per Tim bug catch Oct 31, 2023
  temp$data$use_index_paa[(nyrs - peel + 1):nyrs, drop$index_paa] <- 0
  mod <- fit_wham(temp, do.retro = FALSE, do.osa = FALSE)
  return(mod)
}
