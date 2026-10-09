#' Conduct the P* projections for the MAFMC (model output)
#'
#' Conducts the P* projections, which incorporate uncertainty and risk in the ABC calculation
#' from the OFL using a fitted WHAM model. Projects the stock forward, iteratively computing
#' the ABC for each projection year, and returns the final projection model.
#'
#' @author E Liljestrand 7-19-2024, expanded code provided by C Adams for butterfish 7-5-2022
#'
#' @param mod A WHAM fitted model object (rds from a WHAM fit) with `years` and projection-capable structure.
#' @param SSBmsy The SSBmsy proxy used to calculate the ratio between biomass and the reference point.
#' @param catch.year1 The amount of catch in the assessment year (usually the ABC).
#' @param projyr Number of years to project into the future (integer, default = 3).
#' @param CV Coefficient of variation of the OFL, as a proportion (e.g., 0.6), default = 1.5.
#' @param avg.abc Optional average ABC. If supplied, it replaces the calculated catch for the
#'   intermediate projection years; leave NULL (default) otherwise.
#'
#' @return A WHAM projection model object (from `project_wham()`) with projected values for the
#'   years following the model, based on P* catch advice.
#'
#' @examples
#' \dontrun{
#' mod <- readRDS("Modeling/Run3/fit.RDS")
#' SSBmsy <- 11225
#' catch.year1 <- 7557
#' CV <- 0.6
#' projyr <- 2
#' pstar_model <- pstarmodel(mod, SSBmsy, catch.year1, projyr, CV)
#' }
#' @export

pstarmodel <- function(mod = NULL, SSBmsy = NULL, catch.year1 = NULL, projyr = 3, CV = 1.5, avg.abc = NULL) {
  # Helper functions (abccalc, invabccalc, pstarcalc) are part of the package namespace

  # Specify the model and projection years
  model.years <- mod$years
  proj.years <- c((tail(model.years, 1) + 1):(tail(model.years, 1) + projyr))

  # Empty variables to collect catch and pstar values
  catch.proj <- rep(0, projyr)

  # Catch in the first year of projections is specified
  catch.proj[1] <- catch.year1

  if (projyr > 1) {
    for (i in 1:(projyr - 1))
    {
      # Tell WHAM to do projection based on catch in first i years ('5') and Fmsy proxy ('3') in following years
      proj_F_opt <- c(rep(5, i), rep(3, projyr - i))
      mod.proj <- project_wham(mod, proj.opts = list(n.yrs = projyr, proj_F_opt = proj_F_opt, proj_Fcatch = catch.proj), check.version = F)

      # Get OFL, SSB, and SSB ratio (relative to SSBMSY)
      ofl <- tail(apply(mod.proj$rep$pred_catch, 1, sum), projyr)[i + 1]
      ssb <- tail(apply(mod.proj$rep$SSB, 1, sum), projyr)[i]
      ssbratio <- ssb / SSBmsy

      # Calculate catch from p* code
      catch <- ABC(ofl, ssbratio, CV)

      # As long as its not the terminal year, add the abc catch to catch vector and repeat
      if (i < projyr) {
        catch.proj[i + 1] <- catch
        if (!is.null(avg.abc)) catch.proj[i + 1] <- avg.abc
      }
    }
  }

  # Conduct one last projection to get F and SSB resultant from final ABC
  proj_F_opt <- c(rep(5, projyr))
  mod.proj <- project_wham(mod, proj.opts = list(n.yrs = projyr, proj_F_opt = proj_F_opt, proj_Fcatch = catch.proj), check.version = F)

  # Return the final projection model
  return(mod.proj)
}
