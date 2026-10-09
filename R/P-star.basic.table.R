#' Tabulate the P* projections for the MAFMC
#'
#' Conducts the P* projections, which incorporate uncertainty and risk in the ABC calculation
#' from the OFL using a fitted WHAM model, and summarizes them in a table (year, OFL, ABC,
#' B/BMSY, F, SSB, and P*).
#'
#' @author E Liljestrand 7-19-2024, expanded code provided by C Adams for butterfish 7-5-2022
#'
#' @param mod A WHAM fitted model object (rds from a WHAM fit) with `years` and projection-capable structure.
#' @param SSBmsy The SSBmsy proxy used to calculate the ratio between biomass and the reference point.
#' @param catch.year1 The amount of catch in the assessment year (usually the ABC).
#' @param projyr Number of years to project into the future (integer, default = 3).
#' @param CV Coefficient of variation of the OFL used in `ABC()` and `inv_ABC()` (default = 1.5).
#'   Units must match what `ABC()` expects (a proportion, e.g., 0.6); see the example.
#' @param avg.abc Optional average ABC. If supplied, it replaces the calculated catch for the
#'   intermediate projection years; leave NULL (default) otherwise.
#'
#' @return A data frame with one row per projection year and columns `Year`, `OFL`, `ABC`,
#'   `B/BMSY`, `F`, `SSB`, and `P*`. The first entry of `OFL` and `B/BMSY`/`P*` is `"NA"`
#'   because these are not computed for the first projection year.
#'
#' @examples
#' \dontrun{
#' mod <- readRDS("Modeling/Run3/fit.RDS")
#' SSBmsy <- 11225
#' catch.year1 <- 7557
#' CV <- 0.6  # ABC() expects a fraction (0.6 = 60%)
#' pstar60 <- pstartable(mod, SSBmsy, catch.year1, CV = CV)
#' write.csv(pstar60, "Projections/Pstar60.csv", row.names = FALSE)
#' }
#' @export

pstartable <- function(mod = NULL, SSBmsy = NULL, catch.year1 = NULL, projyr = 3, CV = 1.5, avg.abc = NULL) {
  # Helper functions (abccalc, invabccalc, pstarcalc) are part of the package namespace

  # Specify the model and projection years
  model.years <- mod$years
  proj.years <- c((tail(model.years, 1) + 1):(tail(model.years, 1) + projyr))

  # Empty variables to collect catch and pstar values for table
  # (ratio and P* have projyr - 1 entries because the terminal projection year has no P* value)
  catch.proj <- rep(0, projyr)
  ratio.proj <- rep(0, projyr - 1)
  pstar.proj <- rep(0, projyr - 1)
  ofl.proj <- rep(0, projyr)

  # Catch in the first year of projections is specified
  catch.proj[1] <- catch.year1

  if (projyr > 1) {
    for (i in 1:(projyr - 1))
    {
      # Tell WHAM to do projection based on catch in first i years ('5') and Fmsy proxy ('3') in following years
      proj_F_opt <- c(rep(5, i), rep(3, projyr - i))
      mod.proj <- project_wham(mod, proj.opts = list(n.yrs = projyr, proj_F_opt = proj_F_opt, proj_Fcatch = catch.proj), check.version = F)

      ofl <- tail(apply(mod.proj$rep$pred_catch, 1, sum), projyr)[i + 1]
      ssb <- tail(apply(mod.proj$rep$SSB, 1, sum), projyr)[i]
      ssbratio <- ssb / SSBmsy
      catch <- ABC(ofl, ssbratio, CV)

      ratio.proj[i] <- ssbratio
      pstar.proj[i] <- inv_ABC(catch, ofl, CV)
      ofl.proj[i] <- ofl

      if (i < projyr) {
        catch.proj[i + 1] <- catch
        if (!is.null(avg.abc)) catch.proj[i + 1] <- avg.abc
      }
    }
  }

  # Initiate table for memo with just year and OFL
  pstartable.df <- as.data.frame(cbind(c("NA", round(ofl.proj[-projyr], 0))))
  pstartable.df <- cbind(proj.years, pstartable.df)
  colnames(pstartable.df) <- c("Year", "OFL")

  # Add ABC
  pstartable.df <- cbind(pstartable.df, round(catch.proj, 0))
  colnames(pstartable.df)[3] <- "ABC"

  # Add B/BMSY
  ratio.proj <- c("NA", round(as.numeric(ratio.proj), 2))

  # Conduct one last projection to get F and SSB resultant from final ABC
  proj_F_opt <- c(rep(5, projyr))
  mod.proj <- project_wham(mod, proj.opts = list(n.yrs = projyr, proj_F_opt = proj_F_opt, proj_Fcatch = catch.proj), check.version = F)
  # F (log_F_tot is on the log scale, so exponentiate)
  f.proj <- tail(exp(mod.proj$rep$log_F_tot), (projyr))
  # SSB
  ssb.proj <- tail(apply(mod.proj$rep$SSB, 1, sum), projyr)

  pstartable.df <- cbind(pstartable.df, ratio.proj, f.proj, ssb.proj)
  colnames(pstartable.df)[4:6] <- c("B/BMSY", "F", "SSB")

  # Add P* values
  pstartable.df <- cbind(pstartable.df, c("NA", pstar.proj))
  colnames(pstartable.df)[7] <- "P*"

  return(pstartable.df)
}
