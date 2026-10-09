#' Calculate relative error for a WHAM self-test
#'
#' @description
#' Calculates the relative error, (Est - True) / True, for SSB, F (Fbar), or recruitment across all self-test simulations, along with the mean, median, and approximate 95% CI of the error by year. Called internally by `multi_selftestplot()`; users should not need to call it directly.
#'
#' @param metric A string, one of `"SSB"`, `"Fbar"`, or `"Rec"`.
#' @param mod Result of `fit_wham()`, providing the true values in `rep` and years in `input`.
#' @param selftest Result of the self-test, where `selftest[[1]]` is a list of simulation reports.
#'
#' @return A data frame with columns `mean`, `median`, `Year`, `low`, and `hi`, one row per year.
#' @export

# Relative error for SSB, F and Rec.
selftesterrorfxn <- function(metric = NULL, mod = NULL, selftest = NULL) {
  # Each branch: relative error per simulation, then summary across simulations by year.
  # The CI uses mean +/- qnorm(0.975) * sd / sqrt(length(selftest[[1]])); the number of simulations is taken from `selftest`.
  if (metric == "SSB") {
    true <- mod$rep[metric]
    true <- true$SSB
    est <- sapply(selftest[[1]], function(x) {
      return(x["SSB"])
    })
    est <- matrix(unlist(est), ncol = length(est), byrow = FALSE)
    rel_resid <- apply(est, 2, function(x) x / true - 1)
    mean.metric <- apply(rel_resid, 1, mean)
    median.metric <- apply(rel_resid, 1, median)
    res <- data.frame("mean" = mean.metric, "median" = median.metric, "Year" = mod$input$years)
    resid_cis <- apply(rel_resid, 1, mean) + apply(rel_resid, 1, sd) * qnorm(0.975) * t(matrix(c(-1, 1), 2, length(true))) / sqrt(length(selftest[[1]]))
    res <- data.frame(res, "low" = resid_cis[, 1], "hi" = resid_cis[, 2])
  }
  if (metric == "Fbar") { # end if metric
    true <- mod$rep[metric]
    true <- true$Fbar
    est <- sapply(selftest[[1]], function(x) {
      return(x["F"])
    })
    est <- matrix(unlist(est), ncol = length(est), byrow = FALSE)
    rel_resid <- apply(est, 2, function(x) x / true - 1)
    mean.metric <- apply(rel_resid, 1, mean)
    median.metric <- apply(rel_resid, 1, median)
    res <- data.frame("mean" = mean.metric, "median" = median.metric, "Year" = mod$input$years)
    resid_cis <- apply(rel_resid, 1, mean) + apply(rel_resid, 1, sd) * qnorm(0.975) * t(matrix(c(-1, 1), 2, length(true))) / sqrt(length(selftest[[1]]))
    res <- data.frame(res, "low" = resid_cis[, 1], "hi" = resid_cis[, 2])
  }
  if (metric == "Rec") {
    true <- mod$rep["NAA"][[1]][1, 1, , 1]
    est <- sapply(selftest[[1]], function(x) {
      return(x$NAA[1, 1, , 1])
    })
    est <- matrix(unlist(est), ncol = ncol(est), byrow = FALSE)
    rel_resid <- apply(est, 2, function(x) x / true - 1)
    mean.metric <- apply(rel_resid, 1, mean)
    median.metric <- apply(rel_resid, 1, median)
    res <- data.frame("mean" = mean.metric, "median" = median.metric, "Year" = mod$input$years)
    resid_cis <- apply(rel_resid, 1, mean) + apply(rel_resid, 1, sd) * qnorm(0.975) * t(matrix(c(-1, 1), 2, length(true))) / sqrt(length(selftest[[1]]))
    res <- data.frame(res, "low" = resid_cis[, 1], "hi" = resid_cis[, 2])
  } # end rec
  return(res)
}

#' Plot and save self-test relative error for a WHAM model
#'
#' @description
#' Automated creation of plots from a (multi)WHAM simulation self-test. Main plot shows mean, median, and 95% CI of relative error for SSB, F, and recruitment. Uses `selftesterrorfxn()`.
#'
#' @param mod Result of `fit_wham()`.
#' @param selftest Result of the self-test applied to `mod`.
#' @param pngprefix Default NULL. Prefix added to default names for all saved PNG files.
#' @param save.direct Directory where the PNG plots are saved.
#' @return A list of 3 ggplot objects (SSB, F, recruitment). Also saves 3 PNG files to `save.direct`.
#' @noRd

# Plot and save relative error of SSB, F, and rec.
multi_selftestplot <- function(mod = NULL, selftest = NULL, pngprefix = NULL, save.direct = NULL) {
  # ---- Summarise relative error for each metric ----
  ssb <- selftesterrorfxn(metric = "SSB", mod = mod, selftest = selftest)
  FishMort <- selftesterrorfxn(metric = "Fbar", mod = mod, selftest = selftest)
  Rec <- selftesterrorfxn(metric = "Rec", mod = mod, selftest = selftest)

  figs <- list()

  figs[[1]] <- ggplot(ssb, aes(x = Year, y = mean)) +
    geom_line() +
    geom_line(aes(x = Year, y = median), color = "blue") +
    geom_ribbon(aes(ymin = low, ymax = hi), alpha = 0.3) +
    geom_hline(yintercept = 0, col = "red") +
    # facet_wrap(~VAR, ncol = 3) +
    ylab("Relative Error in SSB") +
    ggtitle("Self-test mean (black) and median (blue) bias with 95% CIs") +
    theme_bw()

  figs[[2]] <- ggplot(FishMort, aes(x = Year, y = mean)) +
    geom_line() +
    geom_line(aes(x = Year, y = median), color = "blue") +
    geom_ribbon(aes(ymin = low, ymax = hi), alpha = 0.3) +
    geom_hline(yintercept = 0, col = "red") +
    # facet_wrap(~VAR, ncol = 3) +
    ylab("Relative Error in F") +
    ggtitle("Self-test mean (black) and median (blue) bias with 95% CIs") +
    theme_bw()

  figs[[3]] <- ggplot(Rec, aes(x = Year, y = mean)) +
    geom_line() +
    geom_line(aes(x = Year, y = median), color = "blue") +
    geom_ribbon(aes(ymin = low, ymax = hi), alpha = 0.3) +
    geom_hline(yintercept = 0, col = "red") +
    # facet_wrap(~VAR, ncol = 3) +
    ylab("Relative Error in Recruitment") +
    ggtitle("Self-test mean (black) and median (blue) bias with 95% CIs") +
    theme_bw()

  ggsave(filename = paste0(pngprefix, "selftest_ssb.png"), path = save.direct, figs[[1]], width = 6, height = 4, units = "in")
  ggsave(filename = paste0(pngprefix, "selftest_F.png"), path = save.direct, figs[[2]], width = 6, height = 4, units = "in")
  ggsave(filename = paste0(pngprefix, "selftest_rec.png"), path = save.direct, figs[[3]], width = 6, height = 4, units = "in")

  return(figs)
}
