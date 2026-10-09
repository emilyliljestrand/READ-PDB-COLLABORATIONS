#' Simulation self-test of a WHAM model
#'
#' Performs a simulation self-test of a WHAM model: data are simulated from the
#' fitted model, refit, and the true and estimated F, recruitment (R), and SSB
#' are compared for each simulation.
#'
#' @param mod A WHAM model object created with `fit_wham()`.
#' @param n_sim Integer. Number of simulations to perform.
#'
#' @return A data frame of annual true (`TRU`) and estimated (`EST`) F, R, and SSB, one row per simulation, year, and variable (`VAR`), with a `converged` flag per simulation.
#' @export
do_selftest <- function(mod, n_sim) {
  # Simulate n_sim datasets from the fitted model
  sim_inputs <- replicate(n_sim, sim_fn(mod), simplify = FALSE)
  res <- list(
    reps = list(), par.est = list(), par.se = list(),
    adrep.est = list(), adrep.se = list()
  )
  j <- 1
  for (i in seq_along(sim_inputs)) {
    cat(paste0("sim ", i, " of ", n_sim, "\n"))
    sim_inputs[[i]]$SIM_ID <- i
    tfit <- NULL
    # Failed fits are reported and skipped; they are not stored in the results
    tryCatch(
      expr = {
        tfit <- fit_wham(sim_inputs[[i]],
          do.osa = FALSE,
          do.retro = FALSE, MakeADFun.silent = TRUE, do.sdrep = FALSE
        )
      },
      error = function(e) {
        message("An error occurred:\n", e)
      },
      finally = {
        if (is.null(tfit)) {

        } else {
          # Record convergence: convergence code 0 means the optimizer converged
          conv <- check_convergence(tfit, ret = T)
          if (conv$convergence == 0) {
            conv$converged <- TRUE
          } else {
            conv$converged <- FALSE
          }
          res$reps[[j]] <- tfit$rep
          res$reps[[j]]$converged <- conv$converged
          res$reps[[j]]$SIM_ID <- i
          res$reps[[j]]$years <- tfit$years
          j <- j + 1
        }
      }
    )
  }

  # True values come from the simulated inputs; estimates from the fits
  true_ssb <- map_df(sim_inputs, function(x) {
    data.frame(
      SIM_ID = x$SIM_ID,
      YEAR = x$years,
      VAR = "SSB",
      TRU = x$data$SSB
    )
  })

  true_f <- map_df(sim_inputs, function(x) {
    data.frame(
      SIM_ID = x$SIM_ID,
      YEAR = x$years,
      VAR = "F",
      TRU = x$data$`F`
    )
  })

  # Recruitment is the age-1 column of NAA
  true_r <- map_df(sim_inputs, function(x) {
    data.frame(
      SIM_ID = x$SIM_ID,
      YEAR = x$years,
      VAR = "R",
      TRU = x$data$NAA[, 1]
    )
  })

  est_ssb <- map_df(res$reps, function(x) {
    data.frame(
      SIM_ID = x$SIM_ID,
      YEAR = x$years,
      VAR = "SSB",
      EST = x$SSB
    )
  })

  est_f <- map_df(res$reps, function(x) {
    data.frame(
      SIM_ID = x$SIM_ID,
      YEAR = x$years,
      VAR = "F",
      EST = x$`F`
    )
  })

  est_r <- map_df(res$reps, function(x) {
    data.frame(
      SIM_ID = x$SIM_ID,
      YEAR = x$years,
      VAR = "R",
      EST = x$NAA[, 1]
    )
  })

  conv_df <- map_df(res$reps, function(x) {
    data.frame(
      SIM_ID = x$SIM_ID,
      converged = x$converged
    )
  })

  # Join all sims
  sim_out <-
    left_join(true_ssb, est_ssb) %>%
    bind_rows({
      left_join(true_f, est_f)
    }) %>%
    bind_rows({
      left_join(true_r, est_r)
    }) %>%
    left_join(conv_df)

  return(sim_out)
}
