#' Plot NAA deviations from WHAM fit
#'
#' Plots numbers-at-age (NAA) random effect deviations from a WHAM fit as a heat map of
#' deviation by year and age.
#'
#' @param mods A WHAM fitted model object (rds from a WHAM fit) with NAA random effects. Must include `rep$NAA_devs`, `input$years_full`, `input$data$n_ages`, and `model_name`.
#' @param cor NULL - a placeholder for future labeling (stored in the plot data as `NAA_cor`, not displayed).
#'
#' @return A ggplot. Must use ggsave after running the function to save the plot.
#'
#' @examples
#' \dontrun{
#' naaplot <- plotNAAfxn(mods = mod, cor = "NA")
#' ggsave(paste("YOURNAME", "_NAAdevs.jpeg"), naaplot, path = YOURPATH)
#' }
#' @export

plotNAAfxn <- function(mods = NULL, cor = NULL) {
  df <- as.data.frame(mods$rep$NAA_devs)
  # NAA devs start in year 2, so year 1 is dropped from the years vector
  df$Year <- mods$input$years_full[2:length(mods$input$years_full)] # no devs in year 1
  colnames(df) <- c(paste0("Age_", 1:(mods$input$data$n_ages)), "Year")
  df$Model <- mods$model_name
  df$NAA_mod <- mods$model_name
  df$NAA_cor <- cor

  # Convert wide age columns to long format; Age is parsed to integer from the "Age_" prefix
  df.new <- df %>% tidyr::pivot_longer(-c(Year, Model, NAA_mod, NAA_cor),
    names_to = "Age",
    names_prefix = "Age_",
    names_transform = list(Age = as.integer),
    values_to = "NAA_Dev"
  )
  naadevplot <- ggplot(df.new, aes(x = Year, y = Age)) +
    geom_tile(aes(fill = NAA_Dev)) +
    # facet_wrap(~ NAA_cor) +
    scale_y_continuous(breaks = seq(1, 10, by = 1)) +
    # scale_fill_viridis_c() +
    # coord_equal() +
    scale_fill_gradient2(low = "royalblue", mid = "white", high = "red", midpoint = 0) +
    theme_bw() +
    theme(
      axis.text = element_text(size = 12, face = "bold"),
      axis.title = element_text(size = 15, face = "bold"),
      plot.title = element_text(size = 15, face = "bold"),
      strip.text = element_text(size = 15, face = "bold")
    )

  return(naadevplot)
}
