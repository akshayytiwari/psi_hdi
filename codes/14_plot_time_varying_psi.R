# ============================================================
# 14_plot_time_varying_psi.R
# ============================================================
# Description:
#   Plot the variation in ψ error (ψ_delta) across days since infection.
# ============================================================

library(ggplot2)
library(dplyr)

plot_time_varying_psi <- function(results_df) {
  results_df <- results_df %>%
    mutate(day_inf = factor(day_inf, levels = unique(day_inf)))
  
  p <- ggplot(results_df) +
    geom_errorbar(aes(x = day_inf, ymin = psi_delta_lower, ymax = psi_delta_upper),
                  width = 0.1, size = 0.6, color = "black", alpha = 0.8) +
    geom_point(aes(x = day_inf, y = psi_delta_median),
               shape = 21, fill = "lightblue", size = 3) +
    scale_y_continuous(breaks = seq(0, 15, 5), limits = c(0, 16)) +
    labs(x = "Days since infection", y = expression(Delta~psi~" (%)")) +
    theme_classic(base_family = "Helvetica") +
    theme(
      axis.text = element_text(size = 11, colour = "black"),
      axis.ticks = element_line(linewidth = 0.6, colour = "black"),
      axis.ticks.length = unit(0.25, "cm"),
      panel.border = element_rect(colour = "black", fill = NA, size = 0.6),
      legend.position = "none"
    )
  
  return(p)
}
