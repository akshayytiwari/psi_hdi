# ============================================================
# 03_plot_heatmaps.R
# ============================================================
# Description:
#   Produces heatmaps showing corrected psi (ψ_hat)
#   across φ_c and ψ_c for multiple rho_c and test quality levels.
# ============================================================

library(ggplot2)

plot_heatmaps <- function(heat_all, s_text = 13) {
  ggplot(heat_all, aes(x = psi_c, y = phi_c, fill = psi_hat)) +
    geom_raster() +
    scale_fill_gradientn(
      colours = c("slateblue1", "seagreen3", "gold3", "tan3", "tomato3"),
      na.value = "cornsilk3", limits = c(0, 1),
      values = scales::rescale(c(0, 0.25, 0.5, 0.75, 1))
    ) +
    scale_x_continuous(breaks = seq(0, 1, 0.25), expand = c(0, 0),
                       limits = c(0, 1.03)) +
    scale_y_continuous(breaks = seq(0, 0.8, 0.2), expand = c(0, 0),
                       limits = c(0, 0.83)) +
    facet_grid(rows = vars(rho_c), cols = vars(test_quality)) +
    theme_minimal(base_size = 11) +
    theme(
      axis.text = element_text(size = 13, colour = "black"),
      panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.6),
      strip.text = element_text(size = 13),
      panel.spacing = unit(1, "lines")
    ) +
    labs(
      x = expression(psi[c]),
      y = expression(phi[c]),
      fill = expression(hat(psi))
    )
}
