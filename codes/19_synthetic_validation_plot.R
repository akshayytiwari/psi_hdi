# ============================================================
# 19_plot_formalism_validation.R
# ============================================================
# Description:
#   Plotting and summary utilities for formalism validation figures (S3a/b)
# ============================================================

library(ggplot2)
library(DescTools)
library(dplyr)

s_text <- 11
s_title <- 12
s_point <- 3
s_scatter <- 2
width_tick <- 0.6
length_tick <- 0.25
width_border <- 0.6

plot_validation_grid <- function(results_df, label = "synthetic") {
  results_df <- results_df %>% mutate(psi_true = as.numeric(psi_true))
  
  plot_grid <- ggplot(results_df, aes(x = psi_true, y = psi_form_median, group = psi_true)) +
    geom_abline(
      slope = 1, intercept = 0, linetype = "dashed",
      color = "black", linewidth = 0.8
    ) +
    geom_boxplot(
      width = 0.1,               # narrow boxes since x is continuous
      fill = "skyblue", alpha = 0.7,
      outlier.shape = 19, outlier.size = 1.5
    ) +
    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
    labs(
      x = expression(psi[true]),
      y = expression(hat(psi)[form]~"(estimated)")
    ) +
    theme_minimal() +
    theme(text = element_text(family = "Helvetica"),
          axis.title = element_blank(),
          axis.text = element_text(size = s_text, colour = "black"),
          axis.ticks = element_line(linewidth = width_tick, colour = "black"),
          axis.ticks.length = unit(length_tick, "cm"),
          panel.border = element_rect(colour = "black", fill = NA, 
                                      size = width_border),
          legend.position = "none")
  return(plot_grid)
}

plot_validation_studies <- function(results_df, label = "real") {
  plot_studies <- ggplot(results_df) +
    geom_abline(
      slope = 1, intercept = 0, linetype = "dashed",
      color = "black", linewidth = 0.8
    ) +
    geom_errorbar(aes(psi_true, ymin = psi_form_lower, ymax = psi_form_upper), 
                  width = 0.01, size = 0.5, alpha = 0.8, colour = "black") +
    geom_point(aes(x = psi_true, y = psi_form_median,), 
               shape = 21, fill = "lightblue", size = 3) + 
    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
    labs(x = expression("True " * psi), 
         y = expression("Estimated " * psi)) +
    theme_minimal() + 
    theme(text = element_text(family = "Helvetica"),
          axis.text = element_text(size = 11, colour = "black"),
          axis.title = element_blank(),
          axis.ticks = element_line(linewidth = 0.6, colour = "black"),
          axis.ticks.length = unit(0.25, "cm"),
          panel.border = element_rect(colour = "black", fill = NA, 
                                      size = 0.6),
          legend.position = "none")
  return(plot_studies)
}

summarize_formalism_accuracy <- function(results_df) {
  tibble(
    mean_abs_error = mean(abs(results_df$psi_form_delta), na.rm = TRUE),
    median_abs_error = median(abs(results_df$psi_form_delta), na.rm = TRUE),
    median_width = median(results_df$psi_form_width, na.rm = TRUE),
    coverage = mean(results_df$psi_true >= results_df$psi_form_lower &
                      results_df$psi_true <= results_df$psi_form_upper, na.rm = TRUE),
    ccc = CCC(results_df$psi_form_median, results_df$psi_true)$rho.c
  )
}
