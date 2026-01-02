# ============================================================
# 05_plot_distributions.R
# ============================================================
# Description:
#   Plot utilities for violin plots, histograms, and scatter plots
# ============================================================

library(ggplot2)
library(ggrepel)
library(dplyr)
library(reshape2)

s_text <- 15
s_title <- 12
s_point <- 5
s_scatter <- 3
width_tick <- 0.6
length_tick <- 0.25
width_border <- 0.6

my_theme <- theme(
  text = element_text(family = "Helvetica"),
  axis.title = element_blank(),
  axis.text = element_text(size = s_text, colour = "black"),
  axis.ticks = element_line(linewidth = width_tick, colour = "black"),
  axis.ticks.length = unit(length_tick, "cm"),
  panel.border = element_rect(colour = "black", fill = NA, 
                              size = width_border),
  legend.position = "none"
)

plot_violin <- function(data, var_name, fill_color) {
  ggplot(data %>% filter(variable == var_name), aes(x = variable, y = value)) +
    geom_violin(trim = TRUE, scale = "width",
                width = 1, size = 1, alpha = 0.3, fill = fill_color) +
    geom_boxplot(width = 0.5, color = "black", outlier.shape = NA, 
                 alpha = 0.5, fill = fill_color, size = 1, fatten = 1.25) +
    geom_jitter(shape = 21, width = 0.5, size = s_scatter-1, fill = fill_color,
                color = "black", alpha = 1) +
    labs(y = expression(Phi[c])) +
    theme_minimal() +
    my_theme
}

plot_violin_combined <- function(df_long) {
  ggplot(df_long[df_long$variable %in% c("psi_c", "psi"), ],
         aes(x = variable, y = value, fill = variable)) +
    geom_violin(position = position_dodge(width = 0.8), 
                trim = TRUE, scale = "width",
                width = 0.8, size = 1, alpha = 0.3) +
    geom_boxplot(width = 0.4, color = "black", outlier.shape = NA, 
                 alpha = 0.5, size = 1, fatten=1.25) +
    scale_fill_manual("values" = c("psi_c" = "orange", "psi" = "red")) +
    geom_jitter(shape = 21, width = 0.2, size = s_scatter, color = "black") +
    theme_minimal() +
    my_theme
}

plot_eta_histogram <- function(df) {
  ggplot(df, aes(x = eta_range)) +
    geom_bar(fill = "skyblue", color = "black", width = 1) +
    scale_x_discrete(breaks = levels(df$eta_range)) +
    labs(x = expression(paste(eta, " (%)"))) +
    theme_minimal() +
    my_theme
}

plot_psi_scatter <- function(df) {
  scale_vector <- quantile(df$eta, probs = c(0, 0.25, 0.5, 0.75, 1))
  ggplot(df) +
    geom_line(aes(x = psi_c, y = psi_c),
              color = "black", linetype = "dashed", size = 1) +
    geom_errorbar(aes(x = psi_c, ymin = psi_l, ymax = psi_u),   
                  color = "black", width = 0.01, size = 0.5, alpha = 0.8) +
    geom_point(aes(x = psi_c, y = psi, fill = eta), 
               shape = 21, color = "black", size = s_point) +
    geom_text_repel(data = df, aes(x = psi_c, y = psi, label = country, 
                                   colour = eta), angle = 90, max.overlaps = Inf,
                    size = 6, nudge_y = 0.01) +
    scale_x_continuous(breaks = seq(0, 1, by = 0.25), limits = c(0, 1.02), 
                       expand = c(0, 0)) +
    scale_y_continuous(breaks = seq(0, 1, by = 0.25), limits = c(0, 1.03), 
                       expand = c(0, 0)) +
    scale_fill_gradientn(colours = c("darkviolet", "darkblue", "dodgerblue", 
                                     "darkgreen", "gold", "chocolate", "firebrick3"),
                         values = scales::rescale(scale_vector)) +
    scale_colour_gradientn(colours = c("darkviolet", "darkblue", "dodgerblue",
                                       "darkgreen", "gold", "chocolate", "firebrick3"),
                           values = scales::rescale(scale_vector)) +
    labs(x = expression(italic(psi[c])), y = expression(italic(psi))) +
    theme_minimal() +
    my_theme
}
