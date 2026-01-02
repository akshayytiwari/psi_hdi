# ============================================================
# 09_misc_correlation.R
# ============================================================
# Description:
#   Plots correlation with sample size, symptom recall period,
#   number of symptoms, and median age of participants
# ============================================================

library(ggplot2)

s_text <- 13
s_point <- 3

width_tick <- 0.6
length_tick <- 0.25
width_border <- 0.6

my_theme <- theme(text = element_text(family = "Helvetica"),
                  axis.text = element_text(size = s_text, colour = "black"),
                  axis.title = element_text(size = s_text, colour = "black"),
                  axis.ticks = element_line(linewidth = width_tick, colour = "black"),
                  axis.ticks.length = unit(length_tick, "cm"),
                  panel.border = element_rect(colour = "black", fill = NA, 
                                              size = width_border),
                  legend.position = "none")

# Plotting psi vs sample size
plot_width_vs_n <- function(df) {
  ggplot(df, aes(x = n, y = rel_width_psi)) +
    geom_point(shape = 21, color = "black", fill = "black", size = s_point) +
    scale_x_continuous(trans = 'log10') +
    labs(x = "Sample size", y = "Uncertainty in psi") +
    theme_minimal() +
    my_theme
}

# Plotting psi vs symptom recall period
plot_recall_vs_psi <- function(df) {
  ggplot(df, aes(x = recall_period, y = psi)) +
    geom_point(size = 1.5) +
    scale_y_continuous(breaks = seq(0, 1, by = 0.25), limits = c(0, 1.03), 
                       expand = c(0, 0)) +
    scale_x_continuous(breaks = seq(0, 50, by = 10), limits = c(0, 54), 
                       expand = c(0, 0)) +
    labs(x = "Symptom recall period (weeks)", y = "psi") +
    theme_minimal() + 
    my_theme
}
  
# Symptom matrix heatmap
plot_symptom_matrix <- function(symptom_csv) {
  library(heatmaply)
  library(plotly)
  
  sym_mat <- read.csv(symptom_csv, header = TRUE,
                      check.names = FALSE,
                      stringsAsFactors = FALSE)
  
  sym_mat[-1] <- lapply(sym_mat[-1], function(x) ifelse(x %in% c("y", "Y"), 1, 0))
  sym_list <- sym_mat[, 1]
  rownames(sym_mat) <- sym_list
  sym_mat_filtered <- sym_mat[-1]
  
  p1 <- heatmaply(sym_mat_filtered, Colv = NA, Rowv = NA, margins = c(60, 50, 40, 20),
                             colors = YlOrRd, hide_colorbar = TRUE,
                             fontsize_row = 8, fontsize_col = 8,
                             grid_color = "black", grid_size = 0.01,
                             column_text_angle = 90, axis_text_color = "black")
  p1 <- p1 %>%
    layout(
      xaxis = list(
        titlefont = list(size = 16, color = "black", family = "Arial", bold = TRUE),
        tickfont  = list(size = 14, color = "black", family = "Arial", bold = TRUE)
      ),
      yaxis = list(
        titlefont = list(size = 16, color = "black", family = "Arial", bold = TRUE),
        tickfont  = list(size = 14, color = "black", family = "Arial", bold = TRUE)
      )
    )
  
  # heatmaply returns a plotly object; you can save as html
  # html_file <- file.path(out_fig_dir, "symptom_matrix_heatmap.html")
  # htmlwidgets::saveWidget(p, file = html_file)
}

# Plotting psi and phi_c vs number of symptoms
plot_sym_cor <- function(df, var, name) {
  ggplot(df, aes(x=num_sym, y=var)) +
    geom_point(size=s_point, colour='black')+
    scale_y_continuous(breaks=seq(0.25,1,by=0.25)) +
    scale_x_continuous(breaks=seq(4,16,by=4)) +
    labs(x = "Number of symptoms", y = name) +
    theme_minimal() + 
    my_theme
}

## Plotting psi and psi_c vs median age
plot_age_vs_psi <- function(df, var, name) {
  ggplot(df, aes(x=mean_median_age, y=var)) +
    geom_point(size=s_point, colour='black')+
    scale_y_continuous(breaks = seq(0, 1, by = 0.25), limits = c(0, 1.03), 
                       expand = c(0, 0), labels = c(0.00, 0.25, 0.50, 0.75, 1.00)) +
    labs(x = "Median age (years)", y = name) +
    theme_minimal() +
    my_theme
}
