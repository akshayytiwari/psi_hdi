# ============================================================
# 07_eta_vs_phic.R
# ============================================================
# Description:
#   Plot Figure 3: eta versus phi_c
# ============================================================

library(ggplot2)

s_text <- 13
s_point <- 4
width_tick <- 0.6
length_tick <- 0.25
width_border <- 0.6

plot_eta_vs_phi_c <- function(df1, df2) {
  scale_vector <- quantile(df$eta, probs = c(0, 0.25, 0.5, 0.75, 1))
  ggplot() +
  geom_line(data = df1, aes(x = phi_c, y = eta), color = "black",
            linewidth = 1) +   
  geom_point(data = df2, aes(x = phi_c, y = eta, fill = eta), shape=21, color = "black",
             size = s_point, position = "jitter") +
  geom_text_repel(data = df2, aes(x = phi_c, y = eta, label = country, color = eta),
                  angle = 90,
                  nudge_x = 0.015,
                  size = 6,
                  min.segment.length = 1,
                  max.overlaps = Inf) +
  scale_fill_gradientn(colours = c("darkviolet", "darkblue", "dodgerblue", 
                                   "darkgreen", "gold", "chocolate", "firebrick3"),
                       values = scales::rescale(scale_vector)) +
  scale_colour_gradientn(colours = c("darkviolet", "darkblue", "dodgerblue", 
                                     "darkgreen", "gold", "chocolate", "firebrick3"),
                         values = scales::rescale(scale_vector)) + 
    labs(x = expression(Phi[c]), y = expression(eta ~ "(%)")) +
  theme_minimal() +
  theme(text = element_text(family = "Helvetica"),
        axis.text = element_text(size = s_text+2, colour = "black"),
        axis.title = element_text(size = s_text+2, colour = "black"),
        axis.ticks = element_line(linewidth = width_tick, colour = "black"),
        axis.ticks.length = unit(length_tick, "cm"),
        panel.border = element_rect(colour = "black", fill = NA, 
                                    size = width_border),
        legend.position = "none")
}
