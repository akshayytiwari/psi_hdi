# ============================================================
# beta_dis.R
# ============================================================
# Estimate Beta(a, b) parameters from three quantiles
# using a fixed grid search.
# ============================================================
library(stats)

estimate_beta <- function(q_vals, sen_range = c(0.5, 100), spe_range = c(0.5, 100)) {
  best_loss <- Inf
  best_params <- c(NA, NA)
  p_vals <- c(0.025, 0.5, 0.975)
  for (a in seq(sen_range[1], sen_range[2], length.out = 1000)) {
    for (b in seq(spe_range[1], spe_range[2], length.out = 1000)) {
      q_model <- qbeta(p_vals, a, b)
      loss <- sum((q_model - q_vals)^2)
      if (loss < best_loss) {
        best_loss <- loss
        best_params <- c(a, b)
      }
    }
  }
  return(data.frame(sen = best_params[1], spe = best_params[2]))
}
