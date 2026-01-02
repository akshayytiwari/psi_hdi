# ============================================================
# 02_generate_heatmap_data.R
# ============================================================
# Description:
#   Generates gridded data of corrected psi (ψ) values for
#   varying phi_c and psi_c, under combinations of
#   test sensitivities, specificities, and seroprevalence (rho_c).
# ============================================================

library(dplyr)
library(purrr)

generate_heatmap_data <- function(df) {
  alpha_q <- quantile(df$alpha, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
  beta_q  <- quantile(df$beta,  probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
  rho_vals <- quantile(df$rho_c, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
  
  psi_c_seq <- seq(min(df$psi_c, na.rm = TRUE), max(df$psi_c, na.rm = TRUE), length.out = 101)
  phi_c_seq <- seq(min(df$phi_c, na.rm = TRUE), max(df$phi_c, na.rm = TRUE), length.out = 101)
  
  build_heat_df <- function(rho_c_val, alpha_val, beta_val, label) {
    grid <- expand.grid(psi_c = psi_c_seq, phi_c = phi_c_seq) %>%
      dplyr::filter(psi_c < 1 - phi_c) %>%
      mutate(
        psi_hat = correction(rho_c_val, psi_c, phi_c, alpha_val, beta_val),
        rho_c = rho_c_val,
        test_quality = label
      )
    return(grid)
  }
  
  test_configs <- list(
    list(name = "Poor test", alpha = alpha_q[[1]], beta = beta_q[[1]]),
    list(name = "Moderate test", alpha = alpha_q[[2]], beta = beta_q[[2]]),
    list(name = "Good test", alpha = alpha_q[[3]], beta = beta_q[[3]])
  )
  
  heat_all <- purrr::map_dfr(rho_vals, function(rv) {
    purrr::map_dfr(test_configs, function(tc) {
      build_heat_df(rv, tc$alpha, tc$beta, tc$name)
    })
  })
  heat_all$test_quality <- factor(heat_all$test_quality, levels = test_levels)

  return(heat_all)
}
