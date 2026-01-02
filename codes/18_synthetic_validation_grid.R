# ============================================================
# 18_synthetic_validation_grid.R
# ============================================================
# Description:
#   Compare formalism (with Monte Carlo) against synthetic data
#   generated from parameter grid (ρ_c, ψ_c, φ_c, α, β).
# ============================================================

library(dplyr)
library(purrr)
library(tibble)

generate_synthetic_validation <- function(df_ref,
                                          weak_prior_fun,
                                          synthetic_fun,
                                          formalism_fun,
                                          kappa = 30,
                                          n_iter_mc = 1e5) {
  
  # Quantile values from empirical dataset
  alpha_val <- quantile(df_ref$alpha, probs = c(0.25, 0.5, 0.75))
  beta_val  <- quantile(df_ref$beta,  probs = c(0.25, 0.5, 0.75))
  rho_vals  <- quantile(df_ref$rho_c, probs = c(0.25, 0.5, 0.75))
  psi_vals  <- quantile(df_ref$psi_c, probs = c(0.25, 0.5, 0.75))
  phi_vals  <- quantile(df_ref$phi_c, probs = c(0.25, 0.5, 0.75))
  n_vals    <- c(1000, 5000, 10000)
  
  grid <- expand.grid(
    test_quality = c("Poor", "Moderate", "Good"),
    rho_c = rho_vals,
    psi_c = psi_vals,
    phi_c = phi_vals,
    n = n_vals
  )
  
  grid <- grid %>%
    mutate(
      alpha = case_when(
        test_quality == "Poor" ~ alpha_val[1],
        test_quality == "Moderate" ~ alpha_val[2],
        TRUE ~ alpha_val[3]
      ),
      beta = case_when(
        test_quality == "Poor" ~ beta_val[1],
        test_quality == "Moderate" ~ beta_val[2],
        TRUE ~ beta_val[3]
      )
    )
  
  # Estimate Beta priors for sensitivity/specificity
  alpha_params <- purrr::map_dfr(grid$alpha, ~ weak_prior(.x, kappa))
  beta_params  <- purrr::map_dfr(grid$beta,  ~ weak_prior(.x, kappa))
  
  grid <- bind_cols(grid,
                    alpha_a = alpha_params$alpha,
                    alpha_b = alpha_params$beta,
                    beta_a  = beta_params$alpha,
                    beta_b  = beta_params$beta)
  print(summary(grid[, c("alpha_a", "alpha_b", "beta_a", "beta_b")]))
  
  results <- vector("list", nrow(grid))
  set.seed(123)
  
  for (i in seq_len(nrow(grid))) {
    g <- grid[i, ]
    n <- g$n
    
    out_syn <- synthetic_fun(n, g$rho_c, g$psi_c, g$phi_c,
                             g$alpha_a, g$alpha_b, g$beta_a, g$beta_b)
    
    rho_c_obs <- (out_syn$n_sym_pos + out_syn$n_asym_pos) /
      sum(out_syn)
    psi_c_obs <- out_syn$n_asym_pos / (out_syn$n_sym_pos + out_syn$n_asym_pos)
    phi_c_obs <- out_syn$n_sym_neg / (out_syn$n_sym_neg + out_syn$n_asym_neg)
    
    out_form <- formalism_fun(
      n, out_syn$n_sym_pos + out_syn$n_asym_pos,
      out_syn$n_sym_neg + out_syn$n_asym_neg,
      n_iter_mc, rho_c_obs, psi_c_obs, phi_c_obs,
      g$alpha_a, g$alpha_b, g$beta_a, g$beta_b
    )
    
    psi_median <- quantile(out_form, 0.5)
    psi_lower  <- quantile(out_form, 0.25)
    psi_upper  <- quantile(out_form, 0.75)
    
    results[[i]] <- tibble(
      n = n,
      rho_true = g$rho_c,
      psi_true = g$psi_c,
      phi_true = g$phi_c,
      alpha_true = g$alpha,
      beta_true = g$beta,
      psi_form_median = psi_median,
      psi_form_lower  = psi_lower,
      psi_form_upper  = psi_upper,
      psi_form_delta  = (psi_true - psi_median) * 100 / psi_true,
      psi_form_width  = psi_upper - psi_lower
    )
  }
  
  df_out <- bind_rows(results)
  # write.csv(df_out, out_tab, row.names = FALSE)
  # message("Saved: ", out_tab)
  return(df_out)
}
