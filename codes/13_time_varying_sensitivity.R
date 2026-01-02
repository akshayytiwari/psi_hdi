#' Run synthetic data simulation with time-varying sensitivity
#'
#' @param df Data frame from prepare_pcr_sensitivity_data()
#' @param n, n_iter_mc Simulation size and Monte Carlo iterations
#' @param synthetic_fun Function for synthetic data generation
#' @param formalism_fun Function for analytical formalism estimation
#'
#' @return Data frame of ψ and ψ_delta summaries across days

library(dplyr)
library(readr)
library(purrr)
library(tibble)
simulate_time_varying_psi <- function(df,
                                      n = 1e5,
                                      n_iter_mc = 1e5,
                                      synthetic_fun,
                                      formalism_fun) {
  
  results_list <- vector("list", nrow(df))
  set.seed(123)
  
  for (i in seq_len(nrow(df))) {
    study <- df[i, ]
    rho_true <- study$rho_true
    psi_true <- study$psi_true
    
    sen_a <- study$alpha_a
    sen_b <- study$alpha_b
    spe_a <- study$beta_a
    spe_b <- study$beta_b
    
    # 1. Generate synthetic serosurvey data
    out_synthetic <- synthetic_fun(n, rho_true, psi_true, sen_a, sen_b, spe_a, spe_b)
    n_sym_pos  <- out_synthetic$n_sym_pos
    n_asym_pos <- out_synthetic$n_asym_pos
    n_sym_neg  <- out_synthetic$n_sym_neg
    n_asym_neg <- out_synthetic$n_asym_neg
    n_total    <- n_sym_pos + n_asym_pos + n_sym_neg + n_asym_neg
    
    rho_c <- (n_sym_pos + n_asym_pos) / n_total
    psi_c <- n_asym_pos / (n_sym_pos + n_asym_pos)
    
    # 2. Apply formalism Monte Carlo estimation
    psi_form <- formalism_fun(n, n_sym_pos + n_asym_pos,
                              n_iter_mc, rho_c, psi_c,
                              sen_a, sen_b, spe_a, spe_b)
    psi_delta <- abs(psi_true - psi_form) * 100 / psi_true
    
    results_list[[i]] <- tibble(
      day_inf = study$day_inf,
      n = n,
      alpha = study$alpha,
      alpha_l = study$alpha_l,
      alpha_u = study$alpha_u,
      psi = quantile(psi_form, 0.5),
      psi_l = quantile(psi_form, 0.25),
      psi_u = quantile(psi_form, 0.75),
      psi_delta_median = quantile(psi_delta, 0.5),
      psi_delta_lower  = quantile(psi_delta, 0.25),
      psi_delta_upper  = quantile(psi_delta, 0.75)
    )
  }
  
  results_df <- bind_rows(results_list)
  return(results_df)
}