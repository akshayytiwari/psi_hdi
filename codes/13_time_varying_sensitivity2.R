# ============================================================
# simulate_time_varying_psi.R
# ============================================================

library(dplyr)
library(purrr)
library(tibble)

#' Simulate ψ (asymptomatic proportion) under time-varying sensitivity
#'
#' @param df Data frame containing columns rho_true, psi_true, alpha_a, alpha_b, beta_a, beta_b, etc.
#' @param n Number of individuals to simulate per study/day
#' @param n_iter_mc Number of Monte Carlo iterations for analytical formalism
#' @param synthetic_fun Function generating synthetic counts (e.g., synthetic_zero_phi)
#' @param formalism_fun Function estimating corrected ψ via Monte Carlo (e.g., monte_carlo_no_phi)
#'
#' @return A tibble containing ψ estimates and uncertainty summaries
#' @export
simulate_time_varying_psi <- function(df,
                                      n = 1e5,
                                      n_iter_mc = 1e5,
                                      synthetic_fun,
                                      formalism_fun) {
  
  set.seed(123)
  
  df %>%
    mutate(
      # 1️⃣ Simulate synthetic datasets row-wise
      count = pmap(
        list(rho_true, psi_true, alpha_a, alpha_b, beta_a, beta_b),
        ~ synthetic_fun(
          n = n,
          rho = ..1,
          psi = ..2,
          alpha_a = ..3,
          alpha_b = ..4,
          beta_a  = ..5,
          beta_b  = ..6
        )
      ),
      
      # 2️⃣ Run analytical Monte Carlo correction for each simulated dataset
      psi_dis = pmap(
        list(count, alpha_a, alpha_b, beta_a, beta_b),
        ~ {
          sp <- ..1$n_sym_pos + ..1$n_asym_pos
          rho_c <- sp / n
          psi_c <- ..1$n_asym_pos / sp
          
          formalism_fun(
            n = n,
            sp = sp,
            n_iter = n_iter_mc,
            rho_c = rho_c,
            psi_c = psi_c,
            alpha_a = ..2,
            alpha_b = ..3,
            beta_a  = ..4,
            beta_b  = ..5
          )
        }
      ),
      
      # 3️⃣ Compute relative deviation ψΔ
      delta_dis = map2(psi_true, psi_dis, ~ abs(.x - .y) * 100 / .x),
      
      # 4️⃣ Summarize ψ and ψΔ with median and IQRs
      psi              = map_dbl(psi_dis,   ~ quantile(.x, 0.5, na.rm = TRUE)),
      psi_l            = map_dbl(psi_dis,   ~ quantile(.x, 0.25, na.rm = TRUE)),
      psi_u            = map_dbl(psi_dis,   ~ quantile(.x, 0.75, na.rm = TRUE)),
      psi_delta_median = map_dbl(delta_dis, ~ quantile(.x, 0.5, na.rm = TRUE)),
      psi_delta_lower  = map_dbl(delta_dis, ~ quantile(.x, 0.25, na.rm = TRUE)),
      psi_delta_upper  = map_dbl(delta_dis, ~ quantile(.x, 0.75, na.rm = TRUE))
    ) %>%
    
    # 5️⃣ Select and arrange output columns neatly
    select(
      day_inf, n, alpha, alpha_l, alpha_u,
      psi, psi_l, psi_u,
      psi_delta_median, psi_delta_lower, psi_delta_upper
    )
}
