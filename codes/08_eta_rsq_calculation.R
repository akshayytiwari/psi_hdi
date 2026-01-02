# ============================================================
# 08_eta_rsq_calculation.R
# ============================================================
# Description:
#   Computes R² between modelled η (eta_model)
#   and Monte Carlo–derived η distributions across studies.
#   Based on Eq. (1) from Tiwari et al. (2025)
# ============================================================

library(dplyr)
library(purrr)
library(tibble)

#' Compute R² between modelled η and Monte Carlo η distributions
#'
#' @param df Data frame containing required columns:
#'        n, sp, sn, rho_c, psi_c, phi_c, alpha_a, alpha_b, beta_a, beta_b
#' @param monte_carlo_fun Monte Carlo function returning list with element `$eta`
#' @param n_iter Number of Monte Carlo iterations per study (default = 1000)
#' @param n_iter_rsq Number of samples for R² calculation (default = 1000)
#' @param seed Random seed for reproducibility (default = 123)
#'
#' @return A list with:
#'   - `eta_df`: Data frame with η quantiles and η_model
#'   - `r_sq`: Vector of R² values across iterations
#'   - `summary`: Median and IQR of R²
#'
#' @examples
#' res <- compute_eta_rsq(df, monte_carlo, n_iter = 1000, n_iter_rsq = 1000)
#' res$summary
#'
#' @export
compute_eta_rsq <- function(df, monte_carlo_fun,
                            n_iter = 1000, seed = 123) {
  
  n_est <- nrow(df)
  message("Running Monte Carlo for η across ", n_est, " studies...")
  
  # Preallocate matrix for η estimates
  eta_dis <- matrix(NA_real_, nrow = n_est, ncol = n_iter)
  
  # Monte Carlo η estimation per study
  for (i in seq_len(n_est)) {
    res <- monte_carlo_fun(
      n = df$n[i], sp = df$sp[i], sn = df$sn[i], n_iter = n_iter,
      rho_c = df$rho_c[i], psi_c = df$psi_c[i], phi_c = df$phi_c[i],
      alpha_a = df$alpha_a[i], alpha_b = df$alpha_b[i],
      beta_a = df$beta_a[i], beta_b = df$beta_b[i]
    )
    eta_dis[i, ] <- res$eta
  }
  
  # Model-based η prediction
  eta_model <- 100 * df$phi_c / (1 - df$phi_c)

  # Compute R² distribution
  eta_mean <- colMeans(eta_dis, na.rm = TRUE)
  rss <- colSums((eta_model - eta_dis)^2, na.rm = TRUE)
  tss <- colSums((eta_dis - matrix(eta_mean, nrow = n_est, ncol = n_iter, byrow = TRUE))^2, na.rm = TRUE)
  r_sq <- 1 - rss / tss
  
  # Summary stats
  median_rsq <- median(r_sq, na.rm = TRUE)
  iqr_rsq <- quantile(r_sq, probs = c(0.25, 0.75), na.rm = TRUE)
  summary_stats <- tibble(
    median_rsq = round(median_rsq, 3),
    iqr_lower = round(iqr_rsq[1], 3),
    iqr_upper = round(iqr_rsq[2], 3),
    iqr_width = round(iqr_rsq[2] - iqr_rsq[1], 3)
  )
  
  message("R² median: ", summary_stats$median_rsq,
          " (IQR: ", summary_stats$iqr_lower, "–", summary_stats$iqr_upper, ")")
  
  list(r_sq = r_sq, summary = summary_stats)
}
