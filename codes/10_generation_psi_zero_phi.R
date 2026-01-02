# ============================================================
# 10_generation_psi_zero_phi.R
# ============================================================
# Description:
#   Generate synthetic survey data when φ_c = 0
#   (typical for PCR-based studies).
# ============================================================

library(dplyr)

#' Generate synthetic data with φ_c = 0
#'
#' @param n Number of individuals to simulate
#' @param rho True infection prevalence
#' @param psi True asymptomatic proportion
#' @param alpha_a,beta_a Alpha parameters for Beta prior of sensitivity/specificity
#' @param alpha_b,beta_b Beta parameters for Beta prior of sensitivity/specificity
#'
#' @return A data frame with counts of symptomatic/asymptomatic positives/negatives
#'
#' @export
# Code for generating synthetic data for the case when phi_c is zero. It usually happens for PCR cases

synthetic_zero_phi <- function (n, rho, psi, alpha_a, alpha_b, beta_a, beta_b) {
  x_sym_pos <- x_asym_pos <- x_sym_neg <- x_asym_neg <- integer(n)
  set.seed(123)
  for (j in 1:n) {
    infected <- rbinom(1, 1, rho)
    if (infected == 1) {
      has_symptoms_x <- rbinom(1, 1, 1 - psi)    # symptoms due to x
      test_positive <- rbinom(1, 1, rbeta(1, alpha_a, alpha_b))
    } else {
      has_symptoms_x <- 0
      test_positive <- rbinom(1, 1, 1 - rbeta(1, beta_a, beta_b))
    }
    if (has_symptoms_x == 1 && test_positive == 1) x_sym_pos[j] <- 1    
    if (has_symptoms_x == 0 && test_positive == 1) x_asym_pos[j] <- 1
    if (has_symptoms_x == 1 && test_positive == 0) x_sym_neg[j] <- 1
    if (has_symptoms_x == 0 && test_positive == 0) x_asym_neg[j] <- 1
  }
  
  n_sym_pos <- sum(x_sym_pos)
  n_asym_pos <- sum(x_asym_pos)
  n_sym_neg <- sum(x_sym_neg)
  n_asym_neg <- sum(x_asym_neg)
  return (data.frame(
    n_sym_pos = n_sym_pos,
    n_asym_pos = n_asym_pos,
    n_sym_neg = n_sym_neg,
    n_asym_neg = n_asym_neg
  ))
}





