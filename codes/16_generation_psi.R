# ============================================================
# 16_generation_psi.R
# ============================================================
# Description:
#   Generate synthetic survey data when φ_c ≠ 0
#   (general case where symptom overlap exists between infected
#   and uninfected individuals).
# ============================================================

library(dplyr)

#' Generate synthetic survey data with φ_c ≠ 0
#'
#' @param n Number of individuals to simulate
#' @param rho True infection prevalence
#' @param psi True asymptomatic proportion among infected
#' @param phi True fraction of symptomatic individuals among uninfected
#' @param alpha_a,alpha_b Alpha parameters for Beta prior of sensitivity
#' @param beta_a,beta_b  Beta parameters for Beta prior of specificity

synthetic <- function (n, rho, psi, phi, alpha_a, alpha_b, beta_a, beta_b) {
  if (any(c(alpha_a, alpha_b, beta_a, beta_b) <= 0 | is.na(c(alpha_a, alpha_b, beta_a, beta_b)))) {
    stop("Invalid Beta parameters: check weak_prior or grid inputs.")
  }
  x_sym_pos <- x_asym_pos <- x_sym_neg <- x_asym_neg <- integer(n)
  
  for (j in 1:n) {
    infected <- rbinom(1, 1, rho)
    if (infected == 1) {
      has_symptoms_x <- rbinom(1, 1, 1 - psi)    # symptoms due to x
      has_symptoms_y <- rbinom(1, 1, phi)        # symptoms due to y
      test_positive <- rbinom(1, 1, rbeta(1, alpha_a, alpha_b))
    } else {
      has_symptoms_x <- 0
      has_symptoms_y <- rbinom(1, 1, phi)
      test_positive <- rbinom(1, 1, 1 - rbeta(1, beta_a, beta_b))
    }
    if ((has_symptoms_x == 1 | has_symptoms_y == 1) && test_positive == 1) x_sym_pos[j] <- 1    
    if (has_symptoms_x == 0 && has_symptoms_y == 0 && test_positive == 1) x_asym_pos[j] <- 1
    if ((has_symptoms_x == 1 | has_symptoms_y == 1) && test_positive == 0) x_sym_neg[j] <- 1
    if (has_symptoms_x == 0 && has_symptoms_y == 0 && test_positive == 0) x_asym_neg[j] <- 1
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




