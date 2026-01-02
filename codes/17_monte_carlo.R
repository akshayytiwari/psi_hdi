# ============================================================
# 17_monte_carlo.R
# ============================================================
# Description:
#   Analytical formalism (Monte Carlo implementation)
#   for corrected ψ when φ_c can take any value.
# ============================================================

library(dplyr)

#' Analytical Monte Carlo formalism for any φ_c (general case)
#'
#' @param n Number of individuals in the survey
#' @param sp Number of test positives (symptomatic + asymptomatic)
#' @param sn Number of test negatives (symptomatic + asymptomatic)
#' @param n_iter Number of Monte Carlo iterations
#' @param rho_c Crude seroprevalence
#' @param psi_c Crude asymptomatic proportion among test-positives
#' @param phi_c Fraction of symptomatic individuals among test-negatives
#' @param alpha_a,alpha_b Beta prior parameters for sensitivity
#' @param beta_a,beta_b  Beta prior parameters for specificity
#'
#' @return Numeric vector of corrected ψ estimates across Monte Carlo samples
#' @export

source(here("scripts/01_formalism.R"))
monte_carlo <- function(n,
                            sp,
                            sn,
                            n_iter,
                            rho_c,
                            psi_c,
                            phi_c,
                            alpha_a,
                            alpha_b,
                            beta_a,
                            beta_b) {
  
  eps <- 1e-8
  
  # Random draws for uncertainty propagation
  rho_c_mc <- rbinom(n_iter, n, rho_c) / n
  psi_c_mc <- rbinom(n_iter, sp, psi_c) / sp
  phi_c_mc <- rbinom(n_iter, sn, phi_c) / sn
  alpha_mc <- rbeta(n_iter, alpha_a, alpha_b)
  beta_mc  <- rbeta(n_iter, beta_a,  beta_b)
  
  # Analytical ψ correction (vectorized)
  psi<- correction(rho_c_mc, psi_c_mc, phi_c_mc, alpha_mc, beta_mc)
  
  # Return only valid ψ values
  psi[!is.na(psi)]
}
