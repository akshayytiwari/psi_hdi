# ============================================================
# 11_monte_carlo_zero_phi.R
# ============================================================
# Description:
#   Analytical formalism (Monte Carlo implementation)
#   for corrected ψ when φ_c = 0.
# ============================================================

library(dplyr)

#' Analytical Monte Carlo formalism for φ_c = 0
#'
#' @param n Number of individuals
#' @param sp Number of test positives (symptomatic + asymptomatic)
#' @param n_iter Number of Monte Carlo iterations
#' @param rho_c,psi_c Crude prevalence and asymptomatic proportion
#' @param alpha_a,alpha_b Alpha/Beta parameters for sensitivity
#' @param beta_a,beta_b Alpha/Beta parameters for specificity
#'
#' @return Numeric vector of ψ estimates across Monte Carlo iterations
#' @export
monte_carlo_no_phi <- function (n, 
                                sp,
                                n_iter, 
                                rho_c, 
                                psi_c, 
                                alpha_a, 
                                alpha_b, 
                                beta_a, 
                                beta_b) {
  eps <- 1e-8
  rho_c <- rbinom(n_iter, n, rho_c) / n
  psi_c <- rbinom(n_iter, sp, psi_c) / sp
  alpha <- rbeta(n_iter, alpha_a, alpha_b)
  beta <- rbeta(n_iter, beta_a, beta_b)
  
  model_form <- function(rho_c, psi_c, alpha, beta) {
    num <- rho_c * (1 - psi_c) * (alpha + beta - 1)
    denom <- alpha * (rho_c + beta - 1)
    denom <- ifelse(abs(denom) < eps, NA, denom)
    
    psi_temp <- 1 - num / denom
    psi_form <- ifelse(is.na(psi_temp) | psi_temp < 0 | psi_temp > 1, NA, psi_temp)
    
    return(psi_form)
  }
  
  psi <- model_form(rho_c, psi_c, alpha, beta)
  return (psi[!is.na(psi)])
}


