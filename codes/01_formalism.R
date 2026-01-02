# ============================================================
# 01_formalism.R
# ============================================================
# Description:
#   Calculates corrected psi using equation (1) from the main text.
#   Inputs: rho_c, psi_c, phi_c, alpha, beta
#   Output: psi
# ============================================================

eps <- 1e-08
correction <- function(rho_c, psi_c, phi_c, alpha, beta) {
  numerator <- rho_c * (1 - rho_c) * (psi_c - 1 + phi_c) * (alpha + beta - 1)
  denominator <- (rho_c + beta - 1) * (psi_c * rho_c * (1 - alpha) - alpha * (1 - phi_c) * (1 - rho_c))
  denominator <- ifelse(abs(denominator) < eps, NA, denominator)

  psi_temp <- 1 - numerator / denominator
  psi <- ifelse(is.na(psi_temp) | psi_temp < 0 | psi_temp > 1, NA, psi_temp)
  
  c(psi)
}


