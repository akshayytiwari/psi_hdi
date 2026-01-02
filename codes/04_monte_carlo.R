# Finding distribution of psi using Monte Carlo simulation based on correction formula

source(here("codes/01_formalism.R"))
set.seed(123)
monte_carlo <- function (n, sp, sn, n_iter,
                         rho_c, psi_c, phi_c,
                         alpha_a, alpha_b, beta_a, beta_b) {
  
  alpha <- rbeta(n_iter, alpha_a, alpha_b)
  beta  <- rbeta(n_iter, beta_a, beta_b)
  rho_c <- rbinom(n_iter, n, rho_c) / n
  psi_c <- rbinom(n_iter, sp, psi_c) / sp
  phi_c <- rbinom(n_iter, sn, phi_c) / sn

  psi_hat <- correction(rho_c, psi_c, phi_c, alpha, beta)
  eta_hat <- 100 * (psi_hat - psi_c) / (1 - psi_c)

  return(list(rho_c = rho_c, psi_c = psi_c, phi_c = phi_c, 
              psi = psi_hat, eta = eta_hat))

}
