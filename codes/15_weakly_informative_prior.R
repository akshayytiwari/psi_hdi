# ============================================================
# 15_weakly_informative_prior.R
# ============================================================
# Description:
#   Generate Beta distribution parameters using weakly informative priors.
# ============================================================

weak_prior <- function (mu, kappa){     # mu: mean, kappa: concentration parameter, represents sample size
  a <- mu*kappa
  b <- (1-mu)*kappa
  return(list(mean = mu, alpha = a, beta = b))
}