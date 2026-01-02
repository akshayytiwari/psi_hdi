# ============================================================
# 06_correlation_analysis.R
# ============================================================
# Description:
#   Computes Spearman correlations of eta with alpha, beta,
#   rho_c, and phi_c
# ============================================================

library(dplyr)
library(broom)

correlate_eta <- function(df) {
  vars <- c("alpha", "beta", "rho_c", "phi_c")
  lapply(vars, function(v) {
    cor.test(df[[v]], df$eta, method = "spearman") %>%
      broom::tidy() %>%
      mutate(variable = v)
  }) %>%
    bind_rows() %>%
    select(variable, estimate, p.value)
}
