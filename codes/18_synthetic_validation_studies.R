# ============================================================
# 16_formalism_validation_real.R
# ============================================================
# Description:
#   Validation of formalism using real serosurvey parameters
#   (Monte Carlo vs observed study parameters)
# ============================================================

library(dplyr)
library(tibble)

generate_real_validation <- function(df_real,
                                     synthetic_fun,
                                     formalism_fun,
                                     n_iter_mc = 1e5,
                                     out_tab = "../outputs/tables/FigureS3b_results.csv") {
  
  results <- vector("list", nrow(df_real))
  set.seed(123)
  
  for (i in seq_len(nrow(df_real))) {
    study <- df_real[i, ]
    
    out_syn <- synthetic_fun(study$n, study$rho_c, study$psi_c, study$phi_c,
                             study$alpha_a, study$alpha_b, study$beta_a, study$beta_b)
    
    rho_c_obs <- (out_syn$n_sym_pos + out_syn$n_asym_pos) / sum(out_syn)
    psi_c_obs <- out_syn$n_asym_pos / (out_syn$n_sym_pos + out_syn$n_asym_pos)
    phi_c_obs <- out_syn$n_sym_neg / (out_syn$n_sym_neg + out_syn$n_asym_neg)
    
    out_form <- formalism_fun(
      study$n,
      out_syn$n_sym_pos + out_syn$n_asym_pos,
      out_syn$n_sym_neg + out_syn$n_asym_neg,
      n_iter_mc,
      rho_c_obs, psi_c_obs, phi_c_obs,
      study$alpha_a, study$alpha_b, study$beta_a, study$beta_b
    )
    
    results[[i]] <- tibble(
      article = study$article,
      n = study$n,
      psi_true = study$psi_c,
      psi_form_median = quantile(out_form$psi_form, 0.5),
      psi_form_lower  = quantile(out_form$psi_form, 0.25),
      psi_form_upper  = quantile(out_form$psi_form, 0.75),
      psi_form_delta  = (study$psi_c - quantile(out_form$psi_form, 0.5)) * 100 / study$psi_c,
      psi_form_width  = diff(quantile(out_form$psi_form, c(0.25, 0.75)))
    )
  }
  
  df_out <- bind_rows(results)
  write.csv(df_out, out_tab, row.names = FALSE)
  message("Saved: ", out_tab)
  return(df_out)
}
