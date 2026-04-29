model_specs <- list(
  bart = list(
    data = tuning_results_60_bart,
    default = list(k = 2, n_trees = 75, nu = 3, q = 0.9),
    group_vars = c("model", "dgp", "k", "n_trees", "nu", "q")
  ),
  
  cf = list(
    data = tuning_results_60_cf,
    default = list(min_node_size = 5, sample_fraction = 0.5, mtry = 26),
    group_vars = c("model", "min_node_size", "sample_fraction", "mtry")
  ),
  
  iptw = list(
    data = tuning_results_60_iptw,
    default = list(ps_model = "linear_basic"),
    group_vars = c("model", "ps_model")
  )
)

summarise_metric <- function(df, metric_name, group_vars) {
  df %>%
    filter(metric == metric_name) %>%
    group_by(across(all_of(group_vars))) %>%
    summarise(value = mean(value), .groups = "drop")
}

metrics <- c("ate_bias", "coverage", "ci_length")

results <- list()

for (m in names(model_specs)) {
  
  spec <- model_specs[[m]]
  df <- spec$data
  gv <- spec$group_vars
  
  rmse_df <- summarise_metric(df, "ate_bias", gv) %>%
    mutate(rmse = sqrt(value^2))  # or sqrt(mean(value^2)) if raw data
  
  cov_df <- summarise_metric(df, "coverage", gv) %>%
    rename(coverage = value)
  
  ci_df <- summarise_metric(df, "ci_length", gv) %>%
    rename(ci_length = value)
  
  # default row
  default_row <- rmse_df
  for (nm in names(spec$default)) {
    default_row <- default_row %>% filter(.data[[nm]] == spec$default[[nm]])
  }
  
  default_row <- default_row %>%
    mutate(setting = "default") %>%
    left_join(cov_df, by = gv) %>%
    left_join(ci_df, by = gv)
  
  # tuned row
  tuned_row <- rmse_df %>%
    group_by(model) %>%
    slice_min(rmse, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    mutate(setting = "tuned") %>%
    left_join(cov_df, by = gv) %>%
    left_join(ci_df, by = gv)
  
  results[[m]] <- bind_rows(default_row, tuned_row)
}

df_summary <- bind_rows(results)

library(dplyr)

df_summary <- df_summary %>%
  select(
    # identifiers
    model, dgp, setting,
    
    # hyperparameters (model-specific, may contain NAs)
    k, n_trees, nu, q,
    min_node_size, sample_fraction, mtry,
    ps_model,
    
    # metrics (always at the end)
    rmse, coverage, ci_length,
    
    # keep anything else (optional safety)
    everything()
  )