mean_results <- df_tuning_results %>% group_by(model, dgp, metric, k, n_trees) %>%
  summarise(mean_value = mean(abs(value)))