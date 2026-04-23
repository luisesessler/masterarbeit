dgp30_ps_bart_comparison_ate_bias <- dgp30_ps_bart %>% group_by(metric, k, n_trees) %>%
  summarise(mean_value = mean(abs(value))) %>%
  filter(metric == "ate_bias")

dgp30_ps_glm_comparison_ate_bias <- dgp30_ps_glm %>% group_by(metric, k, n_trees) %>%
  summarise(mean_value = mean(abs(value))) %>%
  filter(metric == "ate_bias")

