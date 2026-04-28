# DGP60

# IPTW
tuning_results_60_iptw <- read.csv(paste0(PATH_RESULTS, "2026-04-28_tuning_NO-CV_dgp60_iptw.csv"))

# Causal forest
tuning_results_60_cf <- read.csv(paste0(PATH_RESULTS, "2026-04-24_tuning_NO-CV_dgp60_causal-forest_.csv"))

# BART Models

tuning_results_60_bart <- read.csv(paste0(PATH_RESULTS, "2026-04-28_tuning_NO-CV_dgp60_ALL_bart-models_sigma_complete.csv"))

# Calculate RMSE

df_rmse_bart <- tuning_results_60_bart %>% filter(metric == "ate_bias") %>%
  group_by(model, dgp, k, n_trees, nu, q) %>%
  summarise(
    rmse = sqrt(mean(value^2)),
    .groups = "drop"
  )

df_rmse_cf <- tuning_results_60_cf %>% filter(metric == "ate_bias") %>%
  group_by(model, min_node_size, sample_fraction, mtry) %>%
  summarise(
    rmse = sqrt(mean(value^2)),
    .groups = "drop"
  )

df_rmse_iptw <- tuning_results_60_iptw %>% filter(metric == "ate_bias") %>%
  group_by(model, ps_model) %>%
  summarise(
    rmse = sqrt(mean(value^2)),
    .groups = "drop"
  )

# Default parameter

default_bart <- df_rmse_bart %>% filter(k == 2, n_trees == 75, nu == 3, q == 0.9)
default_cf <- df_rmse_cf %>% filter(min_node_size == 5, sample_fraction == 0.5, mtry == 26)
default_iptw <- df_rmse_iptw %>% filter(ps_model == "linear_basic")

df_comparison_rmse <- data.frame(model = character(),
           default = numeric(),
           rmse = numeric())

# Add all defaults to DF

df_comparison_rmse <- rbind(df_comparison_rmse, data.frame(
                            model = default_bart$model,
                            default = 1,
                            rmse = default_bart$rmse))


df_comparison_rmse <- rbind(df_comparison_rmse, data.frame(
  model = default_cf$model,
  default = 1,
  rmse = default_cf$rmse))

df_comparison_rmse <- rbind(df_comparison_rmse, data.frame(
  model = default_iptw$model,
  default = 1,
  rmse = default_iptw$rmse))

# Add best tuned results

#BART

best_rmse_bart <- df_rmse_bart %>%
  group_by(model) %>%
  slice_min(order_by = rmse, n = 1, with_ties = FALSE) %>%
  ungroup()

df_comparison_rmse <- rbind(df_comparison_rmse, data.frame(
  model = best_rmse_bart$model,
  default = 0,
  rmse = best_rmse_bart$rmse))

#CF

best_rmse_cf <- df_rmse_cf %>%
  group_by(model) %>%
  slice_min(order_by = rmse, n = 1, with_ties = FALSE) %>%
  ungroup()

df_comparison_rmse <- rbind(df_comparison_rmse, data.frame(
  model = best_rmse_cf$model,
  default = 0,
  rmse = best_rmse_cf$rmse))

# IPTW

bart_iptw <- df_rmse_iptw %>% filter(ps_model == "bart")

df_comparison_rmse <- rbind(df_comparison_rmse, data.frame(
  model = bart_iptw$model,
  default = 0,
  rmse = bart_iptw$rmse))

#Scatterplot
# TODO x Achsen nicht numerisch, am besten benennen und umdrehen!
ggplot(df_comparison_rmse, aes(x = default, y = rmse, col = model)) +
  geom_point() +
  labs(x = "Default vs tuned parameters", y = "RMSE", title = "Default vs tuned parameters") +
  theme(legend.position = "top") +
  scale_color_brewer(palette="Dark2")



#------Coverage
df_cov_bart <- tuning_results_60_bart %>%
  filter(metric == "coverage") %>%
  group_by(model, dgp, k, n_trees, nu, q) %>%
  summarise(
    coverage = mean(value),
    .groups = "drop"
  )

df_cov_cf <- tuning_results_60_cf %>%
  filter(metric == "coverage") %>%
  group_by(model, min_node_size, sample_fraction, mtry) %>%
  summarise(
    coverage = mean(value),
    .groups = "drop"
  )

df_cov_iptw <- tuning_results_60_iptw %>%
  filter(metric == "coverage") %>%
  group_by(model, ps_model) %>%
  summarise(
    coverage = mean(value),
    .groups = "drop"
  )

best_cov_bart <- df_cov_bart %>%
  inner_join(best_rmse_bart,
             by = c("model", "dgp", "k", "n_trees", "nu", "q"))

best_cov_cf <- df_cov_cf %>%
  inner_join(best_rmse_cf,
             by = c("model", "min_node_size", "sample_fraction", "mtry"))

best_cov_iptw <- df_cov_iptw %>%
  inner_join(bart_iptw,
             by = c("model", "ps_model"))

default_cov_bart <- df_cov_bart %>%
  filter(k == 2, n_trees == 75, nu == 3, q == 0.9)

default_cov_cf <- df_cov_cf %>%
  filter(min_node_size == 5, sample_fraction == 0.5, mtry == 26)

default_cov_iptw <- df_cov_iptw %>%
  filter(ps_model == "linear_basic")

df_comparison_cov <- data.frame(
  model = character(),
  default = numeric(),
  coverage = numeric()
)

# Defaults
df_comparison_cov <- rbind(df_comparison_cov,
                           data.frame(model = default_cov_bart$model, default = 1, coverage = default_cov_bart$coverage),
                           data.frame(model = default_cov_cf$model, default = 1, coverage = default_cov_cf$coverage),
                           data.frame(model = default_cov_iptw$model, default = 1, coverage = default_cov_iptw$coverage)
)

# Tuned (RMSE-optimal params!)
df_comparison_cov <- rbind(df_comparison_cov,
                           data.frame(model = best_cov_bart$model, default = 0, coverage = best_cov_bart$coverage),
                           data.frame(model = best_cov_cf$model, default = 0, coverage = best_cov_cf$coverage),
                           data.frame(model = best_cov_iptw$model, default = 0, coverage = best_cov_iptw$coverage.y)
)


ggplot(df_comparison_cov, aes(x = default, y = coverage, col = model)) +
  geom_beeswarm() +
  labs(x = "Default vs tuned parameters",
       y = "Coverage",
       title = "Default vs tuned parameters (Coverage)") +
  theme(legend.position = "top") +
  scale_color_brewer(palette = "Dark2")













tuning_60_s_learner <- tuning_result_60s %>% filter(model == "bart_s-learner")
tuning_60_t_learner <- tuning_result_60s %>% filter(model == "bart_t-learner")
tuning_60_ps_bart <- tuning_result_60s %>% filter(model == "bart_ps-bart")
tuning_60_ps_glm <- tuning_result_60s %>% filter(model == "bart_ps-glm")





summaries <- tuning_results %>% group_by(model, metric, k, n_trees, nu, q) %>%
  summarise(mean_value = mean(abs(value)))

summaries_s_learner <- summaries %>% filter(model == "bart_s-learner")

summaries_t_learner <- summaries %>% filter(model == "bart_t-learner")

summaries_ps_bart <- summaries %>% filter(model == "bart_ps-bart")

summaries_ps_glm <- summaries %>% filter(model == "bart_ps-glm")