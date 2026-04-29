PATH_RESULTS = "W:/Masterarbeit/Results/final/"

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

ps_bart <- df_rmse_bart %>% filter(model == "bart_ps-bart")

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
                                 default = character(),
                                 rmse = numeric())

# Add all defaults to DF

df_comparison_rmse <- rbind(df_comparison_rmse, data.frame(
  model = default_bart$model,
  default = "default",
  rmse = default_bart$rmse))


df_comparison_rmse <- rbind(df_comparison_rmse, data.frame(
  model = default_cf$model,
  default = "default",
  rmse = default_cf$rmse))

df_comparison_rmse <- rbind(df_comparison_rmse, data.frame(
  model = default_iptw$model,
  default = "default",
  rmse = default_iptw$rmse))

# Add best tuned results

#BART

best_rmse_bart <- df_rmse_bart %>%
  group_by(model) %>%
  slice_min(order_by = rmse, n = 1, with_ties = FALSE) %>%
  ungroup()

df_comparison_rmse <- rbind(df_comparison_rmse, data.frame(
  model = best_rmse_bart$model,
  default = "tuned",
  rmse = best_rmse_bart$rmse))

#CF

best_rmse_cf <- df_rmse_cf %>%
  group_by(model) %>%
  slice_min(order_by = rmse, n = 1, with_ties = FALSE) %>%
  ungroup()

df_comparison_rmse <- rbind(df_comparison_rmse, data.frame(
  model = best_rmse_cf$model,
  default = "tuned",
  rmse = best_rmse_cf$rmse))

# IPTW

bart_iptw <- df_rmse_iptw %>% filter(ps_model == "bart")

df_comparison_rmse <- rbind(df_comparison_rmse, data.frame(
  model = bart_iptw$model,
  default = "tuned",
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
  default = character(),
  coverage = numeric()
)

# Defaults
df_comparison_cov <- rbind(df_comparison_cov,
                           data.frame(model = default_cov_bart$model, default = "default", coverage = default_cov_bart$coverage),
                           data.frame(model = default_cov_cf$model, default = "default", coverage = default_cov_cf$coverage),
                           data.frame(model = default_cov_iptw$model, default = "default", coverage = default_cov_iptw$coverage)
)

# Tuned (RMSE-optimal params!)
df_comparison_cov <- rbind(df_comparison_cov,
                           data.frame(model = best_cov_bart$model, default = "tuned", coverage = best_cov_bart$coverage),
                           data.frame(model = best_cov_cf$model, default = "tuned", coverage = best_cov_cf$coverage),
                           data.frame(model = best_cov_iptw$model, default = "tuned", coverage = best_cov_iptw$coverage)
)


ggplot(df_comparison_cov, aes(x = default, y = coverage, col = model)) +
  geom_beeswarm() +
  labs(x = "Default vs tuned parameters",
       y = "Coverage",
       title = "Default vs tuned parameters (Coverage)") +
  theme(legend.position = "top") +
  scale_color_brewer(palette = "Dark2")

write.csv(df_comparison_cov, paste0(PATH_RESULTS, "2026-04-29_tuning_coverage"))


#-----CI Length
df_ci_bart <- tuning_results_60_bart %>%
  filter(metric == "ci_length") %>%
  group_by(model, dgp, k, n_trees, nu, q) %>%
  summarise(
    ci_length = mean(value),
    .groups = "drop"
  )

df_ci_cf <- tuning_results_60_cf %>%
  filter(metric == "ci_length") %>%
  group_by(model, min_node_size, sample_fraction, mtry) %>%
  summarise(
    ci_length = mean(value),
    .groups = "drop"
  )

df_ci_iptw <- tuning_results_60_iptw %>%
  filter(metric == "ci_length") %>%
  group_by(model, ps_model) %>%
  summarise(
    ci_length = mean(value),
    .groups = "drop"
  )

best_ci_bart <- df_ci_bart %>%
  inner_join(best_rmse_bart,
             by = c("model", "dgp", "k", "n_trees", "nu", "q"))

best_ci_cf <- df_ci_cf %>%
  inner_join(best_rmse_cf,
             by = c("model", "min_node_size", "sample_fraction", "mtry"))

best_ci_iptw <- df_ci_iptw %>%
  inner_join(bart_iptw,
             by = c("model", "ps_model"))

default_ci_bart <- df_ci_bart %>%
  filter(k == 2, n_trees == 75, nu == 3, q == 0.9)

default_ci_cf <- df_ci_cf %>%
  filter(min_node_size == 5, sample_fraction == 0.5, mtry == 26)

default_ci_iptw <- df_ci_iptw %>%
  filter(ps_model == "linear_basic")

df_summary_dgp30 <- bind_rows(
  
  # -------- BART DEFAULT --------
  default_bart %>%
    mutate(setting = "default",
           ci_length = default_ci_bart$ci_length),
  
  # -------- BART TUNED --------
  best_rmse_bart %>%
    left_join(best_cov_bart, by = c("model", "dgp", "k", "n_trees", "nu", "q")) %>%
    left_join(best_ci_bart, by = c("model", "dgp", "k", "n_trees", "nu", "q")) %>%
    mutate(setting = "tuned"),
  
  # -------- CF DEFAULT --------
  default_cf %>%
    mutate(setting = "default",
           ci_length = default_ci_cf$ci_length),
  
  # -------- CF TUNED --------
  best_rmse_cf %>%
    left_join(best_cov_cf, by = c("model", "min_node_size", "sample_fraction", "mtry")) %>%
    left_join(best_ci_cf, by = c("model", "min_node_size", "sample_fraction", "mtry")) %>%
    mutate(setting = "tuned"),
  
  # -------- IPTW DEFAULT --------
  default_iptw %>%
    mutate(setting = "default",
           ci_length = default_ci_iptw$ci_length),
  
  # -------- IPTW TUNED --------
  bart_iptw %>%
    left_join(best_cov_iptw, by = c("model", "ps_model")) %>%
    left_join(best_ci_iptw, by = c("model", "ps_model")) %>%
    mutate(setting = "tuned")
)


# DGP 60

