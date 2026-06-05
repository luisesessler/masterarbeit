PATH_RESULTS = "W:/Masterarbeit/Results/"
PATH_OVERVIEW <- "W:/Masterarbeit/Daten/trueATE/cont_dgp_overview.csv"

dgps <- read.csv(PATH_OVERVIEW)

# DGP60


# Calculate RMSE

get_default_and_tuned_metrics <- function(tuning_bart, tuning_cf, tuning_iptw, n_cov){
mtry_default = min(ceiling(sqrt(n_cov)) + 20, n_cov)
  
  
df_rmse_bart <- tuning_bart %>% filter(metric == "ate_bias") %>%
  group_by(model, dgp, k, n_trees, nu, q) %>%
  summarise(
    rmse = sqrt(mean(value^2)),
    .groups = "drop"
  )

ps_bart <- df_rmse_bart %>% filter(model == "bart_ps-bart")

df_rmse_cf <- tuning_cf %>% filter(metric == "ate_bias") %>%
  group_by(model, min_node_size, sample_fraction, mtry) %>%
  summarise(
    rmse = sqrt(mean(value^2)),
    .groups = "drop"
  )

df_rmse_iptw <- tuning_iptw %>% filter(metric == "ate_bias") %>%
  group_by(model, ps_model) %>%
  summarise(
    rmse = sqrt(mean(value^2)),
    .groups = "drop"
  )

# Default parameter

default_bart <- df_rmse_bart %>% filter(k == 2, n_trees == 75, nu == 3, q == 0.9)
default_cf <- df_rmse_cf %>% filter(min_node_size == 5, sample_fraction == 0.5, mtry == mtry_default) 
default_iptw <- df_rmse_iptw %>% filter(ps_model == "linear_basic")

df_comparison_rmse <- rbind(
  data.frame(model = default_bart$model, default = "default", rmse = default_bart$rmse),
  data.frame(model = default_cf$model, default = "default", rmse = default_cf$rmse),
  data.frame(model = default_iptw$model, default = "default", rmse = default_iptw$rmse)
)

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

bart_iptw <- df_rmse_iptw %>% filter(ps_model == "bart") # TODO best raussuchen

df_comparison_rmse <- rbind(df_comparison_rmse, data.frame(
  model = bart_iptw$model,
  default = "tuned",
  rmse = bart_iptw$rmse))



#------Coverage
df_cov_bart <- tuning_bart %>%
  filter(metric == "coverage") %>%
  group_by(model, dgp, k, n_trees, nu, q) %>%
  summarise(
    coverage = mean(value),
    .groups = "drop"
  )

df_cov_cf <- tuning_cf %>%
  filter(metric == "coverage") %>%
  group_by(model, min_node_size, sample_fraction, mtry) %>%
  summarise(
    coverage = mean(value),
    .groups = "drop"
  )

df_cov_iptw <- tuning_iptw %>%
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
  filter(min_node_size == 5, sample_fraction == 0.5, mtry == mtry_default)

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


#-----CI Length
df_ci_bart <- tuning_bart %>%
  filter(metric == "ci_length") %>%
  group_by(model, dgp, k, n_trees, nu, q) %>%
  summarise(
    ci_length = mean(value),
    .groups = "drop"
  )

df_ci_cf <- tuning_cf %>%
  filter(metric == "ci_length") %>%
  group_by(model, min_node_size, sample_fraction, mtry) %>%
  summarise(
    ci_length = mean(value),
    .groups = "drop"
  )

df_ci_iptw <- tuning_iptw %>%
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
  filter(min_node_size == 5, sample_fraction == 0.5, mtry == mtry_default)

default_ci_iptw <- df_ci_iptw %>%
  filter(ps_model == "linear_basic")

df_summary <- bind_rows(
  
  # -------- BART DEFAULT --------
  default_bart %>%
    mutate(setting = "default",
           ci_length = default_ci_bart$ci_length,
           coverage  = default_cov_bart$coverage),
  
  # -------- BART TUNED --------
  best_rmse_bart %>%
    left_join(best_cov_bart, by = c("model", "dgp", "k", "n_trees", "nu", "q")) %>%
    left_join(best_ci_bart, by = c("model", "dgp", "k", "n_trees", "nu", "q")) %>%
    mutate(setting = "tuned"),
  
  # -------- CF DEFAULT --------
  default_cf %>%
    mutate(setting = "default",
           ci_length = default_ci_cf$ci_length,
           coverage  = default_cov_cf$coverage),
  
  # -------- CF TUNED --------
  best_rmse_cf %>%
    left_join(best_cov_cf, by = c("model", "min_node_size", "sample_fraction", "mtry")) %>%
    left_join(best_ci_cf, by = c("model", "min_node_size", "sample_fraction", "mtry")) %>%
    mutate(setting = "tuned"),
  
  # -------- IPTW DEFAULT --------
  default_iptw %>%
    mutate(setting = "default",
           ci_length = default_ci_iptw$ci_length,
           coverage  = default_cov_iptw$coverage),
  
  # -------- IPTW TUNED --------
  bart_iptw %>%
    left_join(best_cov_iptw, by = c("model", "ps_model")) %>%
    left_join(best_ci_iptw, by = c("model", "ps_model")) %>%
    mutate(setting = "tuned")
)
return(df_summary)

}

# DGP 30

summary_30_tuning <- get_default_and_tuned_metrics(dgp30_bart_tuning, dgp30_cf_tuning, dgp30_ipwt_tuning, dgps$n_covariates[dgps$DGPid == 30])
summary_30_tuning$rmse_relative <- summary_30_tuning$rmse / 10 

ggplot(summary_30_tuning, aes(x = setting, y = rmse_relative, col = model)) +
  geom_beeswarm() +
  labs(x = "DGP", y = "RMSE", title = "RMSE by dgp and model (Outlier removed)") +
  theme(legend.position = "top") +
  scale_color_brewer(palette="Accent")


write.csv(summary_30_tuning, paste0(PATH_RESULTS,"2026-06-05_tuning_summary_dgp30.csv"), row.names = FALSE)



# DGP 40

summary_40_tuning <- get_default_and_tuned_metrics(dgp40_bart_tuning, dgp40_cf_tuning, dgp40_ipwt_tuning, dgps$n_covariates[dgps$DGPid == 40])
summary_40_tuning$rmse_relative <- summary_40_tuning$rmse / 6.961985 

ggplot(summary_40_tuning, aes(x = setting, y = rmse, col = model)) +
  geom_beeswarm() +
  labs(x = "DGP", y = "RMSE", title = "RMSE by dgp and model (Outlier removed)") +
  theme(legend.position = "top") +
  scale_color_brewer(palette="Accent")

write.csv(summary_40_tuning, paste0(PATH_RESULTS,"2026-06-05_tuning_summary_dgp40.csv"), row.names = FALSE)


# DGP 60

summary_60_tuning <- get_default_and_tuned_metrics(dgp60_bart_tuning, dgp60_cf_tuning, dgp60_ipwt_tuning, dgps$n_covariates[dgps$DGPid == 60])
summary_60_tuning$rmse_relative <- summary_60_tuning$rmse / 1.686248 


ggplot(summary_60_tuning, aes(x = setting, y = rmse, col = model)) +
  geom_beeswarm() +
  labs(x = "DGP", y = "RMSE", title = "RMSE by dgp and model (Outlier removed)") +
  theme(legend.position = "top") +
  scale_color_brewer(palette="Accent")

write.csv(summary_60_tuning, paste0(PATH_RESULTS,"2026-06-05_tuning_summary_dgp60.csv"), row.names = FALSE)




# DGP 63

summary_63_tuning <- get_default_and_tuned_metrics(dgp63_bart_tuning, dgp63_cf_tuning, dgp63_ipwt_tuning, dgps$n_covariates[dgps$DGPid == 63])
summary_63_tuning$rmse_relative <- summary_63_tuning$rmse / 2.000000
 


ggplot(summary_63_tuning, aes(x = setting, y = rmse, col = model)) +
  geom_beeswarm() +
  labs(x = "DGP", y = "RMSE", title = "RMSE by dgp and model (Outlier removed)") +
  theme(legend.position = "top") +
  scale_color_brewer(palette="Accent")

write.csv(summary_63_tuning, paste0(PATH_RESULTS,"2026-06-05_tuning_summary_dgp63.csv"), row.names = FALSE)
