PATH_RESULTS <- "w:/Masterarbeit/Results/"
PATH_OVERVIEW <- PATH_OVERVIEW <- "W:/Masterarbeit/Daten/trueATE/cont_dgp_overview.csv"
dgps <- read.csv(PATH_OVERVIEW) # Needed for normalisation & relative bias
dgps <- dgps %>%
  rename(dgp = DGPid)
dgps$dgp <- paste0("dgp", dgps$dgp)

results <- read.csv(paste0(PATH_RESULTS, "2026-04-25_NEW_all_results.csv"))

## Data frames to use for tables (96 rows) ----

df_bias <- results %>% filter(metric == "ate_bias") %>%
  group_by(model, dgp) %>%
  summarise(bias_abs = mean(abs(value)))

df_bias <- df_bias %>%
  left_join(
    dgps %>% select(dgp, trueATE, sd_y),
    by = "dgp"
  ) %>%
  mutate(bias_rel = abs(bias_abs / trueATE),
         bias_std = bias_abs/sd_y) %>%
  select(-c("trueATE","sd_y"))

df_rmse <- results %>% filter(metric == "ate_bias") %>%
  group_by(model, dgp) %>%
  summarise(
    rmse = sqrt(mean(value^2)),
    .groups = "drop"
  )

df_rmse <- df_rmse %>%
  left_join(
    dgps %>% select(dgp, trueATE, sd_y),
    by = "dgp"
  ) %>%
  mutate(rmse_rel = abs(rmse / trueATE),
         rmse_std = rmse/sd_y) %>%
  select(-c("trueATE","sd_y")) 

df_coverage <- results %>% filter(metric == "coverage") %>%
  group_by(model, dgp) %>%
  summarise(coverage = mean(value))

df_ci_length <- results %>% filter(metric == "ci_length") %>%
  group_by(dgp, model) %>%
  summarise(ci_length = mean(value))

df_ci_length <- df_ci_length %>%
  left_join(
    dgps %>% select(dgp, trueATE, sd_y),
    by = "dgp"
  ) %>%
  mutate(ci_length_rel = abs(ci_length / trueATE),
         ci_length_std = ci_length/sd_y) %>%
  select(-c("trueATE","sd_y"))


# Model & DGP Summary
by <- join_by(model, dgp)
df_dgps_metrics <- left_join(df_bias, df_rmse) %>% 
  left_join(df_coverage) %>% 
  left_join(df_ci_length)

# write.csv(df_dgps_metrics, paste0(PATH_RESULTS, "2026-04-30_model_dgp_summary_ALL_metrics.csv"), row.names = FALSE)


# Aggregated Metrics over DGPs

# Compare metrics per model ----
# Aggregated DF (Coverage) - per model
df_agg_metrics <- df_dgps_metrics %>%
  group_by(model) %>%
  summarise(
    mean_bias_abs      = mean(bias_abs),
    mean_bias_rel      = mean(bias_rel),
    mean_bias_std      = mean(bias_std),
    mean_rmse          = mean(rmse),
    mean_rmse_rel      = mean(rmse_rel),
    mean_rmse_std      = mean(rmse_std),
    mean_coverage      = mean(coverage),
    mean_ci_length     = mean(ci_length),
    mean_ci_length_rel = mean(ci_length_rel),
    mean_ci_length_std = mean(ci_length_std)
  )

# write.csv(df_agg_metrics, paste0(PATH_RESULTS, "2026-04-30_metrics_summary.csv"), row.names = FALSE)



#----Grafiken
#----Boxplots
ggplot(df_dgps_metrics %>% filter(bias_abs < 10), aes(x = model, y = bias_abs)) +
  geom_boxplot() +
  scale_y_continuous(limits = c(0.05, 0.9)) +
  labs(
    x = "Model",
    y = "Bias (absolute)"
  ) +
  theme_minimal()

ggplot(df_dgps_metrics, aes(x = model, y = bias_rel)) +
  geom_boxplot() +
  scale_y_continuous(limits = c(0.00, 0.22)) +
  labs(
    x = "Model",
    y = "Bias (relative)"
  ) +
  theme_minimal()

  ggplot(df_dgps_metrics, aes(x = model, y = coverage)) +
    geom_boxplot() +
    scale_y_continuous(limits = c(0.58, 1.0)) +
    geom_line(stat="summary", fun=mean) +
  labs(
    x = "Model",
    y = "Coverage"
  ) +
    theme_minimal()
  
  ggplot(df_dgps_metrics, aes(x = model, y = ci_length)) +
    geom_boxplot() +
    geom_line(stat="summary", fun=mean) +
    labs(
      x = "Model",
      y = "CI Length (absolute)"
    ) +
    theme_minimal()
  
  ggplot(df_dgps_metrics, aes(x = model, y = ci_length_rel)) +
    geom_boxplot() +
    geom_line(stat="summary", fun=mean) +
    labs(
      x = "Model",
      y = "CI Length (absolute)"
    ) +
    theme_minimal()
  

#----Scatterplots

# Bias
  
  ggplot(df_dgps_metrics %>% filter (rmse < 10), aes(x = dgp, y = bias_abs,
                                                     col = model)) +
    geom_beeswarm() +
    labs(x = "DGP", y = "Bias", title = "Absolute bias by dgp and model (Outlier removed)")+
    theme(legend.position = "top") +
    scale_color_brewer(palette="Accent")
  
  
  ggplot(df_dgps_metrics %>% filter (rmse < 10), aes(x = dgp, y = bias_rel,
                                                     col = model)) +
    geom_beeswarm() +
    labs(x = "DGP", y = "Bias/true ATE", title = "Relative bias by dgp and model (Outlier removed)")+
    theme(legend.position = "top") +
    scale_color_brewer(palette="Accent")
  
  
  
# RMSE

ggplot(df_dgps_metrics %>% filter (rmse < 10), aes(x = dgp, y = rmse,
                    col = model)) +
  geom_beeswarm() +
  labs(x = "DGP", y = "RMSE", title = "RMSE by dgp and model (Outlier removed)")+
  theme(legend.position = "top") +
  scale_color_brewer(palette="Accent")

ggplot(df_dgps_metrics %>% filter (rmse < 10), aes(x = dgp, y = rmse_rel,
                                                   col = model)) +
  geom_beeswarm() +
  labs(x = "DGP", y = "RMSE/true ATE", title = "Relative RMSE by dgp and model (Outlier removed)")+
  theme(legend.position = "top") +
  scale_color_brewer(palette="Accent")


ggplot(df_dgps_metrics %>% filter (rmse < 10), aes(x = dgp, y = rmse_std, 
                                                   col = model)) +
  geom_beeswarm() +
  labs(x = "DGP", y = "RMSE/SD(y)", title = "Standardized RMSE by dgp and model (Outlier removed)")+
  theme(legend.position = "top") +
  scale_color_brewer(palette="Accent")

# Coverage

ggplot(df_dgps_metrics %>% filter (rmse < 10), aes(x = dgp, y = coverage, 
                                                   col = model)) +
  geom_beeswarm() +
  labs(x = "DGP", y = "Coverage rate", title = "Coverage rate by dgp and model (Outlier removed)")+
  theme(legend.position = "top") +
  scale_color_brewer(palette="Accent")

# CI

ggplot(df_dgps_metrics %>% filter (rmse < 10), aes(x = dgp, y = ci_length, 
                                                   col = model)) +
  geom_beeswarm() +
  labs(x = "DGP", y = "CI Length", title = "Standardised CI Length by dgp and model (Outlier removed)")+
  theme(legend.position = "top") +
  scale_color_brewer(palette="Accent")

ggplot(df_dgps_metrics %>% filter (rmse < 10), aes(x = dgp, y = ci_length_rel, 
                                                   col = model)) +
  geom_beeswarm() +
  labs(x = "DGP", y = "CI Length/trueATE", title = "Standardised CI Length by dgp and model (Outlier removed)")+
  theme(legend.position = "top") +
  scale_color_brewer(palette="Accent")



