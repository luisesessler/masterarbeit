library(purrr)
library(dplyr)
library(tidyr)

#PATH_RESULTS <- "C:/Users/luise/Documents/Masterarbeit/Results/"
PATH_RESULTS <- "w:/Masterarbeit/Results/"
PATH_OVERVIEW <- PATH_OVERVIEW <- "W:/Masterarbeit/Daten/trueATE/cont_dgp_overview.csv"
dgps <- read.csv(PATH_OVERVIEW) # Needed for normalisation & relative bias
dgps <- dgps %>%
  rename(dgp = DGPid)
dgps$dgp <- paste0("dgp", dgps$dgp)

df <- read.csv(paste0(PATH_RESULTS, "2026-04-10_all_results.csv"))

# _________________________________________________________

## Data frames to use for tables (96 rows) ----
df_rmse_complete <- df %>% filter(metric == "ate_bias") %>%
  group_by(model, dgp) %>%
  summarise(
    rmse = sqrt(mean(value^2)),
    .groups = "drop"
)

df_rmse_sdy <- df_rmse_complete %>%
  left_join(
    dgps %>% select(dgp, sd_y),
    by = "dgp"
  ) %>%
  mutate(normalized_rmse = abs(rmse / sd_y))

df_bias_complete <- df %>% filter(metric == "ate_bias") %>%
  group_by(model, dgp) %>%
  summarise(mean_bias = mean(abs(value), na.rm = TRUE))

df_bias_abs_rel <- df_bias_complete %>%
  left_join(
    dgps %>% select(dgp, trueATE),
    by = "dgp"
  ) %>%
  mutate(relative_bias = abs(mean_bias / trueATE))


df_coverage_complete <- df %>% filter(metric == "coverage") %>%
  group_by(model, dgp) %>%
  summarise(coverage = mean(value, na.rm = TRUE))

df_ci_length_complete <- df %>% filter(metric == "ci_length") %>%
  group_by(dgp, model) %>%
  summarise(mean_ci_length = mean(value, na.rm = TRUE)) 

df_ci_length_sdy <- df_ci_length_complete %>%
  left_join(
    dgps %>% select(dgp, sd_y),
    by = "dgp"
  ) %>%
  mutate(normalized_ci_length = abs(mean_ci_length / sd_y))

# Model & DGP Summary
by <- join_by(model, dgp)
df_models_dgps_all_metrics <- left_join(df_rmse_complete, df_coverage_complete) %>% 
  left_join(df_ci_length_complete) %>%
  select(-sd)

write.csv(df_models_dgps_all_metrics, paste0(PATH_RESULTS, "2026-04-10_model_dgp_summary_metrics.csv"), row.names = FALSE)

# ___________________________________________________________

## Data frames to use for graphs (remove outliers) ----
df_rmse <- df_rmse_complete %>% 
  filter(rmse < 10) 

df_coverage <- df_coverage_complete %>%
  filter(coverage > 0.1)

# ____________________________________________________________ 

# Compare metrics per model ----
# Aggregated DF (Coverage) - per model
df_coverage_agg_model <- df_coverage_complete %>%
  group_by(model) %>%
  summarise(mean_coverage = mean(coverage, na.rm = TRUE),
            median_coverage = median(coverage, na.rm = TRUE),
            sd_coverage = sd(coverage, na.rm = TRUE)) 

# Aggregated DF (RMSE) - per model
df_rmse_agg_model <- df_rmse_complete %>%
  group_by(model) %>%
  summarise(mean_rmse = mean(rmse, na.rm = TRUE),
            median_rmse = median(rmse, na.rm = TRUE),
            sd_rmse = sd(rmse, na.rm = TRUE)) 

df_bias_agg_model <- df_bias_abs_rel %>%
  group_by(model) %>%
  summarise(mean_bias = mean(mean_bias, na.rm = TRUE),
            median_bias = median(mean_bias, na.rm = TRUE),
            mean_rel_bias = mean(relative_bias, na.rm = TRUE),
            sd_bias = sd(mean_bias, na.rm = TRUE)) 

# Aggregated DF (CI Length) - per model
df_ci_agg_model <- df_ci_length_complete %>%
  group_by(model) %>%
  summarise(ci_length_mean = mean(mean_ci_length, na.rm = TRUE),
            ci_length_median = median(mean_ci_length, na.rm = TRUE),
            sd_ci_length = sd(mean_ci_length, na.rm = TRUE))

# ________________________________________________________

# Model Summary DF ----
by <- join_by(model)
df_model_metrics_complete <- full_join(df_rmse_agg_model, df_ci_agg_model) %>% full_join(df_coverage_agg_model)
df_model_summary <- df_model_metrics_complete %>% select(model, mean_rmse, mean_coverage, ci_length_mean) 

write.csv(df_model_summary, paste0(PATH_RESULTS, "2026-04-10_model_summaries_metrics.csv"), row.names = FALSE)

# ________________________________________________________

# Boxplots (& Scatterplots) for models ----
# Coverage - models
# Boxplot
ggplot(df_coverage, aes(x = model, y = coverage)) +
  geom_boxplot() +
  labs(
    x = "Model",
    y = "Coverage"
  ) +
  theme_minimal()

# Scatterplot
ggplot(df_coverage, aes(x = model, y = coverage)) +
  geom_point() +
  labs(x = "DGP", y = "Coverage", title = "Coverage by model (Outlier removed)") +
  theme(legend.position = "top") +
  scale_color_brewer(palette="Dark2")

# RMSE - models
# Boxplot
ggplot(df_rmse, aes(x = model, y = rmse)) +
  geom_boxplot() +
  labs(
    x = "Model",
    y = "RMSE"
  ) +
  theme_minimal()

# Scatterplot
ggplot(df_rmse, aes(x = model, y = rmse)) +
  geom_point() +
  labs(x = "DGP", y = "RMSE", title = "RMSE by model (Outlier removed)") +
  theme(legend.position = "top") +
  scale_color_brewer(palette="Dark2")

# CI Length - models
# Boxplot
ggplot(df_ci_length_complete, aes(x = model, y = mean_ci_length)) +
  geom_boxplot() +
  labs(
    x = "Model",
    y = "CI Length"
  ) +
  theme_minimal()

# Scatterplot
ggplot(df_ci_length_complete, aes(x = model, y = mean_ci_length)) +
  geom_point() +
  labs(x = "DGP", y = "CI Length", title = "Ci Length by model (Outlier removed)") +
  theme(legend.position = "top") +
  scale_color_brewer(palette="Dark2")

# __________________________________________

# Long format (Keine Ahnung, kann vlt weg) ----
df <- imap_dfr(results, function(metric_list, dgp_name) {
  imap_dfr(metric_list, function(model_list, metric_name) {
    imap_dfr(model_list, function(iter_list, model_name) {
      tibble(
        dgp = dgp_name,
        metric = metric_name,
        model = model_name,
        iteration = seq_along(iter_list),
        value = unlist(iter_list)
      )
    })
  })
})


df <- na.omit(df)

write.csv(df, paste0(PATH_RESULTS, "results_dataframe_long_format_rmse.csv"))

# Ranking ----
# RMSE
df_rmse <- df %>% filter(metric == "rmse") %>%
  group_by(dgp, model) %>%
  summarise(mean_rmse = mean(value, na.rm = TRUE))

ranked_rmse <- df_rmse %>%
  group_by(dgp) %>%
  mutate(rank = rank(mean_rmse, ties.method = "first")) %>%
  ungroup()

rank_counts_rmse <- ranked_rmse %>%
  count(model, rank) %>%
  group_by(model) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

ranked_rmse_summary <- ranked_rmse %>%
  group_by(model) %>%
  summarise(mean_rank = mean(rank))

# Coverage
df_coverage <- df %>% filter(metric == "coverage") %>%
  group_by(dgp, model) %>%
  summarise(mean_coverage = mean(value, na.rm = TRUE))

ranked_coverage <- df_coverage %>%
  group_by(dgp) %>%
  mutate(rank = rank(-mean_coverage, ties.method = "first")) %>%
  ungroup()

rank_counts_coverage <- ranked_coverage %>%
  count(model, rank) %>%
  group_by(model) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

ranked_coverage_summary <- ranked_coverage %>%
  group_by(model) %>%
  summarise(mean_rank = mean(rank))


# Tables and plots for DGPS 

# Join all the 

# Which DGPS have lowest RMSE?
df_rmse_dgp <- df %>% filter(metric == "rmse") %>%
  group_by(dgp) %>%
  summarise(mean_rmse = mean(value, na.rm = TRUE),
            sd = sd(value, na.rm = TRUE)) 

rmse_over_3 <- df_rmse %>% 
  filter(mean_value > 0.3)

table(rmse_over_3$dgp)
table(rmse_over_3$model)

# Which DGPS have lowest NORMALIZED RMSE?
df_nrmse_overview <- df %>% filter(metric == "rmse_normalized") %>%
  group_by(dgp) %>%
  summarise(mean_nrmse = mean(value, na.rm = TRUE),
            sd = sd(value, na.rm = TRUE)) 


df_nrmse <- df %>% filter(metric == "rmse_normalized") %>%
  group_by(dgp, model) %>%
  summarise(mean_nrmse = mean(value, na.rm = TRUE))

rmse_over_3 <- df_rmse %>% 
  filter(mean_value > 0.3)

table(rmse_over_3$dgp)
table(rmse_over_3$model)


# RMSE and NRMSE
by <- join_by(dgp, model)
df_both_rmse <- left_join(df_rmse, df_nrmse)


# Which DGPS have highest Coverage?
df_coverage_dgp <- df %>% filter(metric == "coverage") %>%
  group_by(dgp) %>%
  summarise(mean_coverage = mean(value, na.rm = TRUE),
            sd = sd(value, na.rm = TRUE)) 

df_coverage_overview$sd_1 <- df_coverage %>% group_by(dgp) %>%
  summarise(sd = sd(mean_coverage, na.rm = TRUE)) %>%
  select(sd)

coverage_under_95 <- df_coverage %>% 
  filter(mean_value < 0.95)

table(coverage_under_95$dgp)
table(coverage_under_95$model)

coverage_under_90 <- df_coverage %>% 
  filter(mean_value < 0.90)

table(coverage_under_90$dgp)
table(coverage_under_90$model)

# Which DGPS have longest intervals?
df_ci_length_dgp <- df %>% filter(metric == "ci_length") %>%
  group_by(dgp) %>%
  summarise(mean_ci_length = mean(value, na.rm = TRUE),
            sd = sd(value, na.rm = TRUE)) 

# Scatter Plot CI Length and Coverage
by <- join_by(dgp, model)
ci_and_coverage <- left_join(df_ci_length, df_coverage) %>% 
  filter(mean_coverage > 0.1)

ggplot(joined_table, aes(x = mean_ci_length, y = mean_coverage,
                         col = model)) +
  geom_point() +
  labs(x = "Ci Length", y = "Coverage", title = "CI Length and Coverage")+
  theme(legend.position = "top") +
  scale_color_brewer(palette="Dark2")



# Scatter Plot RMSE and Coverage
by <- join_by(dgp, model)
joined_table <- left_join(df_rmse, df_coverage) %>% 
  filter(mean_coverage > 0.1)

ggplot(joined_table, aes(x = mean_rmse, y = mean_coverage,
                    col = model)) +
  geom_point() +
  labs(x = "RMSE", y = "Coverage", title = "RMSE and Coverage")+
  theme(legend.position = "top") +
  scale_color_brewer(palette="Dark2")


# Plots DGPs ----

# Average RMSE over DGPS
ggplot(df_rmse, aes(x = dgp, y = rmse,
                    col = model)) +
  geom_beeswarm() +
  labs(x = "DGP", y = "RMSE", title = "RMSE by dgp and model (Outlier removed)")+
  theme(legend.position = "top") +
  scale_color_brewer(palette="Accent")


# Average Coverage over DGPS
ggplot(df_coverage, aes(x = dgp, y = coverage,
                    col = model)) +
  geom_beeswarm() +
  labs(x = "DGP", y = "Coverage", title = "Coverage by dgp and model (Outlier removed)") +
  theme(legend.position = "top") +
  scale_color_brewer(palette="Accent")


# Average CI Length
ggplot(df_ci_length_complete, aes(x = dgp, y = mean_ci_length,
                        col = model)) +
  geom_beeswarm() +
  labs(x = "DGP", y = "CI Length", title = "CI Length by dgp and model (Outlier removed)") +
  theme(legend.position = "top") +
  scale_color_brewer(palette="Accent")


# Average NORMALIZED RMSE ----
df_nrmse <- df %>% filter(metric == "rmse_normalized") %>%
  group_by(model, dgp) %>%
  summarise(mean_nrmse = mean(value, na.rm = TRUE),
            sd = sd(value, na.rm = TRUE)) %>% 
  filter(mean_nrmse < 1) # remove one outlier: DGP30, weighting

ggplot(df_nrmse, aes(x = dgp, y = mean_nrmse,
                     col = model)) +
  geom_point() +
  labs(x = "DGP", y = "RMSE (normalized)", title = "RMSE (normalized) by dgp and model (Outlier removed)")+
  theme(legend.position = "top") +
  scale_color_brewer(palette="Dark2")


# Relative bias
ggplot(df_bias_abs_rel %>% filter(relative_bias < 1), aes(x = dgp, y = relative_bias,
                                  col = model)) +
  geom_beeswarm() +
  labs(x = "DGP", y = "Relative bias", title = "Relative bias by dgp and model (Outlier removed)") +
  theme(legend.position = "top") +
  scale_color_brewer(palette="Accent")

# Normalized RMSE SD(Y)
ggplot(df_rmse_sdy %>% filter(normalized_rmse < 0.4), aes(x = dgp, y = normalized_rmse,
                                                          col = model)) +
  geom_beeswarm() +
  labs(x = "DGP", y = "RMSE/SD(Y)", title = "Normalized RMSE by dgp and model (Outlier removed)") +
  theme(legend.position = "top") +
  scale_color_brewer(palette="Accent")

# Normalized SD(Y)
ggplot(df_ci_length_sdy, aes(x = dgp, y = normalized_ci_length,
                                                          col = model)) +
  geom_beeswarm() +
  labs(x = "DGP", y = "Average CI Length/SD(Y)", title = "Normalized CI Length by dgp and model (Outlier removed)") +
  theme(legend.position = "top") +
  scale_color_brewer(palette="Accent")
