library(purrr)
library(dplyr)
library(tidyr)

PATH_RESULTS <- "C:/Users/luise/Documents/Masterarbeit/Results/"

results <- readRDS(paste0(PATH_RESULTS, "2026-03-1_complete_results_nrmse.RData"))
summaries <- readRDS(paste0(PATH_RESULTS, "2026-03-11_complete_summaries_nrmse.RData"))
averages <- readRDS(paste0(PATH_RESULTS, "2026-02-25_avgs_NOMATCHING.RData")) %>% select(-ate_estimate)

df <- map_dfr(names(summaries), function(dgp_name){
  summaries[[dgp_name]] %>%
    mutate(
      model = rownames(.),
      dgp = dgp_name) %>%
    select(dgp, model, coverage)
})

all_summaries <- bind_rows(summaries, .id = "DGP")

library(ggplot2)

## Data frames to use for tables (96 rows)
df_rmse <- df %>% filter(metric == "rmse") %>%
  group_by(model, dgp) %>%
  summarise(mean_rmse = mean(value, na.rm = TRUE),
            sd = sd(value, na.rm = TRUE))

df_coverage <- df %>% filter(metric == "coverage") %>%
  group_by(model, dgp) %>%
  summarise(coverage = mean(value, na.rm = TRUE),
            sd = sd(value, na.rm = TRUE))

df_ci_length <- df %>% filter(metric == "ci_length") %>%
  group_by(dgp, model) %>%
  summarise(mean_ci_length = mean(value, na.rm = TRUE),
            sd = sd(value, na.rm = TRUE)) 

## Data frames to use for graphs (remove outliers)
df_rmse <- df_rmse %>% 
  filter(mean_rmse < 1) 

df_coverage <- df_coverage %>%
  filter(coverage > 0.1)

# Compare metrics per model
# Coverage - models

# Aggregated DF (Coverage) - per model
df_coverage_agg_model <- df_coverage %>%
  group_by(model) %>%
  summarise(mean_coverage = mean(coverage, na.rm = TRUE),
            median_coverage = median(coverage, na.rm = TRUE),
            sd = sd(coverage, na.rm = TRUE)) 

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
  labs(x = "DGP", y = "Coverage", title = "Coverage by dgp and model (Outlier removed)") +
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
ggplot(df_rmse, aes(x = model, y = mean_rmse)) +
  geom_point() +
  labs(x = "DGP", y = "RMSE", title = "RMSE by dgp and model (Outlier removed)") +
  theme(legend.position = "top") +
  scale_color_brewer(palette="Dark2")


# CI Length - models
# Aggregated DF (CI Length) - per model
df_ci_agg_model <- df_ci_length %>%
  group_by(model) %>%
  summarise(mean_ci_length = mean(mean_ci_length, na.rm = TRUE),
            median_ci_length = median(mean_ci_length, na.rm = TRUE),
            sd = sd(mean_ci_length, na.rm = TRUE)) 

ggplot(df_ci_length, aes(x = model, y = ci_length)) +
  geom_boxplot() +
  labs(
    x = "Model",
    y = "Ci Length"
  ) +
  theme_minimal()


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


# Overview, which model is on which place how many times?
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


# Which DGPS have lowest RMSE?
df_rmse_overview <- df %>% filter(metric == "rmse") %>%
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
df_coverage_overview <- df %>% filter(metric == "coverage") %>%
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
df_coverage_overview <- df %>% filter(metric == "ci_length") %>%
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



# Average RMSE over DGPS
ggplot(df_rmse, aes(x = dgp, y = mean_rmse,
                    col = model)) +
  geom_point() +
  labs(x = "DGP", y = "RMSE", title = "RMSE by dgp and model (Outlier removed)")+
  theme(legend.position = "top") +
  scale_color_brewer(palette="Dark2")

# Average NORMALIZED RMSE
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


# Average Coverage over DGPS
ggplot(df_coverage, aes(x = dgp, y = mean_coverage,
                    col = model)) +
  geom_point() +
  labs(x = "DGP", y = "Coverage", title = "Coverage by dgp and model (Outlier removed)") +
  theme(legend.position = "top") +
  scale_color_brewer(palette="Dark2")


# Average CI Length
ggplot(df_ci_length, aes(x = dgp, y = mean_ci_length,
                        col = model)) +
  geom_point() +
  labs(x = "DGP", y = "CI Length", title = "CI Length by dgp and model (Outlier removed)") +
  theme(legend.position = "top") +
  scale_color_brewer(palette="Dark2")
