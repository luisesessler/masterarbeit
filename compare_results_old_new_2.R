library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

PATH_RESULTS <- "C:/Users/Amelie/Documents/Luise Masterarbeit/Results/"
results_bart <- read.csv(paste0(PATH_RESULTS, "bart_results_new.csv"))
results_old <- read.csv(paste0(PATH_RESULTS, "results_dataframe_long_format_rmse.csv"))

# Join full results (only BART models)
results_old$model <- recode(results_old$model,
                             "BART_one_model" = "s_learner",
                             "BART_two_models" = "t_learner",
                             "BART_ps_bart" = "ps_bart",
                             "BART_ps_glm" = "ps_glm"
)

colnames(results_old)[which(names(results_old) == "value")] <- "value_old"
colnames(results_bart)[which(names(results_bart) == "value")] <- "value_new"

results_bart$dgp <- paste0("dgp", results_bart$dgp)

by <- join_by(model, dgp, iteration)
df_old_new_bart <- left_join(results_old, results_bart)

# Calculate average metrics

df_abs_bias <- df_old_new_bart %>% filter(metric == "ate_bias") %>%
  group_by(model) %>%
  summarise(mean_bias_old = mean(abs(value_old)),
            mean_bias_new = mean(abs(value_new)),
            diff = mean_bias_old - mean_bias_new)

df_coverage <- df_old_new_bart %>% filter(metric == "coverage") %>%
  group_by(model) %>%
  summarise(mean_coverage_old = mean(value_old),
            mean_coverage_new = mean(value_new),
            diff = mean_coverage_old - mean_coverage_new)

df_ci <- df_old_new_bart %>% filter(metric == "ci_length") %>%
  group_by(model) %>%
  summarise(mean_ci_old = mean(abs(value_old)),
            mean_ci_new = mean(abs(value_new)),
            diff = mean_ci_old - mean_ci_new)

# Aggregated
df_bias_dgps <- df_old_new_bart %>% filter(metric == "ate_bias" & !is.na(value_new)) %>%
  group_by(model, dgp) %>%
  summarise(mean_bias_old = mean(abs(value_old)),
            mean_bias_new = mean(abs(value_new)),
            diff = round(mean_bias_old - mean_bias_new, 3))

df_coverage_dgps <- df_old_new_bart %>% filter(metric == "coverage" & !is.na(value_new)) %>%
  group_by(model, dgp) %>%
  summarise(coverage_old = mean(abs(value_old)),
            coverage_new = mean(abs(value_new)),
            diff = round(coverage_old - coverage_new, 3))

df_old_new_bart %>% filter(metric == "ate_bias" & !is.na(value_new)) %>%
  group_by(model) %>%
  summarise(mean_diff = mean(abs(value_old) - mean(abs(value_new))))

df_old_new_bart %>% filter(metric == "coverage" & !is.na(value_new)) %>%
  group_by(model) %>%
  summarise(mean_diff = mean(abs(value_old) - mean(abs(value_new))))

df_old_new_bart %>% filter(metric == "ci_length" & !is.na(value_new)) %>%
  group_by(model) %>%
  summarise(mean_diff = mean(abs(value_old) - mean(abs(value_new))))

df_bias_dgps %>% 
  group_by(model) %>%
  summarise(mean_diff = mean(diff))
# negative diff values sind gut für 

ggplot(data = df_bias_dgps, aes(x = model, y = diff)) +
  geom_point()


# Comparison plots
df_old_new_long <- df_old_new_bart %>%
  pivot_longer(
    cols = c(value_new, value_old),
    names_to = "type",
    values_to = "value"
  )

df_long_dgp <- df_old_new_long

df_long_dgp$dgp <- ifelse(df_long_dgp$type == "value_new", paste0(df_long_dgp$dgp, "_new"), df_long_dgp$dgp)
  
grouped_bias_old_new <- df_long_dgp %>% filter(metric == "ate_bias") %>%
  group_by(dgp, model) %>%
  summarise(mean_bias = mean(abs(value))) %>%
  filter(dgp %in% c("dgp32", "dgp32_new", "dgp37", "dgp37_new", "dgp38", "dgp38_new"))

ggplot(grouped_bias_old_new, aes(x = dgp, y = mean_bias, col = model)) +
  geom_point()


# Join aggregated for models & dgps
bias_agg_new <- results_bart %>% filter(metric == "ate_bias") %>%
  group_by(model, dgp) %>%
  summarise(mean_bias_new = mean(abs(value)))

bias_agg_old <- results_old %>% filter(metric == "ate_bias" & model %in% c("s_learner", "t_learner", "ps_bart", "ps_glm")) %>%
  group_by(model, dgp) %>%
  summarise(mean_bias_old = mean(abs(value_old)))

by <- join_by(dgp, model)
df_both_bias <- left_join(bias_agg_old, bias_agg_new)

df_both_bias$diff <- round(df_both_bias$mean_bias_old - df_both_bias$mean_bias_new, 3)

# Means
df_mean_bias <- df_both_bias %>% 
  group_by(model) %>%
  summarise(mean_model_bias_old = mean(abs(mean_bias_old)),
            mean_model_bias_new = mean(abs(mean_bias_new)))


ggplot(bias_agg_new, aes(x = dgp, y = mean_bias_new, col = model)) +
  geom_point()


