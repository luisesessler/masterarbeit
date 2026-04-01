df_alt_6methods <- read.csv(paste0(PATH_RESULTS, "results_dataframe_long_format_rmse.csv"))

df_6models <- df_alt_6methods[ ,-1] %>% filter(metric %in% c("ate_estimate", "ate_bias", "ci_length", "coverage"))

df_5models <-  df_6models %>% filter(model != "BART_two_models")

unique(df_5models$model)

df_all_results <- rbind(df_5models, chains10_t)

# Analyze
summaries <- df_all_results %>% group_by(model, metric) %>%
  summarise(mean_value = mean(abs(value))
)

write.csv(df_all_results, paste0(PATH_RESULTS, "2026-04-01_all_results_10chains.csv"))





df_alt_6methods %>% filter(model == "BART_two_models") %>%
  group_by(metric) %>% summarise(
  mean_value = mean(abs(value))
)