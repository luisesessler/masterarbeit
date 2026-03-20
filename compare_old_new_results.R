results_bart <- read.csv(paste0(PATH_RESULTS, "bart_results_new.csv"))

bias_agg_new <- results_bart %>% filter(metric == "ate_bias") %>%
  group_by(model, dgp) %>%
  summarise(mean_bias = mean(abs(value)))

bias_agg_old <- df %>% filter(metric == "ate_bias" & model %in% c("BART_one_model", "BART_two_models", "BART_ps_bart", "BART_ps_glm")) %>%
  group_by(model, dgp) %>%
  summarise(mean_bias = mean(abs(value)))

bias_agg_old$model <- recode(bias_agg_old$model,
                 "BART_one_model" = "s_learner",
                 "BART_two_models" = "t_learner",
                 "BART_ps_bart" = "ps_bart",
                 "BART_ps_glm" = "ps_glm"
)

by <- join_by(dgp, model)
df_both_rmse <- left_join(df_rmse, df_nrmse)

ggplot(bias_agg, aes(x = dgp, y = mean_bias, col = model)) +
  geom_point()
