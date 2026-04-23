weighting_bart_agg <- df_tuning_results %>% group_by(dgp, metric) %>%
  summarise(mean_value = mean(abs(value)))

weighting_glm <- read.csv(paste0(PATH_RESULTS, "2026-04-23_weighting_glm64.csv"))

weighting_glm_agg <- weighting_glm %>% 
  filter(iteration < 21) %>%
  group_by(dgp, metric) %>%
  summarise(mean_value = mean(abs(value)))


together <- cbind(weighting_glm_agg, weighting_bart_agg)

together$diff <- together$mean_value...3 - together$mean_value...6