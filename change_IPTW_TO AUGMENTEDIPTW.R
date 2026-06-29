all_results_aiptw <- read.csv("w:\\Masterarbeit\\Results\\2026-06-29_aiptw_default64.csv")

library(dplyr)

summary_aiptw <- all_results_aiptw %>%
  group_by(dgp) %>%
  summarise(
    rmse = sqrt(mean(value[metric == "ate_bias"]^2)),
    coverage = mean(value[metric == "coverage"]),
    interval_length = mean(value[metric == "ci_length"]),
    .groups = "drop"
  )


summary_aiptw$rmse_rel <- round(summary_aiptw$rmse / abs(dgps$trueATE), 3)
summary_aiptw$ci_rel <- round(summary_aiptw$interval_length / abs(dgps$trueATE), 3)

all_results_aiptw <- write.csv(all_results_aiptw, "w:\\Masterarbeit\\Results\\2026-06-29_aiptw_default_summary.csv")



________________________________
all_results_other <- read.csv("w:\\Masterarbeit\\Results\\2026-04-30_model_dgp_summary_ALL_metrics.csv")

all_results_other$rmse_rel <- round(all_results_other$rmse_rel, 3)
all_results_other$ci_length_rel <- round(all_results_other$ci_length_rel, 3)

write.csv(all_results_other, "w:\\Masterarbeit\\Results\\2026-06-29_aiptw_default_summary_old.csv")


all_results_other <- all_results_other %>%
  filter(model != "weighting")

all_results_other_selected <- all_results_other %>% select(model, dgp, rmse_rel, coverage, ci_length_rel)

names(summary_aiptw)[names(summary_aiptw) == 'ci_rel'] <- 'ci_length_rel'

summary_aiptw$model <- "aiptw"

summary_aiptw_new <- summary_aiptw %>% select(model, dgp, rmse_rel, coverage, ci_length_rel)


new_df <- rbind(all_results_other_selected, summary_aiptw_new)

write.csv(new_df, "w:\\Masterarbeit\\Results\\2026-06-29_dgp_summaries_FINAL.csv")

