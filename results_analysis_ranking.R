df_dgps_metrics <- read.csv("w:\\Masterarbeit\\Results\\2026-06-29_dgp_summaries_FINAL.csv")

# RMSE

ranked_rmse_rel <- df_dgps_metrics %>% 
  select(model, dgp, rmse_rel) %>%
  group_by(dgp) %>%
  mutate(rank = rank(rmse_rel, ties.method = "first")) %>%
  ungroup()

rank_counts_rmse_rel <- ranked_rmse_rel %>%
  count(model, rank) %>%
  group_by(model) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

write.csv(rank_counts_rmse_rel, paste0(PATH_RESULTS,"2026-06-29_ranking_rmse_rel.csv"), row.names = FALSE)


ranked_rmse_summary <- ranked_rmse %>%
  group_by(model) %>%
  summarise(mean_rank = mean(rank))

# S- vs. T-Learner
ranked_rmse <- df_dgps_metrics %>% 
  filter(model %in% c("BART_s-learner", "BART_t-learner")) %>%
  select(model, dgp, rmse) %>%
  group_by(dgp) %>%
  mutate(rank = rank(rmse, ties.method = "first")) %>%
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

ranked_coverage <- df_dgps_metrics %>% 
  select(model, dgp, coverage) %>%
  group_by(dgp) %>%
  mutate(rank = rank(-coverage, ties.method = "first")) %>%
  ungroup()

rank_counts_coverage <- ranked_coverage %>%
  count(model, rank) %>%
  group_by(model) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

write.csv(rank_counts_coverage, paste0(PATH_RESULTS,"2026-06-29_ranking_coverage.csv"), row.names = FALSE)


ranked_coverage_summary <- ranked_coverage %>%
  group_by(model) %>%
  summarise(mean_rank = mean(rank))
