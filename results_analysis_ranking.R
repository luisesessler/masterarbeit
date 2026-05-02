# RMSE

ranked_rmse <- df_dgps_metrics %>% 
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
