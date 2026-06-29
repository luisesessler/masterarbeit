new_df <- read.csv("w:\\Masterarbeit\\Results\\2026-06-29_dgp_summaries_FINAL.csv")


new_df <- new_df %>%
  mutate(
    model = recode(
      model,
      "BART_ps-bart" = "BART/PS-BART",
      "BART_ps-glm" = "BART/PS-GLM",
      "BART_s-learner" = "BART/S-Learner",
      "BART_t-learner" = "BART/T-Learner",
      "causal_forest" = "Causal forest",
      "aiptw" = "AIPTW"
    )
  )


plot_data <- new_df %>%   # replace with your data frame name
  pivot_longer(
    cols = c(rmse_rel, coverage, ci_length_rel),
    names_to = "metric",
    values_to = "value"
  )


#--------------- BOXPLOTS

ggplot(new_df %>% filter(coverage > 0.1), aes(x = model, y = rmse_rel)) +
  geom_boxplot() +
  labs(
    x = "Model",
    y = "RMSE/true ATE"
  ) +
  theme_minimal()

ggplot(new_df %>% filter(coverage > 0.1), aes(x = model, y = coverage)) +
  geom_boxplot() +
  labs(
    x = "Model",
    y = "Coverage"
  ) +
  theme_minimal()

ggplot(new_df %>% filter(coverage > 0.1), aes(x = model, y = ci_length_rel)) +
  geom_boxplot() +
  labs(
    x = "Model",
    y = "Interval length/true ATE"
  ) +
  theme_minimal()

#----------------- Scatterplots

# Average RMSE over DGPS
ggplot(new_df %>% filter(coverage > 0.1), aes(x = dgp, y = rmse_rel,
                    col = model)) +
  geom_beeswarm() +
  labs(x = "DGP", y = "RMSE", title = "RMSE by dgp and model (Outlier removed)")+
  theme(legend.position = "top") +
  scale_color_brewer(palette="Accent")


# Average Coverage over DGPS
ggplot(new_df %>% filter(coverage > 0.1), aes(x = dgp, y = coverage,
                        col = model)) +
  geom_beeswarm() +
  labs(x = "DGP", y = "Coverage", title = "Coverage by dgp and model (Outlier removed)") +
  theme(legend.position = "top") +
  scale_color_brewer(palette="Accent")


# Average CI Length
ggplot(new_df, aes(x = dgp, y = ci_length_rel,
                                  col = model)) +
  geom_beeswarm() +
  labs(x = "DGP", y = "CI Length", title = "CI Length by dgp and model (Outlier removed)") +
  theme(legend.position = "top") +
  scale_color_brewer(palette="Accent")

