# Compare 4 chains vs 10 chains T-Learner

dgpis <- read.csv(PATH_OVERVIEW)
cont_dgpis <- dgpis %>% filter(binary == 0)

convert_to_df <- function(obj, dgp_id, model_name) {
  obj %>%
    imap_dfr(function(metric_list, metric_name) {
      metric_list %>%
        imap_dfr(function(values, model) {
          
          # skip if all NA
          if (all(is.na(values))) return(NULL)
          
          tibble(
            dgp = dgp_id,
            model = model,
            metric = metric_name,
            iteration = seq_along(values),
            value = as.numeric(values)
          )
        })
    }) %>%
    mutate(model = model_name)  # overwrite with known model name if needed
}


# for (i in 1:nrow(cont_dgpis)){
results_list <- list()

# Convert 10 chains run to df

for (i in c(4:7)) {
  dgp_index <- cont_dgpis$DGPid[i]

  t_learner <- readRDS(paste0(PATH_RESULTS, "2026-03-31_t-learner_", dgp_index, ".RData"))

  
  results_list[[length(results_list) + 1]] <- bind_rows(
    convert_to_df(t_learner, dgp_index, "t_learner")
  )
}

final_df_10chains <- bind_rows(results_list)
write.csv(final_df_10chains, paste0(PATH_RESULTS, "t_learner_10chains.csv"), row.names = FALSE)

df_bart_4chains <- read.csv(paste0(PATH_RESULTS, "bart_results_new.csv"))

chains4_t <- df_bart_4chains %>% filter(model == "t_learner")

chains10_t <- final_df_10chains %>% filter(model == "t_learner")

chains4_t %>% group_by(model, metric) %>% summarise(
  mean_value = mean(abs(value))
)

chains10_t %>% group_by(model, metric) %>% summarise(
  mean_value = mean(abs(value))
)
