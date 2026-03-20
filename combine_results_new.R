# s-learner

PATH_RESULTS <- "W:/Masterarbeit/Results/"
PATH_OVERVIEW <-  "W:/Masterarbeit/Daten/trueATE/dgp_overview.csv"

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

for (i in 16) {
  dgp_index <- cont_dgpis$DGPid[i]
  
  s_learner <- readRDS(paste0(PATH_RESULTS, "2026-03-19_s-learner-", dgp_index, ".RData"))
  t_learner <- readRDS(paste0(PATH_RESULTS, "2026-03-19_t-learner_", dgp_index, ".RData"))
  ps_bart   <- readRDS(paste0(PATH_RESULTS, "2026-03-19_ps-bart_", dgp_index, ".RData"))
  ps_glm    <- readRDS(paste0(PATH_RESULTS, "2026-03-19_ps-glm_", dgp_index, ".RData"))
  
  results_list[[length(results_list) + 1]] <- bind_rows(
    convert_to_df(s_learner, dgp_index, "s_learner"),
    convert_to_df(t_learner, dgp_index, "t_learner"),
    convert_to_df(ps_bart, dgp_index, "ps_bart"),
    convert_to_df(ps_glm, dgp_index, "ps_glm")
  )
}

final_df <- bind_rows(results_list)


# nur rumspielen
coverage_agg <- final_df %>% filter(metric == "coverage") %>%
  group_by(model) %>%
  summarise(mean_coverage = mean(value))

bias_agg <- final_df %>% filter(metric == "ate_bias") %>%
  group_by(model) %>%
  summarise(mean_rmse = mean(abs(value)))