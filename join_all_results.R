# Merge all the results

models <- c(
  "BART_one_model",
  "BART_two_models",
  "BART_ps_glm",
  "BART_ps_bart",
  "causal_forest",
  "weighting",
  "matching"
)

complete_results <- list()

dgp_numbers <- cont_dgpis$DGPid

for (i in 1:16){
  dgp_number <- dgp_numbers[i]
  complete_results[[paste0("dgp", dgp_number)]] <-  list()
}

for (i in 1:16){
  
  dgp_number <- dgp_numbers[i]
  
  metrics <- c(
    "ate_estimate", "pehe", "ate_bias", "ci_length", "coverage"
  )
  
  complete_results[[paste0("dgp", dgp_number)]] <- 
    setNames(
      lapply(metrics, function(metric_name) {
        setNames(vector("list", length(models)), models)
      }),
      metrics
    )
}

insert_model_results <- function(complete_results, model_results) {
  
  for (dgp_name in names(model_results)) {
    for (metric_name in names(model_results[[dgp_name]])) {
      for (model_name in names(model_results[[dgp_name]][[metric_name]])) {
        
        values <- model_results[[dgp_name]][[metric_name]][[model_name]]
        
        # Only overwrite if not all NA
        if (!all(is.na(values))) {
          complete_results[[dgp_name]][[metric_name]][[model_name]] <- values
        }
      }
    }
  }
  
  return(complete_results)
}

# BART


# BART 2 Model
results_bart2model <- readRDS(paste0(PATH_RESULTS, "2025-02-25_29-64_2models.RData"))

complete_results <- insert_model_results(complete_results, results_bart2model)

# Weighting
res_weighting <- readRDS(paste0(PATH_RESULTS, "2025-02-25_29-64_weighting.RData"))

complete_results <- insert_model_results(complete_results, res_weighting)

# Causal forest
results_cf <- readRDS(paste0(PATH_RESULTS, "2026-02-25_29-64_CF.RData"))

complete_results <- insert_model_results(complete_results, results_cf)


# BART one model & BART with PS
results_BART_40_59_64 <- readRDS(paste0(PATH_RESULTS, "2026-02-24_59-64u40_bart.RData"))
results_BART_29 <- readRDS(paste0(PATH_RESULTS, "2026-02-25_29_BART.RData"))
results_BART_30 <- readRDS(paste0(PATH_RESULTS, "2026-02-25_30_BART.RData"))
results_BART_31 <- readRDS(paste0(PATH_RESULTS, "2026-02-25_31_BART.RData"))
results_BART_39 <- readRDS(paste0(PATH_RESULTS, "2025-02-25_39_BART.RData"))


results_BART <- results_BART_40_59_64
results_BART$dgp29 <- results_BART_29
results_BART$dgp30 <- results_BART_30
results_BART$dgp31 <- results_BART_31
results_BART$dgp39 <- results_BART_39$dgp39


results_bart_32_38 <- readRDS(paste0(PATH_RESULTS, "2026-02-25_32-38_BART.RData"))
results_BART$dgp32 <- results_bart_32_38$dgp32
results_BART$dgp37 <- results_bart_32_38$dgp37
results_BART$dgp38 <- results_bart_32_38$dgp38

complete_results <- insert_model_results(complete_results, results_BART)

saveRDS(complete_results, paste0(PATH_RESULTS, "2026-02-25_complete_results_NOMATCHING.RData"))

#Add RMSE

for (dgp_name in names(complete_results)) {
  
  complete_results[[dgp_name]][["rmse"]] <-
    setNames(
      lapply(models, function(m) rep(NA, 100)),
      models
    )
}


add_iterationwise_rmse <- function(complete_results, models) {
  
  for (dgp_name in names(complete_results)) {
    
    for (model_name in models) {
      
      bias_vec <- complete_results[[dgp_name]][["ate_bias"]][[model_name]]
      
      # Only compute if bias exists and is not all NA
      if (!is.null(bias_vec) && !all(is.na(bias_vec))) {
        
        complete_results[[dgp_name]][["rmse"]][[model_name]] <-
          sqrt(bias_vec^2)
      }
    }
  }
  
  return(complete_results)
}

complete_results <- add_iterationwise_rmse(complete_results, models)

# Delete PEHE
for (dgp_name in names(complete_results)) {
  complete_results[[dgp_name]][["pehe"]] <- NULL
}

# Summaries

metrics <- setdiff(names(complete_results[[1]]), "pehe")

create_dgp_summary <- function(dgp_list, models, metrics) {
  
  summary_df <- data.frame(model = models)
  
  for (metric in metrics) {
    metric_values <- sapply(models, function(model_name) {
      vec <- dgp_list[[metric]][[model_name]]
      if (is.null(vec)) return(NA)
      mean(vec, na.rm = TRUE)
    })
    
    summary_df[[metric]] <- metric_values
  }
  
  return(summary_df)
}

dgp_summaries <- lapply(complete_results, function(dgp_list) {
  create_dgp_summary(dgp_list, models, metrics)
})

saveRDS(complete_results, paste0(PATH_RESULTS, "2026-02-25_complete_summaries_NOMATCHING.RData"))

# All summaries
# Combine all DGPs into one table
all_summaries <- bind_rows(dgp_summaries, .id = "DGP")

# Compute average over DGPs for each model
avg_over_dgps <- all_summaries %>%
  group_by(model) %>%
  summarise(across(-DGP, ~ mean(.x, na.rm = TRUE)))