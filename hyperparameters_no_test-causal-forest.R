# Amelies Computer
# PATH_DATASETS <- "C:/Users/Amelie/Documents/Luise Masterarbeit/Daten/"  
# PATH_TRUE_ATES <- "C:/Users/Amelie/Documents/Luise Masterarbeit/Daten/lowDim_trueATE.csv"  
# PATH_OVERVIEW <- "C:/Users/luise/Documents/Luise Masterarbeit/Daten/cont_dgp_overview.csv" 

# Uni-Computer
PATH_DATASETS <- "W:/Masterarbeit/Daten/"
PATH_TRUE_ATES <- "W:/Masterarbeit/Daten/trueATE/lowDim_trueATE.csv"
PATH_OVERVIEW <- "W:/Masterarbeit/Daten/trueATE/cont_dgp_overview.csv"
PATH_RESULTS <- "W:/Masterarbeit/Results/"

dgps <- read.csv(PATH_OVERVIEW)
all_datasets <- read.csv(PATH_TRUE_ATES)

set.seed(213)

df_tuning_results <- data.frame(
  dgp = numeric(),
  metric = character(),
  model = character(),
  iteration =  numeric(),
  min_node_size = numeric(),
  sample_fraction = numeric(),
  mtry = numeric(),
  value = numeric()
)


dgp_test <- c(2, 12)




for(i in c(5, 7)){
  dgp_index <- dgps$DGPid[i]
  true_ate <- dgps$trueATE[i]
  datasets_dgp <- all_datasets %>% filter(DGPid == dgp_index)
  
  n_cov <- dgps$n_covariates[i]
  
  #for (j in 1:nrow(datasets_dgp)){
  for (j in 1:20){
    hyper_cf <- expand.grid(
      min_node_size = c(5, 10, 20, 50),
      sample_fraction = c(0.3, 0.5),
      mtry = c(
        min(ceiling(sqrt(n_cov)) + 20, n_cov),
        ceiling(n_cov/2),
        ceiling(n_cov/3)
      )
    )
    
    
    for (l in 1:nrow(hyper_cf)){
    hyperparams <- hyper_cf[l,]
    
    file_name <- datasets_dgp[j, "filename"]
    data <- read.csv(paste0(PATH_DATASETS, file_name, ".csv"))
    y <- data$Y
    z <- data$A
    X <- data %>% select(starts_with("V"))
    
    metrics_run <- estimate_causal_forest(y, z, X, 
                                          min_node_size = hyperparams$min_node_size,
                                          sample_fraction = hyperparams$sample_fraction,
                                          mtry = hyperparams$mtry,
                                          true_ate)
    
    df_tuning_results <- rbind(df_tuning_results, data.frame(
      dgp = rep(paste0("dgp", dgp_index), 4),
      metric = c("ate_estimate", "ate_bias", "ci_length", "coverage"),
      model = rep("causal_forest", 4),
      iteration =  rep(j, 4),
      min_node_size = rep(hyperparams$min_node_size, 4),
      sample_fraction = rep(hyperparams$sample_fraction, 4),
      mtry = rep(hyperparams$mtry),
      value = metrics_run
    )
    )
    print(paste("dgp:", i, "iteration:", j, "Hyperparams iteration:", l))
  }
  }
  write.csv(df_tuning_results, paste0(PATH_RESULTS, "2026-04-24_tuning_NO-CV_dgp",dgp_index,"_causal-forest.csv"))
}


estimate_causal_forest <- function(y, z, X, true_ate, min_node_size, sample_fraction, mtry){
  X_mm <- model.matrix(~ . - 1, data = X)
  fit_causal_forest <- causal_forest(X_mm, y, z, min.node.size = min_node_size, sample.fraction = sample_fraction, mtry =  mtry)
  
  # testdata_mm <- model.matrix(~ . - 1, data = testdata)
  
  icate_predictions <- fit_causal_forest$predictions
  estimates <- average_treatment_effect(fit_causal_forest)
  ate_estimate <- estimates[1] %>% unname() # TODO benennen!
  se <- estimates[2] %>% unname()
  
  results_run <- get_metrics_not_bart(ate_estimate, se, true_ate)
  return(results_run)
}

get_metrics_not_bart <- function(ate_estimate, se_ate, ate_true){
  # Bias
  ate_bias <- ate_true - ate_estimate
  
  # Confidence intervals
  ci_lower <- ate_estimate - 1.96 * se_ate
  ci_upper <- ate_estimate + 1.96 * se_ate
  
  # CI length
  ci_length <- ci_upper - ci_lower
  
  # Coverage
  is_covered <- ifelse(ci_lower <= ate_true && ate_true <= ci_upper, 1, 0)
  
  # Save results
  metrics <- c(
    ate_estimate = ate_estimate,
    ate_bias = ate_bias,
    ci_length = ci_length,
    is_covered = is_covered
  )
  return(metrics)
}