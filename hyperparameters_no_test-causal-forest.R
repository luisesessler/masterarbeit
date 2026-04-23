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

hyper_cf <- expand.grid(
  k = c(1, 2, 3, 5),
  n_trees = c(75, 100, 200)
)


df_tuning_results <- data.frame(
  dgp = character(),
  metric = character(),
  model = character(),
  iteration = numeric(),
  k = numeric(),
  n_trees = numeric(),
  value = numeric()
)

dgp_test <- c(2, 12)


for(i in 2){
  dgp_index <- dgps$DGPid[i]
  true_ate <- dgps$trueATE[i]
  datasets_dgp <- all_datasets %>% filter(DGPid == dgp_index)
  
  #for (j in 1:nrow(datasets_dgp)){
  for (j in 19:20){
    for (l in 1:nrow(hyper_bart)){
    hyperparams <- hyper_bart[l,]
    
    file_name <- datasets_dgp[j, "filename"]
    data <- read.csv(paste0(PATH_DATASETS, file_name, ".csv"))
    y <- data$Y
    z <- data$A
    X <- data %>% select(starts_with("V"))
    
    metrics_run <- bart_s_learner(y, z, X, k = hyperparams$k, n_trees = hyperparams$n_trees, true_ate = true_ate,
                                  method_trt = "bart", ps_as_covariate = TRUE
                                  )
    
    df_tuning_results <- rbind(df_tuning_results, data.frame(
      dgp = rep(paste0("dgp", dgp_index), 4),
      metric = c("ate_estimate", "ate_bias", "ci_length", "coverage"),
      model = rep("causal_forest", 4),
      iteration =  rep(j, 4),
      k = rep(hyperparams$k, 4),
      n_trees = rep(hyperparams$n_trees, 4),
      value = metrics_run
    )
    )
    
    print(paste("dgp:", i, "iteration:", j, "k:", hyperparams$k, "n.trees:", hyperparams$n_trees))
  }
  }
  write.csv(df_tuning_results, paste0(PATH_RESULTS, "2026-04-21_tuning_NO-CV_dgp60_ps-bart.csv"))
}


estimate_causal_forest <- function(y, z, X, results, true_ate){
  X_mm <- model.matrix(~ . - 1, data = X)
  fit_causal_forest <- causal_forest(X_mm, y, z, tune.parameters = "all")
  
  # testdata_mm <- model.matrix(~ . - 1, data = testdata)
  
  icate_predictions <- fit_causal_forest$predictions
  estimates <- average_treatment_effect(fit_causal_forest)
  ate_estimate <- estimates[1] %>% unname() # TODO benennen!
  se <- estimates[2] %>% unname()
  
  results <- get_metrics_not_bart(results, "causal_forest", icate_predictions, ate_estimate, se, true_ate)
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