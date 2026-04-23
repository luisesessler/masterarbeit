# Mein Computer
# PATH_DATASETS <- "C:/Users/luise/Documents/Masterarbeit/Daten/"  
# PATH_TRUE_ATES <- "C:/Users/luise/Documents/Masterarbeit/Daten/lowDim_trueATE.csv"  
# PATH_OVERVIEW <- "C:/Users/luise/Documents/Masterarbeit/Daten/dgp_overview.csv" 

# Uni-Computer
PATH_DATASETS <- "W:/Masterarbeit/Daten/"  
PATH_TRUE_ATES <- "W:/Masterarbeit/Daten/trueATE/lowDim_trueATE.csv"  
PATH_OVERVIEW <- "W:/Masterarbeit/Daten/trueATE/dgp_overview.csv" 
PATH_RESULTS <- "W:/Masterarbeit/Results/" 

dgps <- read.csv(PATH_OVERVIEW)
all_datasets <- read.csv(PATH_TRUE_ATES)

set.seed(213)

df_tuning_results <- data.frame(
  dgp = character(),
  metric = character(),
  model = character(),
  iteration = numeric(),
  ps_model = character(),
  regression_model = character(),
  value = numeric()
)



#for(i in 1:length(cont_dgpis)){
  for(i in 11:16){
  dgp_index <- dgps$DGPid[i]
  true_ate <- dgps$trueATE[i]
  
  datasets_dgp <- all_datasets %>% filter(DGPid == dgp_index)
  
  #for (j in 1:nrow(datasets_dgp)){
  for (j in 1:20){
    file_name <- datasets_dgp[j, "filename"]
    data <- read.csv(paste0(PATH_DATASETS, file_name, ".csv"))
    y <- data$Y
    z <- data$A
    X <- data %>% select(starts_with("V"))
    
    metrics_run <- weighting_regression_bart(y, z, X, results, true_ate)
    df_tuning_results <- rbind(df_tuning_results, data.frame(
      dgp = rep(paste0("dgp", dgp_index), 4),
      metric = c("ate_estimate", "ate_bias", "ci_length", "coverage"),
      model = rep("bart_ps-bart", 4),
      iteration =  rep(j, 4),
      ps_model = rep("bart", 4),
      regression_model = rep("linear_basic", 4),
      value = metrics_run
    )
    )
    print(paste("DGP: ", i, "Iteration:", j))
  }
  
  write.csv(df_tuning_results, paste0(PATH_RESULTS, "2026-04-23_weighting_bart", dgp_index,".csv"))
}

weighting_regression_bart <-  function(y, z, X, results, true_ate, newdata = X){
  data <- data.frame(X, z, y)

  # Calculate Propensity scores
  X_mm <- model.matrix(~ . - 1, data = X)
  data_ps_mm <- data.frame(X_mm, z)
  
  formula_mm <- colnames(X_mm) %>% paste0(collapse = " + ")
  ps_formula <- as.formula(paste0("z ~ ", formula_mm))
  outcome_formula <- as.formula(paste0("y ~ z + ", formula_mm))
  
  ps_fit <- bart2(formula = ps_formula, data = data_ps_mm, keepTrees = TRUE, combineChains = TRUE, n.chains = 10)
  ps <- predict(ps_fit, data_ps_mm, type = "ev") %>% colMeans()
  
  # Stabilized ATE weights
  p_treat <- mean(z)
  weights <- ifelse(z == 1,
                    p_treat / ps,
                    (1 - p_treat) / (1 - ps))
  
  
  fit_weighting <- lm(outcome_formula, data = data, weights = weights)
  # Adjust standard errors
  est_robust_se <- coeftest(fit_weighting,
                            vcov = vcovHC(fit_weighting, type = "HC0"))
  
  ate_est <- est_robust_se["z", 1]
  se_ate  <- est_robust_se["z", 2]
  
  results <- get_metrics_not_bart(ate_est, se_ate, ate_true = true_ate)
  return(results)
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