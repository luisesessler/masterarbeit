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

hyper_bart <- expand.grid(
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

dgp_test <- c(12)


for(i in 2){
  dgp_index <- dgps$DGPid[i]
  true_ate <- dgps$trueATE[i]
  datasets_dgp <- all_datasets %>% filter(DGPid == dgp_index)
  
  #for (j in 1:nrow(datasets_dgp)){
  for (j in 20){
    for (l in 1:nrow(hyper_bart)){
    hyperparams <- hyper_bart[l,]
    
    file_name <- datasets_dgp[j, "filename"]
    data <- read.csv(paste0(PATH_DATASETS, file_name, ".csv"))
    y <- data$Y
    z <- data$A
    X <- data %>% select(starts_with("V"))
    
    metrics_run <- bart_t_learner(y, z, X, k = hyperparams$k, n_trees = hyperparams$n_trees, true_ate = true_ate)
                                  
    df_tuning_results <- rbind(df_tuning_results, data.frame(
      dgp = rep(paste0("dgp", dgp_index), 4),
      metric = c("ate_estimate", "ate_bias", "ci_length", "coverage"),
      model = rep("bart_t-learner", 4),
      iteration =  rep(j, 4),
      k = rep(hyperparams$k, 4),
      n_trees = rep(hyperparams$n_trees, 4),
      value = metrics_run
    )
    )
    
    print(paste("dgp:", i, "iteration:", j, "k:", hyperparams$k, "n.trees:", hyperparams$n_trees))
  }
  }
  write.csv(df_tuning_results, paste0(PATH_RESULTS, "2026-04-23_tuning_NO-CV_dgp30_t-learner_20.csv"))
}


bart_s_learner <- function(y, z, X, true_ate, method_trt = "none", ps_as_covariate = FALSE, k, n_trees, testdata = X){
  bart_fit <- bartc(response = y, treatment = z, confounders = X,
                    method.rsp = "bart", method.trt = method_trt, 
                    estimand = "ate", keepTrees = TRUE, 
                    p.scoreAsCovariate = ps_as_covariate,
                    n.trees = n_trees, k = k)
  
  #----
  # In sample metrics
  # Predict each observation in treatment and in control group
  predictions_treated <- predict(bart_fit, testdata, type = "mu.1") 
  predictions_control <- predict(bart_fit, testdata, type = "mu.0") 
  predictions_ite <- predictions_treated - predictions_control #TODO vlt CATE oder ICATE nennen?
  results_run <- get_metrics(predictions_ite, true_ate)
  return(results_run)
}

bart_t_learner <- function(y, z, X, true_ate, n_trees, k, testdata = X){
  X_mm <- model.matrix(~ . - 1, data = X)
  X_df <- data.frame(X_mm)
  data_mm <- data.frame(X_mm, y)
  
  treated <- data_mm[z == 1,]
  control <- data_mm[z == 0,]
  
  # TODO: das vlt auslagern, da auch für PS Matching genutzt wird
  formula_mm <- colnames(X_mm) %>% paste0(collapse = " + ")
  bart_formula <- as.formula(paste0("y ~ ", formula_mm))
  
  # bart2 muss genutzt werden, weil nur dort n.samples spezifizert werden kann
  # TODO: chains and samples erhöhen
  bart_fit_treated <- bart2(formula = bart_formula, data = treated, n.trees = n_trees, k = k, keepTrees = TRUE, combineChains = TRUE, n.chains = 10)
  bart_fit_control <- bart2(formula = bart_formula, data = control, n.trees = n_trees, k = k, keepTrees = TRUE, combineChains = TRUE, n.chains = 10)
  
  # In Sample prediction
  # Prediction for all units
  predictions_treated <- predict(bart_fit_treated, newdata = X_df, type = "ev") 
  predictions_control <- predict(bart_fit_control, newdata = X_df, type = "ev")
  
  # Calcuate ITEs
  predictions_ite <- predictions_treated - predictions_control
  
  results_run <- get_metrics(predictions_ite, true_ate)
  return(results_run)
}


get_metrics <- function(ite_matrix, true_ate){ 
  # Mean ITE for each observation
  ite_estimates <- colMeans(ite_matrix) 
  ate_draws <- rowMeans(ite_matrix)
  
  # ATE Bias
  ate_estimate <- mean(ite_estimates)
  ate_bias <- ate_estimate - true_ate
  
  # PEHE
  # pehe_test <- sqrt(mean((true_ites - ite_prediction)^2))
  
  # Credibility interval
  credible_interval <- quantile(ate_draws, probs = c(0.025, 0.975))
  
  # Interval length
  ci_length <- unname(credible_interval[2] - credible_interval[1])
  
  # Coverage
  is_covered <- ifelse(credible_interval[1] <= true_ate &
                         true_ate <= credible_interval[2], 1, 0) %>% unname()
  
  
  metrics <- c(
    ate_estimate = ate_estimate,
    ate_bias = ate_bias,
    ci_length = ci_length,
    is_covered = is_covered
  )
  return(metrics)
}