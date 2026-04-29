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

sigma_params <- list(c(3, 0.90),c(3, 0.99), c(10, 0.75)) 
k_vals <- c(1, 2, 3, 5)
n_trees_vals <- c(75, 100, 200)

hyper_bart <- expand.grid(idx = 1:3, k = k_vals, n_trees = n_trees_vals)

hyper_bart$nu <- sapply(hyper_bart$idx, function(i) sigma_params[[i]][1])
hyper_bart$q  <- sapply(hyper_bart$idx, function(i) sigma_params[[i]][2])

hyper_bart$idx <- NULL

df_tuning_results <- data.frame(
  dgp = character(),
  metric = character(),
  model = character(),
  iteration = numeric(),
  k = numeric(),
  n_trees = numeric(),
  nu = numeric(),
  q = numeric(),
  value = numeric()
)

dgp_test <- c(12)

for(i in 8){
  dgp_index <- dgps$DGPid[i]
  true_ate <- dgps$trueATE[i]
  datasets_dgp <- all_datasets %>% filter(DGPid == dgp_index)
  
  #for (j in 1:nrow(datasets_dgp)){
  for (j in 1:20){
    for (l in 1:nrow(hyper_bart)){
      hyperparams <- hyper_bart[l,]
      
      file_name <- datasets_dgp[j, "filename"]
      data <- read.csv(paste0(PATH_DATASETS, file_name, ".csv"))
      y <- data$Y
      z <- data$A
      X <- data %>% select(starts_with("V"))
      
      metrics_run <- bart_s_learner(y, z, X, ps_as_covariate = TRUE, method_trt = "glm",
                                    k = hyperparams$k, n_trees = hyperparams$n_trees, 
                                    nu = hyperparams$nu, q = hyperparams$q,
                                    true_ate = true_ate)
      
      df_tuning_results <- rbind(df_tuning_results, data.frame(
        dgp = rep(paste0("dgp", dgp_index), 4),
        metric = c("ate_estimate", "ate_bias", "ci_length", "coverage"),
        model = rep("bart_ps-glm", 4),
        iteration =  rep(j, 4),
        k = rep(hyperparams$k, 4),
        n_trees = rep(hyperparams$n_trees, 4),
        nu =  rep(hyperparams$nu, 4),
        q =  rep(hyperparams$q, 4),
        value = metrics_run
      )
      )
      
      print(paste("dgp:", i, "iteration:", j, "hyperparams:", l))
    }
  }
  write.csv(df_tuning_results, paste0(PATH_RESULTS, "2026-04-29_tuning_NO-CV_dgp40_ps-glm.csv"))
}

bart_s_learner <- function(y, z, X, true_ate, method_trt = "none", ps_as_covariate = FALSE, 
                           k, n_trees, nu, q, testdata = X){
  bart_fit <- bartc(response = y, treatment = z, confounders = X,
                    method.rsp = "bart", method.trt = method_trt, 
                    estimand = "ate", keepTrees = TRUE, 
                    p.scoreAsCovariate = ps_as_covariate,
                    n.trees = n_trees, k = k,
                    sigdf = nu, sigquant = q)
  
  #----
  # In sample metrics
  # Predict each observation in treatment and in control group
  predictions_treated <- predict(bart_fit, testdata, type = "mu.1") 
  predictions_control <- predict(bart_fit, testdata, type = "mu.0") 
  predictions_ite <- predictions_treated - predictions_control #TODO vlt CATE oder ICATE nennen?
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