bart_s_learner <- function(y, z, X, true_ate, testdata = X){
  
  bart_fit <- bartc(response = y, treatment = z, confounders = X, # subset = train, 
                    method.rsp = "bart", method.trt = "none",
                    estimand = "ate", keepTrees = TRUE, p.scoreAsCovariate = FALSE)
  
  #----
  # In sample metrics
  # Predict each observation in treatment and in control group
  predictions_treated <- predict(bart_fit, testdata, type = "mu.1") 
  predictions_control <- predict(bart_fit, testdata, type = "mu.0") 
  predictions_ite <- predictions_treated - predictions_control #TODO vlt CATE oder ICATE nennen?
  results_run <- get_metrics(predictions_ite, results, true_ate)
  return(results_run)
}

get_metrics <- function(ite_matrix,  results_list, true_ate){#, true_ites) {
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
  
  # Save results
  metrics <- data.frame(
    ate_estimate = ate_estimate,
    ate_bias = ate_bias,
    ci_length = ci_length,
    is_covered = is_covered
  )

  return(metrics)
}