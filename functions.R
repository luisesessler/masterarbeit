# Creates a list to save all the result metrics
make_results_list <- function() {
  list_names <- c(
    "BART_one_model", "BART_two_models", "BART_ps_BART",
    "BART_ps_glm", "causal_forest", "matching", "weighting"
  )
  
  metrics <- c("pehe", "ate_bias", "ci_length", "coverage")
  
  make_empty_metric <- function() {
    setNames(
      replicate(length(list_names), list(), simplify = FALSE),
      list_names
    )
  }
  
  setNames(
    lapply(metrics, \(x) make_empty_metric()),
    metrics
  )
}

#In sample metrics

get_train_metrics <- function(bart_model, model_name, results_lists, ate_true){ #, ites_true) {
  
  summary_bart <- summary(bart_model)$estimate
  
  # ATE Bias
  ate_estimate <- summary_bart$estimate
  ate_bias <- ate_true - ate_estimate 
  
  # PEHE
  # pehe_train <- sqrt(mean((ites_true - fitted(bart_model, type = "ite"))^2))
  
  # CI length
  ci_length <- summary_bart$ci.upper - summary_bart$ci.lower
  
  # Coverage
  is_covered <- ifelse(summary_bart$ci.upper <= ate_true &
                         ate_true <= summary_bart$ci.lower, 1, 0)
  # ite_draws_train <- extract(bart_model, type = "ite")
  # ite_cis_train <- t(apply(ite_draws_train, 2, quantile, probs = c(0.025, 0.975)))
  # is_covered_train <- ifelse(ite_cis_train[,1] <= true_ites &
  #                                        true_ites <= ite_cis_train[,2], 1, 0)
  # coverage_train <- sum(is_covered_train) / length(is_covered_train)
  
  # Save results
  # results_lists$pehe_results_train[[model_name]][[length(results_lists$pehe_results_train[[model_name]]) + 1]]  <- pehe_train
  results_lists$ate_bias_results_train[[model_name]][[length(results_lists$ate_bias_results_train[[model_name]]) + 1]]  <- ate_bias
  results_lists$ci_length_results_train[[model_name]][[length(results_lists$ci_length_results_train[[model_name]]) + 1]]  <- ci_length 
  results_lists$coverage_results_train[[model_name]][[length(results_lists$coverage_results_train[[model_name]]) + 1]]  <- is_covered

  return(results_lists)
}



get_test_metrics <- function(ite_matrix, model_name, results_lists, true_ate, true_ites) {
  # Mean ITE for each observation
  ite_estimates <- colMeans(ite_matrix) 
  
  ate_draws <- rowMeans(ite_matrix)
  
  # ATE Bias
  ate_estimate <- mean(ite_estimates)
  ate_bias <- true_ate - ate_estimate
  
  # PEHE
  pehe_test <- sqrt(mean((true_ites - ite_prediction)^2))
  
  # Credibility intervals
  credible_intervals <- ate_estimate + sd_estimate * 1.96 # This is what the function "summary" uses for in sample estimates, so I used it as well instead of using quantile()
  colnames(credible_intervals_test) <- c("lower", "upper")
  
  # Interval length
  ci_length_test <- credible_intervals_test[,2] - credible_intervals_test[,1]
  mean_ci_length_test <- mean(ci_length_test)
  
  # Coverage
  is_covered <- ifelse(credible_intervals_test[,1] <= true_ate &
                                                       true_ate <= credible_intervals_test[,2], 1, 0)
  # TODO: löschen, das misst die Coverage für die ITEs einer Iteration
  #is_covered_test <- ifelse(credible_intervals_test[,1] <= true_ites &
  #                                 true_ites <= credible_intervals_test[,2], 1, 0)
  #coverage_test <- sum(is_covered_test) / length(is_covered_test)
  
  # Save results
  results_lists$pehe_results_test[[model_name]][[length(results_lists$pehe_results_test[[model_name]]) + 1]]  <- pehe_test
  results_lists$ate_bias_results_test[[model_name]][[length(results_lists$ate_bias_results_test[[model_name]]) + 1]]  <- ate_bias_test
  results_lists$ci_length_results_test[[model_name]][[length(results_lists$ci_length_results_test[[model_name]]) + 1]]  <- mean_ci_length_test 
  results_lists$coverage_results_test[[model_name]][[length(results_lists$coverage_results_test[[model_name]]) + 1]]  <- coverage_test
  
  return(results_lists)
}

get_metrics <- function(ite_matrix, model_name, results_list, true_ate){#, true_ites) {
  # Mean ITE for each observation
  ite_estimates <- colMeans(ite_matrix) 
  ate_draws <- rowMeans(ite_matrix)
  
  # ATE Bias
  ate_estimate <- mean(ite_estimates)
  ate_bias <- true_ate - ate_estimate
  
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
  results_list$ate_bias[[model_name]] <- c(results_list$ate_bias[[model_name]], ate_bias)
  results_list$ci_length[[model_name]] <- c(results_list$ci_length[[model_name]], ci_length)
  results_list$coverage[[model_name]] <- c(results_list$coverage[[model_name]], is_covered)
  
  return(results_list)
}

get_metrics_not_bart <- function(results_list, model_name, ate_estimate, se_ate, ate_true){
  # PEHE
  # pehe_cf_test <- sqrt(mean((data_test$ite - ite_cf_test)^2))
  
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
  results_list$ate_bias[[model_name]] <- c(results_list$ate_bias[[model_name]], ate_bias)
  results_list$ci_length[[model_name]] <- c(results_list$ci_length[[model_name]], ci_length)
  results_list$coverage[[model_name]] <- c(results_list$coverage[[model_name]], is_covered)
  return(results_list)
}