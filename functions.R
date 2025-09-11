#In sample metrics

get_train_metrics <- function(bart_model, model_name, results_lists, true_ate, true_ites) {
  
  summary_bart <- summary(bart_model)$estimate
  
  # ATE Bias
  ate_train <- summary_bart$estimate
  ate_bias_train <- true_ate - ate_train 
  
  # PEHE
  pehe_train <- sqrt(mean((true_ites - fitted(bart_model, type = "ite"))^2))
  
  # CI length
  ci_length_train <- summary_bart$ci.upper - summary_bart$ci.lower
  
  # Coverage
  ite_draws_train <- extract(bart_model, type = "ite")
  ite_cis_train <- t(apply(ite_draws_train, 2, quantile, probs = c(0.025, 0.975)))
  is_covered_train <- ifelse(ite_cis_train[,1] <= true_ites &
                                         true_ites <= ite_cis_train[,2], 1, 0)
  coverage_train <- sum(is_covered_train) / length(is_covered_train)
  
  # Save results
  results_lists$pehe_results_train[[model_name]][[length(results_lists$pehe_results_train[[model_name]]) + 1]]  <- pehe_train
  results_lists$ate_bias_results_train[[model_name]][[length(results_lists$ate_bias_results_train[[model_name]]) + 1]]  <- ate_bias_train
  results_lists$ci_length_results_train[[model_name]][[length(results_lists$ci_length_results_train[[model_name]]) + 1]]  <- ci_length_train 
  results_lists$coverage_results_train[[model_name]][[length(results_lists$coverage_results_train[[model_name]]) + 1]]  <- coverage_train

  return(results_lists)
  
}



get_test_metrics <- function(ite_matrix, model_name, results_lists, true_ate, true_ites) {
  # Mean ITE for each observation
  ite_test <- colMeans(ite_matrix) 
  
  # ATE Bias
  ate_test <- mean(ite_test)
  ate_bias_test <- true_ate - ate_test 
  
  # PEHE
  pehe_test <- sqrt(mean((true_ites - ite_test)^2))
  
  # Credibility intervals
  credible_intervals_test <- t(apply(ite_matrix, 2, quantile, probs = c(0.025, 0.975)))
  colnames(credible_intervals_test) <- c("lower", "upper")
  
  # Interval length
  ci_length_test <- credible_intervals_test[,2] - credible_intervals_test[,1]
  mean_ci_length_test <- mean(ci_length_test)
  
  # Coverage
  is_covered_test <- ifelse(credible_intervals_test[,1] <= true_ites &
                                   true_ites <= credible_intervals_test[,2], 1, 0)
  coverage_test <- sum(is_covered_test) / length(is_covered_test)
  
  # Save results
  results_lists$pehe_results_test[[model_name]][[length(results_lists$pehe_results_test[[model_name]]) + 1]]  <- pehe_test
  results_lists$ate_bias_results_test[[model_name]][[length(results_lists$ate_bias_results_test[[model_name]]) + 1]]  <- ate_bias_test
  results_lists$ci_length_results_test[[model_name]][[length(results_lists$ci_length_results_test[[model_name]]) + 1]]  <- mean_ci_length_test 
  results_lists$coverage_results_test[[model_name]][[length(results_lists$coverage_results_test[[model_name]]) + 1]]  <- coverage_test
  
  return(results_lists)
  
}