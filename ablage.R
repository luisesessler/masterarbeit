#Ablage

#In sample metrics
summary_bart_one_model <- summary(bart_fit_one_model)$estimate

# ATE Bias
ate_train_one_model <- summary_bart_one_model$estimate
ate_bias_train_one_model <- ate_train - ate_train_one_model 

# PEHE
pehe_train_one_model <- sqrt(mean((data_train$ite - fitted(bart_fit_one_model, type = "ite"))^2))

# CI length
ci_length_train_one_model <- summary_bart_one_model$ci.upper - summary_bart_one_model$ci.lower

# Coverage
ite_draws_train_one_model <- extract(bart_fit_one_model, type = "ite")
ite_cis_train_one_model <- t(apply(ite_draws_train_one_model, 2, quantile, probs = c(0.025, 0.975)))
is_covered_train_one_model <- ifelse(ite_cis_train_one_model[,1] <= data_train$ite &
                                       data_train$ite <= ite_cis_train_one_model[,2], 1, 0)
coverage_train_one_model <- sum(is_covered_train_one_model) / length(is_covered_train_one_model)

# Save results
pehe_results_train$BART_one_model[[length(pehe_results_train$BART_one_model) + 1]]  <- pehe_train_one_model
ate_bias_results_train$BART_one_model[[length(ate_bias_results_train$BART_one_model) + 1]]  <- ate_bias_train_one_model
ci_length_results_train$BART_one_model[[length(ci_length_results_train$BART_one_model) + 1]]  <- ci_length_train_one_model 
coverage_results_train$BART_one_model[[length(coverage_results_train$BART_one_model) + 1]]  <- coverage_train_one_model



#---- Out of sample metrics

# Mean ITE for each observation
ite_one_model <- colMeans(ite_matrix_one_model) 

# ATE Bias
ate_one_model <- mean(ite_one_model)
ate_bias_one_model <- ate_test - ate_one_model 

# PEHE
pehe_one_model <- sqrt(mean((data_test$ite - ite_one_model)^2))

# Credibility intervals
credible_intervals_one_model <- t(apply(ite_matrix_one_model, 2, quantile, probs = c(0.025, 0.975)))
colnames(credible_intervals_one_model) <- c("lower", "upper")

# Interval length
ci_length_one_model <- credible_intervals_one_model[,2] - credible_intervals_one_model[,1]
mean_ci_length_one_model <- mean(ci_length_one_model)

# Mean CI
# TODO: Braucht man das?
mean_ci_one_model <- colMeans(credible_intervals_one_model) 

# Coverage
is_covered_one_model <- ifelse(credible_intervals_one_model[,1] <= data_test$ite &
                                 data_test$ite <= credible_intervals_one_model[,2], 1, 0)
coverage_one_model <- sum(is_covered_one_model) / length(is_covered_one_model)

# Save results
pehe_results_test$BART_one_model[[length(results$BART_one_model) + 1]]  <- pehe_one_model
ate_bias_results_test$BART_one_model[[length(results$BART_one_model) + 1]]  <- ate_bias_one_model
ci_length_results_test$BART_one_model[[length(results$BART_one_model) + 1]]  <- mean_ci_length_one_model
coverage_results_test$BART_one_model[[length(results$BART_one_model) + 1]]  <- coverage_one_model





test_covariates <- data_test %>% select(starts_with("x_"))

prediction_treated <- predict(bart_fit_treated, newdata = test_covariates)
prediction_control <- predict(bart_fit_control, newdata = test_covariates)

pred_means_treated <- colMeans(prediction_treated)
pred_means_control <- colMeans(prediction_control)

ite_two_models <- pred_means_treated - pred_means_control
ate_two_models <- mean(ite_two_models)

pehe_two_models <- sqrt(mean((data_test$ite - ite_two_models)^2))

results$BART_two_models[[length(results$BART_two_models) + 1]]  <- pehe_two_models