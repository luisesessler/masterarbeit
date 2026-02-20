# When using with In Sample data, don't specify "test_data"
# When using different test data, provide argument "test_data"
bart_one_model <- function(y, z, X, results, true_ate, testdata = X){
  
  bart_fit <- bartc(response = y, treatment = z, confounders = X, # subset = train, 
                              method.rsp = "bart", method.trt = "none",
                              estimand = "ate", keepTrees = TRUE, p.scoreAsCovariate = FALSE)
  
  #----
  # In sample metrics
  # Predict each observation in treatment and in control group
  predictions_treated <- predict(bart_fit, testdata, type = "mu.1") 
  predictions_control <- predict(bart_fit, testdata, type = "mu.0") 
  predictions_ite <- predictions_treated - predictions_control
  results <- get_metrics(predictions_ite, "BART_one_model", results, true_ate)
}

bart_two_models <- function(y, z, X, results, true_ate, testdata = X){
  X_mm <- model.matrix(~ . - 1, data = X)
  data_mm <- data.frame(X_mm, y)

  treated <- data_mm[z == 1,]
  control <- data_mm[z == 0,]
  
  # TODO: das vlt auslagern, da auch für PS Matching genutzt wird
  formula_mm <- colnames(X_mm) %>% paste0(collapse = " + ")
  bart_formula <- as.formula(paste0("y ~ ", formula_mm))
  
  # bart2 muss genutzt werden, weil nur dort n.samples spezifizert werden kann
  # TODO: chains and samples erhöhen
  bart_fit_treated <- bart2(formula = bart_formula, data = treated, keepTrees = TRUE, combineChains = TRUE)
  bart_fit_control <- bart2(formula = bart_formula, data = control, keepTrees = TRUE, combineChains = TRUE)
  
  # In Sample prediction
  # Prediction for all units
  predictions_treated <- predict(bart_fit_treated, newdata = data_mm, type = "ppd") 
  predictions_control <- predict(bart_fit_control, newdata = data_mm, type = "ppd")
  
  # Calcuate ITEs
  predictions_ite <- predictions_treated - predictions_control
  
  results_train <- get_metrics(predictions_ite,  "BART_two_models", results, true_ate)
  
  # Out of Sample prediction
  # Prediction for all units
  # predictions_treated <- predict(bart_fit_treated, newdata = testdata, type = "ppd") 
  # predictions_control <- predict(bart_fit_control, newdata = testdata, type = "ppd")
  
  # Calcuate ITEs
  # predictions_ite <- predictions_treated - predictions_control
  
  results <- get_metrics(predictions_ite,  "BART_two_models", results, true_ate)
}


bart_ps <- function(y, z, X, results, true_ate, trt_method, testdata = X){
  model_name <- paste0("BART_ps_", trt_method)  
  
  bart_fit <- bartc(response = y, treatment = z, confounders = X,
                      method.rsp = "bart", method.trt = trt_method,
                      estimand = "ate", keepTrees = TRUE, p.scoreAsCovariate = TRUE) #TODO: warum steht da estimand = "ATE"
    
  predictions_treated <- predict(bart_fit, testdata, type = "mu.1") 
  predictions_control <- predict(bart_fit, testdata, type = "mu.0") 
  predictions_ite <- predictions_treated - predictions_control
  results_train <- get_metrics(predictions_ite, model_name, results, true_ate)
}

# Propensity Score Matching + Regression
matching_regression <- function(y, z, X, results, true_ate, testdata = X){
  # Nearest neighbor matching with logistic regression
  # TODO vlt nochmal anpassen
  match_model <- matchit(ps_formula,
                         data = data.frame(X, z, y),
                         method = "optimal",      
                         distance = "logit")
  
  # Compute standardized mean differences
  # smd_mean <- mean(smds$Balance$Diff.Adj)
  # smd_max <- max(smds$Balance$Diff.Adj)
  # smd_too_big <- sum(abs(smds$Balance$Diff.Adj) >= 0.1)
  
  matched_data <- match.data(match_model)
  
  fit_match_regression <- lm(outcome_formula, matched_data)
  adjusted_se_matching <- vcovCL(fit_match_regression, cluster = ~ subclass)
  
  coefs_match <- summary(fit_match_regression)$coef
  ate_estimate <- coefs_match["z", 1]
  se <- coefs_match["z", 2]
  
  results <- get_metrics_not_bart(results, "matching", ate_estimate, se, true_ate)
}

weighting_regression <-  function(y, z, X, results, true_ate, newdata = X){
  data <- data.frame(X, z, y)
  # Calculate Propensity scores
  # TODO gleich wie PS Matching?
  fit_ps_scores <- glm(ps_formula, data = data, family = binomial)
  ps_scores_prediction <- predict(fit_ps_scores, type = "response")
  weights <- ifelse(z == 1, 1/ps_scores_prediction, 1/(1-ps_scores_prediction))
  
  # Check balance
  # balance_measures <- bal.tab(ps_formula, 
                              #data = data, 
                              #weights = weights,  
                              #method = "weighting")
  # print(balance_measures)
  
  # Compute standardized mean differences
  # smd_mean_weighting <- mean(balance_measures$Balance$Diff.Adj)
  # smd_max_weighting <- max(balance_measures$Balance$Diff.Adj)
  # smd_too_big_weighting <- sum(abs(balance_measures$Balance$Diff.Adj) >= 0.1)
  
  # love.plot(bal.tab)
  
  fit_weighting <- lm(outcome_formula, data = data, weights = weights)
  # Adjust standard errors
  estimates_adjusted_se <- coeftest(fit_weighting, vcov = vcovHC(fit_weighting, type = "HC0")) 
  
  ate_weighting <- estimates_adjusted_se["z", 1]
  se_weighting <- estimates_adjusted_se["z", 2]
  
  results_train <- get_metrics_not_bart(results_train, "weighting", ate_weighting, se_weighting, ate_true = true_ate)
}

causal_forest <- function(y, z, X, results, true_ate){
  X_mm <- model.matrix(~ . - 1, data = X)
  fit_causal_forest <- causal_forest(X_mm, y, z)
  
  # testdata_mm <- model.matrix(~ . - 1, data = testdata)
  
  icate_predictions <- fit_causal_forest$predictions
  ate_estimate <- average_treatment_effect(fit_causal_forest)[1]
  se <- average_treatment_effect(fit_causal_forest)[2]

  results <- get_metrics_not_bart(results, "causal_forest", icate_predictions, ate_estimate, se, true_ate)
}



