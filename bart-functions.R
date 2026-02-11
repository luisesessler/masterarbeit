bart_one_model <- function(y, z, X, results_train, results_test, true_ate, newData = X){
  
  bart_fit <- bartc(response = y, treatment = z, confounders = X, # subset = train, 
                              method.rsp = "bart", method.trt = "none",
                              estimand = "ate", keepTrees = TRUE, p.scoreAsCovariate = FALSE)
  
  #----
  # In sample metrics
  # Predict each observation in treatment and in control group
  # TODO In Sample und Out of Sample neue Namen geben
  predictions_treated <- predict(bart_fit, newdata = X, type = "y.1") 
  predictions_control <- predict(bart_fit, newdata = X, type = "y.0") 
  predictions_ite <- predictions_treated - predictions_control
  results_train <- get_metrics(predictions_ite, "BART_one_model", results_train, true_ate)
  
  #---- 
  # Out of sample predictions
  predictions_treated <- predict(bart_fit, newdata = newData, type = "y.1") 
  predictions_control <- predict(bart_fit, newdata = newData, type = "y.0") 
  predictions_ite <- predictions_treated - predictions_control
  results_test <- get_metrics(predictions_ite, "BART_one_model", results_test, true_ate)
}

bart_two_models <- function(y, z, X, newData, true_ate){
  X_mm <- model.matrix(~ . - 1, data = X)
  data_mm <- data.frame(X_mm, y)

  treated <- data_mm[z == 1,]
  control <- data_mm[z == 1,]
  
  # TODO: das vlt auslagern, da auch für PS Matching genutzt wird
  formula_rightside <- colnames(X_mm) %>% paste0(collapse = " + ")
  bart_formula <- as.formula(paste0("y ~ ", formula_rightside))
  
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
  
  results_train <- get_metrics(predictions_ite,  "BART_two_models", results_train, true_ate)
  
  # Out of Sample prediction
  # Prediction for all units
  predictions_treated <- predict(bart_fit_treated, newdata = newData, type = "ppd") 
  predictions_control <- predict(bart_fit_control, newdata = newData, type = "ppd")
  
  # Calcuate ITEs
  predictions_ite <- predictions_treated - predictions_control
  
  results_test <- get_metrics(predictions_ite,  "BART_two_models", results_test, true_ate)
}

bart_ps <- function(y, z, X, results_train, results_test, true_ate, ps_method, newData = X){
  model_name <- paste0("BART_ps_", ps_method)  
  
  bart_fit <- bartc(response = y, treatment = z, confounders = X,
                      method.rsp = "bart", method.trt = trt_method,
                      estimand = "ate", keepTrees = TRUE, p.scoreAsCovariate = TRUE)
    
    #----
    # In sample metrics
    # Predict each observation in treatment and in control group
    # TODO In Sample und Out of Sample neue Namen geben
    predictions_treated <- predict(bart_fit, newdata = X, type = "y.1") 
    predictions_control <- predict(bart_fit, newdata = X, type = "y.0") 
    predictions_ite <- predictions_treated - predictions_control
    results_train <- get_metrics(predictions_ite, model_name, results_train, true_ate)
    
    #---- 
    # Out of sample predictions
    predictions_treated <- predict(bart_fit, newdata = newData, type = "y.1") 
    predictions_control <- predict(bart_fit, newdata = newData, type = "y.0") 
    predictions_ite <- predictions_treated - predictions_control
    results_test <- get_metrics(predictions_ite, model_name, results_test, true_ate)
}

matching_regression <- function(y, z, X, true_ate){
  # Propensity Score Matching + Regression
  formula_rightside <- colnames(X) %>% paste0(collapse = " + ")
  ps_formula <- as.formula(paste0("z ~ ", formula_rightside))
  
  # Nearest neighbor matching with logistic regression
  match_model <- matchit(ps_formula,
                         data = data.frame(X, z, y),
                         method = "optimal",      
                         distance = "logit")
  
  # Compute standardized mean differences
  # smd_mean <- mean(smds$Balance$Diff.Adj)
  # smd_max <- max(smds$Balance$Diff.Adj)
  # smd_too_big <- sum(abs(smds$Balance$Diff.Adj) >= 0.1)
  
  matched_data <- match.data(match_model)
  
  match_regression_formula <- as.formula(paste0("y ~ z + ", formula_rightside))
  
  fit_match_regression <- lm(match_regression_formula, matched_data)
  adjusted_se_matching <- vcovCL(fit_match_regression, cluster = ~ subclass)
  
  coefs_match <- summary(fit_match_regression)$coef
  ate_estimate <- coefs_match["z", 1]
  se <- coefs_match["z", 2]
  
  results_train <- get_metrics_not_bart(results_train, "matching", ate_estimate, se, true_ate)
}


