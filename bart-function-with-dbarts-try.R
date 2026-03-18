# Ausprobieren: Die bart S-learner + zwei PS Models mit dbart implementieren statt bin cforest,
# damit Vergleichbarkeit wirklich gegeben ist und Hyperparameter gleich getuned werden können

bart_one_model_new <- function(y, z, X, results, true_ate, testdata = X){
  X_mm <- model.matrix(~ . - 1, data = X)
  data_mm <- data.frame(X_mm, z, y)
  
  # TODO: das vlt auslagern, da auch für PS Matching genutzt wird
  formula_mm <- colnames(X_mm) %>% paste0(collapse = " + ")
  bart_formula <- as.formula(paste0("y ~ z + ", formula_mm))
  
  bart_fit <- bart2(formula = bart_formula, data = data_mm, keepTrees = TRUE, combineChains = TRUE,
                    n.trees = 200, n.burn = 1000, n.samples = 2000, n.chains = 4)

  data_mm_treated <- data.frame(X_mm, z = 1)
  data_mm_control <- data.frame(X_mm, z = 0)
  
  predictions_treated <- predict(bart_fit, newdata = data_mm_treated, type = "ev") 
  predictions_control <- predict(bart_fit, newdata = data_mm_control, type = "ev")
  
  #----
  # In sample metrics
  # Predict each observation in treatment and in control group

  predictions_ite <- predictions_treated - predictions_control
  results <- get_metrics(predictions_ite, "BART_one_model", results, true_ate)
  return(results)
}

bart_ps_bart_new <- function(y, z, X, results, true_ate, testdata = X){
  X_mm <- model.matrix(~ . - 1, data = X)
  data_ps_mm <- data.frame(X_mm, z)
  
  formula_mm <- colnames(X_mm) %>% paste0(collapse = " + ")
  ps_formula <- as.formula(paste0("z ~ ", formula_mm))
  
  ps_fit <- bart2(formula = ps_formula, data = data_ps_mm, family = "binomial", keepTrees = TRUE, combineChains = TRUE)
  ps <- predict(ps_fit, data_ps_mm, type = "ev") %>% colMeans()
  
  data_mm <- data.frame(X_mm, ps, z, y)
  
  # TODO: das vlt auslagern, da auch für PS Matching genutzt wird
  bart_formula <- as.formula(paste0("y ~ z + ps + ", formula_mm))
  
  bart_fit <- bart2(formula = bart_formula, data = data_mm, keepTrees = TRUE, combineChains = TRUE)
  
  data_mm_treated <- data.frame(X_mm, ps = ps, z = 1)
  data_mm_control <- data.frame(X_mm, ps = ps, z = 0)
  
  predictions_treated <- predict(bart_fit, newdata = data_mm_treated, type = "ev") 
  predictions_control <- predict(bart_fit, newdata = data_mm_control, type = "ev")
  
  #----
  # In sample metrics
  # Predict each observation in treatment and in control group
  
  predictions_ite <- predictions_treated - predictions_control
  results <- get_metrics(predictions_ite, "BART_ps_bart", results, true_ate)
  return(results)
}

bart_ps_ps_new <- function(y, z, X, results, true_ate, testdata = X){
  X_mm <- model.matrix(~ . - 1, data = X)
  data_ps_mm <- data.frame(X_mm, z)
  
  formula_mm <- colnames(X_mm) %>% paste0(collapse = " + ")
  ps_formula <- as.formula(paste0("z ~ ", formula_mm))
  
  ps_fit <- bart2(formula = ps_formula, data = data_ps_mm, family = "binomial", keepTrees = TRUE, combineChains = TRUE)
  ps <- predict(ps_fit, data_ps_mm, type = "ppd") %>% colMeans()
  
  data_mm <- data.frame(X_mm, ps, z, y)
  
  # TODO: das vlt auslagern, da auch für PS Matching genutzt wird
  bart_formula <- as.formula(paste0("y ~ z + ps + ", formula_mm))
  
  bart_fit <- bart2(formula = bart_formula, data = data_mm, keepTrees = TRUE, combineChains = TRUE)
  
  data_mm_treated <- data.frame(X_mm, ps = ps, z = 1)
  data_mm_control <- data.frame(X_mm, ps = ps, z = 0)
  
  predictions_treated <- predict(bart_fit, newdata = data_mm_treated, type = "ppd") 
  predictions_control <- predict(bart_fit, newdata = data_mm_control, type = "ppd")
  
  #----
  # In sample metrics
  # Predict each observation in treatment and in control group
  
  predictions_ite <- predictions_treated - predictions_control
  results <- get_metrics(predictions_ite, "BART_ps_bart", results, true_ate)
  return(results)
}