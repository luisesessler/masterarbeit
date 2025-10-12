library(bartCause) # Causal BART
library(dbarts) # General BART
library(dplyr) 
library(MatchIt) # Propensity Score Matching
library(grf) # Causal Forests
library(bcf) # Bayesian Causal Forests
library(survey) # Weighting
library(cobalt) # Balance diagnostics
library(sandwich) # Adjust standard errors
library(lmtest)

# TODO Speichern in List über index aus Iteration
n_iter <- 1

# ---- Set up Ergebnisse speichern ----
list_names <- c("BART_one_model", "BART_two_models", "BART_ps_BART", "BART_ps_glm", "causal_forest", "matching", "weighting")

n_metrics <- length(list_names)
template <- list()

pehe_results_train <- replicate(n_metrics, template, simplify = FALSE)
ate_bias_results_train <- replicate(n_metrics, template, simplify = FALSE)
ci_length_results_train <- replicate(n_metrics, template, simplify = FALSE)
coverage_results_train <- replicate(n_metrics, template, simplify = FALSE)

pehe_results_test <- replicate(n_metrics, template, simplify = FALSE)
ate_bias_results_test <- replicate(n_metrics, template, simplify = FALSE)
ci_length_results_test <- replicate(n_metrics, template, simplify = FALSE)
coverage_results_test <- replicate(n_metrics, template, simplify = FALSE)

names(pehe_results_train) <- list_names
names(ate_bias_results_train) <- list_names
names(ci_length_results_train) <- list_names
names(coverage_results_train) <- list_names

names(pehe_results_test) <- list_names
names(ate_bias_results_test) <- list_names
names(ci_length_results_test) <- list_names
names(coverage_results_test) <- list_names

results_train<- list(pehe_results_train = pehe_results_train, 
                     ate_bias_results_train = ate_bias_results_train, 
                     ci_length_results_train = ci_length_results_train, 
                     coverage_results_train = coverage_results_train)
results_test <- list(pehe_results_test = pehe_results_test, 
                     ate_bias_results_test = ate_bias_results_test, 
                     ci_length_results_test = ci_length_results_test, 
                     coverage_results_test = coverage_results_test)

# ---- Daten laden und vorbereiten ----
# ACIC 2016 datafiles

X <- read.csv("C:\\Users\\luise\\Documents\\Masterarbeit\\data-ACIC-2016\\data\\x.csv")
counterfactuals <- read.csv("C:\\Users\\luise\\Documents\\Masterarbeit\\data-ACIC-2016\\data_cf_all\\1\\zymu_336720322.csv")
z <- counterfactuals$z
y1 <- counterfactuals$y1
y0 <- counterfactuals$y0
y <- ifelse(z == 1, y1, y0)
ite <- y1 - y0

# Overall ATE
ate_complete <- mean(y1 - y0)


# merge
X[] <- lapply(X, function(x) if(is.character(x)) factor(x) else x)
# z <- as.factor(z)
df <- data.frame(X, z, y0, y1, y, ite)

# How many treated & non-treated?
perc_treated <- nrow(df[z == 1, ])/nrow(df)
perc_control <- nrow(df[z == 0, ])/nrow(df)


#TODO: Split training/test
percentage_train <- 0.7
train <- sample(nrow(df), percentage_train * nrow(df), replace = FALSE)

data_train <- df[train,]
data_test <- df[-train,]


# ATEs
ate_train <- mean(y1[train] - y0[train])
ate_test <- mean(y1[-train] - y0[-train])

#--------------------------------------------------------------
#--------------------------------------------------------------
# Propensity Scores (using BART)
ps_formula_rightside <- paste0("x_", 1:58, collapse = " + ")
ps_formula <- paste0("z ~ ", formula_rightside) %>% as.formula

p_scores_BART <- bart2(formula = ps_formula, data = data_train, keepTrees = TRUE, combineChains = TRUE)

ps_hat <- colMeans(p_scores_BART$yhat.train)

#--------------------------------------------------------------
#--------------------------------------------------------------
# Fit models
#--------------------------------------------------------------
#BART, one model, no PS in X

#---- Fit model
bart_fit_one_model <- bartc(response = y, treatment = z, confounders = X, subset = train, method.rsp = "bart", method.trt = "none",
                             estimand = "ate", keepTrees = TRUE, p.scoreAsCovariate = FALSE)


#----
# In sample metrics
results_train <- get_train_metrics(bart_fit_one_model, "BART_one_model", results_train, ate_train, data_train$ite)

#---- 
# Out of sample predictions
# For each observation in train, put them in both treatment and control  
z_0 <- rep(0, nrow(data_test))
z_1 <- rep(1, nrow(data_test))

# Predict each observation in treatment and in control group
prediction_one_model_treated <-  predict(bart_fit_one_model, newdata = data.frame(X[-train,], z = z_1), type = "mu")
prediction_one_model_control <-  predict(bart_fit_one_model, newdata = data.frame(X[-train,], z = z_0), type = "mu")

#---- Out of sample metrics
# Calculate ITEs for all iterations
ite_matrix_one_model <- prediction_one_model_treated - prediction_one_model_control

results_test <- get_test_metrics(ite_matrix_one_model, "BART_one_model", results_test, ate_test, data_test$ite)

#--------------------------------------------------------------
#--------------------------------------------------------------
#BART (two models)
treated <- data_train[data_train$z == 1,]
control <- data_train[data_train$z == 0,]

formula_rightside <- paste0("x_", 1:58, collapse = " + ")
bart_formula <- paste0("y ~ ", formula_rightside) %>% as.formula

bart_fit_treated <- bart2(formula = bart_formula, data = treated, keepTrees = TRUE, combineChains = TRUE)
bart_fit_control <- bart2(formula = bart_formula, data = control, keepTrees = TRUE, combineChains = TRUE)


# Prediction for all units
prediction_matrix_treated_all <- predict(bart_fit_treated, newdata = X) 
prediction_matrix_control_all <- predict(bart_fit_control, newdata = X)

# Calcuate ITEs
ite_matrix_two_models_all <- prediction_matrix_treated_all - prediction_matrix_control_all
ite_two_models_all <- colMeans(ite_matrix_two_models_all)


#---- Metrics
# PEHE
pehe_two_models_train <- sqrt(mean((data_train$ite - ite_two_models_all[train])^2))
pehe_two_models_test <- sqrt(mean((data_test$ite - ite_two_models_all[-train])^2))

# ATE
ate_bias_two_models_train <- ate_train - mean(ite_two_models_all[train])
ate_bias_two_models_test <- ate_test - mean(ite_two_models_all[-train])

# Credibility intervals
credible_intervals_two_models_all <- t(apply(ite_matrix_two_models_all, 2, quantile, probs = c(0.025, 0.975)))
colnames(credible_intervals_two_models_all) <- c("lower", "upper")

# Interval length
ci_length_two_models_all <- credible_intervals_two_models_all[,2] - credible_intervals_two_models_all[,1]

ci_length_two_models_train <- ci_length_two_models_all[train] %>% mean()
ci_length_two_models_test <- ci_length_two_models_all[-train] %>% mean()

# Coverage
is_covered_two_models_all <- ifelse(credible_intervals_two_models_all[,1] <= ite &
                                 ite <= credible_intervals_two_models_all[,2], 1, 0)
coverage_two_models_train <- sum(is_covered_two_models_all[train]) / length(is_covered_two_models_all[train])
coverage_two_models_test <- sum(is_covered_two_models_all[-train]) / length(is_covered_two_models_all[-train])


# Save results
results_train$pehe_results_train$BART_two_models[[length(results_train$pehe_results_train$BART_two_models) + 1]]  <- pehe_two_models_train
results_train$ate_bias_results_train$BART_two_models[[length(results_train$ate_bias_results_train$BART_two_models) + 1]]  <- ate_bias_two_models_train
results_train$ci_length_results_train$BART_two_models[[length(results_train$ci_length_results_train$BART_two_models) + 1]]  <- ci_length_two_models_train 
results_train$coverage_results_train$BART_two_models[[length(results_train$coverage_results_train$BART_two_models) + 1]]  <- coverage_two_models_train

results_test$pehe_results_test$BART_two_models[[length(results_test$pehe_results_test$BART_two_models) + 1]]  <- pehe_two_models_test
results_test$ate_bias_results_test$BART_two_models[[length(results_test$ate_bias_results_test$BART_two_models) + 1]]  <- ate_bias_two_models_test
results_test$ci_length_results_test$BART_two_models[[length(results_test$ci_length_results_test$BART_two_models) + 1]]  <- ci_length_two_models_test 
results_test$coverage_results_test$BART_two_models[[length(results_test$coverage_results_test$BART_two_models) + 1]]  <- coverage_two_models_test


#--------------------------------------------------------------
#--------------------------------------------------------------
# BART with Propensity score as variable (using BART)

fit_ps_BART <- bartc(response = y, treatment = z, confounders = X, subset = train, method.rsp = "bart", method.trt = "bart",
                  estimand = "ate", keepTrees = TRUE)

# For each observation in train, put them in both treatment and control  
z_0 <- rep(0, nrow(data_test))
z_1 <- rep(1, nrow(data_test))

prediction_ps_BART_treated <-  predict(fit_ps_BART, newdata = data.frame(X[-train,], z = z_1), type = "mu")
prediction_ps_BART_control <-  predict(fit_ps_BART, newdata = data.frame(X[-train,], z = z_0), type = "mu")

#---- Out of sample metrics
# Calculate ITEs for all iterations
ite_matrix_ps_BART <- prediction_ps_BART_treated - prediction_ps_BART_control

results_test <- get_test_metrics(ite_matrix_ps_BART, "BART_ps_BART", results_test, ate_test, data_test$ite)


#----
# In sample metrics
results_train <- get_train_metrics(fit_ps_BART, "BART_ps_BART", results_train, ate_train, data_train$ite)

#--------------------------------------------------------------
#--------------------------------------------------------------
# BART with Propensity score as variable (using a GLM)

fit_ps_glm <- bartc(response = y, treatment = z, confounders = X, subset = train, method.rsp = "bart", method.trt = "glm",
                     estimand = "ate", keepTrees = TRUE)

#----
# In sample metrics
results_train <- get_train_metrics(fit_ps_glm, "BART_ps_glm", results_train, ate_train, data_train$ite)

# For each observation in train, put them in both treatment and control  
z_0 <- rep(0, nrow(data_test))
z_1 <- rep(1, nrow(data_test))

prediction_ps_glm_treated <-  predict(fit_ps_glm, newdata = data.frame(X[-train,], z = z_1), type = "mu")
prediction_ps_glm_control <-  predict(fit_ps_glm, newdata = data.frame(X[-train,], z = z_0), type = "mu")

#---- Out of sample metrics
# Calculate ITEs for all iterations
ite_matrix_ps_glm <- prediction_ps_glm_treated - prediction_ps_glm_control

results_test <- get_test_metrics(ite_matrix_ps_glm, "BART_ps_glm", results_test, ate_test, data_test$ite)

#--------------------------------------------------------------
#--------------------------------------------------------------
# Bayesian Causal Forest
X_matrix <- model.matrix(~ . - 1, data = X)

fit_bcf <- bcf(y[train], z[train], X_matrix, X_matrix, pihat = fit_ps_BART$p.score, nburn = 2000, nsim = 5000)



#--------------------------------------------------------------
#--------------------------------------------------------------
# Propensity Score Matching + Regression
formula_rightside_ps <- paste0("x_", 1:58, collapse = " + ")
ps_formula <- paste0("z ~ ", formula_rightside_ps) %>% as.formula

# Nearest neighbor matching with logistic regression
match_model <- matchit(ps_formula,
                       data = data_train,
                       method = "optimal",      
                       distance = "logit")

# Compute standardized mean differences
smd_mean <- mean(smds$Balance$Diff.Adj)
smd_max <- max(smds$Balance$Diff.Adj)
smd_too_big <- sum(abs(smds$Balance$Diff.Adj) >= 0.1)

matched_data <- match.data(match_model)

match_regression_formula <- paste0("y ~ z + ", formula_rightside_ps) %>% as.formula

fit_match_regression <- lm(match_regression_formula, matched_data)
adjusted_se_matching <- vcovCL(fit_match_regression, cluster = ~ pair_id)

coefs_match <- summary(fit_match_regression)$coef
ate_match_train <- coefs_match["z", 1]
se_match <- coefs_match["z", 2]

results_train <- get_metrics_not_bart(results_train, "matching", ate_match_train, se_match, ate_train)

#--------------------------------------------------------------
#--------------------------------------------------------------
# Weighting + Regression

# Calculate Propensity scores
fit_ps_scores <- glm(ps_formula, data = data_train, family = binomial)
ps_scores_prediction <- predict(fit_ps_scores, type = "response")
weights <- ifelse(data_train$z == 1, 1/ps_scores_prediction, 1/(1-ps_scores_prediction))

# Check balance
balance_measures <- bal.tab(ps_formula, 
                   data = data_train, 
                   weights = weights,  
                   method = "weighting")
print(balance_measures)

# Compute standardized mean differences
smd_mean_weighting <- mean(balance_measures$Balance$Diff.Adj)
smd_max_weighting <- max(balance_measures$Balance$Diff.Adj)
smd_too_big_weighting <- sum(abs(balance_measures$Balance$Diff.Adj) >= 0.1)

love.plot(bal.tab)

fit_weights_double_rob <- lm(match_regression_formula, data = data_train, weights = weights)
# Adjust standard errors
adjusted_se_weighting <- coeftest(fit_weights_double_rob, vcov = vcovHC(fit, type = "HC0")) 

ate_weighting_train <- adjusted_se_weighting["z", 1]
se_weighting <- adjusted_se_weighting["z", 2]

results_train <- get_metrics_not_bart(results_train, "weighting", ate_weighting_train, se_weighting, ate_train)


#--------------------------------------------------------------
#--------------------------------------------------------------
# Causal forest

fit_causal_forest <- causal_forest(X_matrix[train,], y[train], z[train])

prediction_cf_test <- predict(fit_causal_forest, X_matrix[-train,], estimate.variance = TRUE)
ite_cf_test <- prediction_cf_test$predictions

# PEHE
pehe_cf_test <- sqrt(mean((data_test$ite - ite_cf_test)^2))

# Bias
ate_cf_test <- mean(ite_cf_test)
ate_bias_cf_test <- ate_test - ate_cf_test 

# Confidence intervals
var_tau <- prediction_cf_test$variance.estimates
se_ate <- sqrt(sum(var_tau) / length(var_tau)^2)

ci_lower_cf_test <- ate_cf_test - 1.96 * se_ate
ci_upper_cf_test <- ate_cf_test + 1.96 * se_ate

# CI length
ci_length_cf_test <- ci_upper_cf_test - ci_lower_cf_test

# Coverage
is_covered_cf_test <- ci_lower_cf_test <= ate_test && ate_test <= ci_upper_cf_test

# Save results
results_train$pehe_results_train$BART_two_models[[length(results_train$pehe_results_train$BART_two_models) + 1]]  <- pehe_two_models_train
results_train$ate_bias_results_train$BART_two_models[[length(results_train$ate_bias_results_train$BART_two_models) + 1]]  <- ate_bias_two_models_train
results_train$ci_length_results_train$BART_two_models[[length(results_train$ci_length_results_train$BART_two_models) + 1]]  <- ci_length_two_models_train 
results_train$coverage_results_train$BART_two_models[[length(results_train$coverage_results_train$BART_two_models) + 1]]  <- coverage_two_models_train

results_test$pehe_results_test$causal_forest[[length(results_test$pehe_results_test$causal_forest) + 1]]  <- pehe_cf_test
results_test$ate_bias_results_test$causal_forest[[length(results_test$ate_bias_results_test$causal_forest) + 1]]  <- ate_bias_cf_test
results_test$ci_length_results_test$causal_forest[[length(results_test$ci_length_results_test$causal_forest) + 1]]  <- ci_length_cf_test
results_test$coverage_results_test$causal_forest[[length(results_test$coverage_results_test$causal_forest) + 1]]  <- is_covered_cf_test



#--------------------------------------------------------------
# Plots

# Sigma
plot_sigma(bart_fit_one_model, main = "Traceplot sigma - One Model (no PS)")
#TODO two models
plot_sigma(fit_ps_BART, main = "Trace plot - PS (BART)")
plot_sigma(fit_ps_glm, main = "Trace plot - PS (GLM)")



#----------------------------------------------------------------
# Functions

# calculate metrics and save in list (In sample)

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

# calculate metrics and save in list (Out of sample)

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

get_metrics_not_bart <- function(results_list, model_name, ate, se_ate, true_ate){
  #ite_cf_test <- prediction_cf_test$predictions
  
  # PEHE
  # pehe_cf_test <- sqrt(mean((data_test$ite - ite_cf_test)^2))
  
  # Bias
  ate_bias <- true_ate - ate 
  
  # Confidence intervals
  ci_lower <- ate_cf_test - 1.96 * se_ate
  ci_upper <- ate_cf_test + 1.96 * se_ate
  
  # CI length
  ci_length <- ci_upper - ci_lower
  
  # Coverage
  is_covered <- ci_lower <= true_ate && true_ate <= ci_upper
  
  # Save results
  # results_list$pehe_results_test[[model_name]][[length(results_lists$pehe_results_train[[model_name]]) + 1]]  <- pehe
  results_list$ate_bias_results_train[[model_name]][[length(results_list$ate_bias_results_train[[model_name]]) + 1]]  <- ate_bias
  results_list$ci_length_results_train[[model_name]][[length(results_list$ci_length_results_train[[model_name]]) + 1]]  <- ci_length 
  results_list$coverage_results_train[[model_name]][[length(results_list$coverage_results_train[[model_name]]) + 1]]  <- is_covered

  return(results_list)
  }

get_averages <- function(results_list){
  
  results_num <- lapply(results_list, function(metric) {
    lapply(metric, function(method) unlist(method))
  })
  
  avg_results <- lapply(results_num, function(metric) {
    if (startsWith(metric, "coverage_results"))
    sapply(metric, mean)   
  })
  
  avg_results_df <- do.call(rbind, avg_results)
  }
  
} 




