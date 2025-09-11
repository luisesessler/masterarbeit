library(bartCause)
library(dbarts)
library(dplyr)
library(MatchIt)
library(grf)

# TODO Speichern in List über index aus Iteration
n_iter <- 1

# ---- Set up Ergebnisse speichern ----
list_names <- c("BART_one_model", "BART_two_models", "BART_ps_BART", "BART_ps_glm")

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
#ACIC 2016 datafiles

X <- read.csv("C:\\Users\\luise\\Documents\\Masterarbeit\\data-ACIC-2016\\data\\x.csv")
counterfactuals <- read.csv("C:\\Users\\luise\\Documents\\Masterarbeit\\data-ACIC-2016\\data_cf_all\\1\\zymu_13.csv")
z <- counterfactuals$z
y1 <- counterfactuals$y1
y0 <- counterfactuals$y0
y <- ifelse(z == 1, y1, y0)
ite <- y1 - y0

# Overall ATE
ate_complete <- mean(y1 - y0)


# merge
X[] <- lapply(X, function(x) if(is.character(x)) factor(x) else x)
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
# Propensity Score Matching


#--------------------------------------------------------------
#--------------------------------------------------------------
# Lasso Regression



#--------------------------------------------------------------
#--------------------------------------------------------------
# Causal forest




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




