library(bartCause)
library(dbarts)
library(dplyr)
library(MatchIt)
library(grf)

# TODO Speichern in List über index aus Iteration
n_iter <- 1

# Save results
list_names <- c("BART_one_model", "BART_two_models", "BART_ps_BART", "BART_ps_glm")

n_metrics <- length(list_names)
template <- list()

# Use replicate() to clone the template
results <- replicate(n_metrics, template, simplify = FALSE)

names(results) <- list_names


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
df <- data.frame(X, z, y0, y1, y, ite)
df[] <- lapply(df, function(x) if(is.character(x)) factor(x) else x)

# How many treated & non-treated?
perc_treated <- nrow(df[z == 1, ])/nrow(df)
perc_control <- nrow(df[z == 0, ])/nrow(df)


#TODO: Split training/test
percentage_train <- 0.7
train <- sample(nrow(df), percentage_train * nrow(df), replace = FALSE)

data_train <- df[train,]
data_test <- df[-train,]


# ATEs
ate_train <- mean(data_train$y1 - data_train$y0)
ate_test <- mean(data_test$y1 - data_test$y0)

#--------------------------------------------------------------
#--------------------------------------------------------------
# Fit models
#--------------------------------------------------------------
#BART, one model, no PS in X
bart_fit_one_model <- bartc(response = y, treatment = z, confounders = X, subset = train, method.rsp = "bart", method.trt = "none",
                             estimand = "ate", keepTrees = TRUE, p.scoreAsCovariate = FALSE)

# For each observation in train, put them in both treatment and control  
z_0 <- rep(0, nrow(data_test))
z_1 <- rep(1, nrow(data_test))

# Predict each observation in treatment and in control group
prediction_one_model_treated <-  predict(bart_fit, newdata = data.frame(X[-train,], z = z_1), type = "mu")
prediction_one_model_control <-  predict(bart_fit, newdata = data.frame(X[-train,], z = z_0), type = "mu")

# Calculate ITEs for all iterations
ite_matrix_one_model <- prediction_one_model_treated - prediction_one_model_control

# Mean ITE for each observation
mean_ite_one_model <- colMeans(ite_matrix_one_model) 

# ATE
ate_one_model <- mean(mean_ite_one_model)

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
results$BART_one_model[[length(results$BART_one_model) + 1]]  <- pehe_one_model

#--------------------------------------------------------------
#--------------------------------------------------------------
#BART (two models)
treated <- data_train[data_train$z == 1,]
control <- data_train[data_train$z == 0,]

formula_rightside <- paste0("x_", 1:58, collapse = " + ")
bart_formula <- paste0("y ~ ", formula_rightside) %>% as.formula


bart_fit_treated <- bart2(formula = bart_formula, data = treated, keepTrees = TRUE)
bart_fit_control <- bart2(formula = bart_formula, data = control, keepTrees = TRUE)

test_covariates <- data_test %>% select(starts_with("x_"))

prediction_treated <- predict(bart_fit_treated, newdata = test_covariates)
prediction_control <- predict(bart_fit_control, newdata = test_covariates)

pred_means_treated <- colMeans(prediction_treated)
pred_means_control <- colMeans(prediction_control)

ite_two_models <- pred_means_treated - pred_means_control
ate_two_models <- mean(ite_two_models)

pehe_two_models <- sqrt(mean((data_test$ite - ite_two_models)^2))

results$BART_two_models[[length(results$BART_two_models) + 1]]  <- pehe_two_models

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

pred_means_ps_BART_treated <- colMeans(prediction_ps_BART_treated)
pred_means_ps_BART_control <- colMeans(prediction_ps_BART_control)

ite_ps_BART <- pred_means_ps_BART_treated - pred_means_ps_BART_control
ate_ps_BART <- mean(ite_ps_BART)

pehe_ps_BART <- sqrt(mean((data_test$ite - ite_ps_BART)^2))

results$BART_ps_BART[[length(results$BART_ps_BART) + 1]]  <- pehe_ps_BART

#--------------------------------------------------------------
#--------------------------------------------------------------
# BART with Propensity score as variable (using a GLM)

fit_ps_glm <- bartc(response = y, treatment = z, confounders = X, subset = train, method.rsp = "bart", method.trt = "glm",
                     estimand = "ate", keepTrees = TRUE)

# For each observation in train, put them in both treatment and control  
z_0 <- rep(0, nrow(data_test))
z_1 <- rep(1, nrow(data_test))

prediction_ps_glm_treated <-  predict(fit_ps_glm, newdata = data.frame(X[-train,], z = z_1), type = "mu")
prediction_ps_glm_control <-  predict(fit_ps_glm, newdata = data.frame(X[-train,], z = z_0), type = "mu")

pred_means_ps_glm_treated <- colMeans(prediction_ps_glm_treated)
pred_means_ps_glm_control <- colMeans(prediction_ps_glm_control)

ite_ps_glm <- pred_means_ps_glm_treated - pred_means_ps_glm_control
ate_ps_glm <- mean(ite_ps_glm)

pehe_ps_glm <- sqrt(mean((data_test$ite - ite_ps_glm)^2))

results$BART_ps_glm[[length(results$ps_glm) + 1]]  <- pehe_ps_glm

#--------------------------------------------------------------
#--------------------------------------------------------------
# Propensity Score Matching


#--------------------------------------------------------------
#--------------------------------------------------------------
# Lasso Regression



#--------------------------------------------------------------
#--------------------------------------------------------------
# Causal forest

X_numeric <- model.matrix(~ . -1, data = X)

fit_cf <- causal_forest(X_numeric[train,], y[train], z[train])

ite_cf <- predict(fit_cf, newdata = X_numeric[-train,])$predictions

ate_cf <- mean(ite_cf)


#--------------------------------------------------------------
# Plots

# Sigma
plot_sigma(bart_fit_one_model, main = "Traceplot sigma - One Model (no PS)")
#TODO two models
plot_sigma(fit_ps_BART, main = "Trace plot - PS (BART)")
plot_sigma(fit_ps_glm, main = "Trace plot - PS (GLM)")




