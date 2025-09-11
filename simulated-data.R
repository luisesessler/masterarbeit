# Set seed for reproducibility
set.seed(42)

# Sample size
n <- 1000

# Covariates (similarly distributed for everyone)
x1 <- rnorm(n, mean = 0, sd = 1)
x2 <- rnorm(n, mean = 0, sd = 1)
x3 <- rnorm(n, mean = 0, sd = 1)

# Data frame of covariates
X <- data.frame(x1, x2, x3)

# Treatment assignment model — moderate coefficients → good overlap
logit_p <- 0.2 * x1 - 0.2 * x2 + 0.1 * x3
p_treat <- plogis(logit_p)  # Converts log-odds to probabilities (0–1)

# Simulate treatment assignment
treat <- rbinom(n, 1, p_treat)

# Check overlap
hist(p_treat[treat == 1], col = rgb(1, 0, 0, 0.5), main = "Propensity Score Overlap", xlab = "Propensity Score", breaks = 20)
hist(p_treat[treat == 0], col = rgb(0, 0, 1, 0.5), add = TRUE, breaks = 20)
legend("topright", legend = c("Treated", "Control"), fill = c(rgb(1, 0, 0, 0.5), rgb(0, 0, 1, 0.5)))


y1 <- 2 + 0.4 * 1 + 0.7 * x1 - 0.5 * x2 + 0.3 * x3 + rnorm(n)
y0 <- 2 +           0.7 * x1 - 0.5 * x2 + 0.3 * x3 + rnorm(n)
y <-  (y1 * treat) + (y0 * (1 - treat))
ite <- y1 - y0
ate_sim <- mean(ite)
att_sim <- mean(ite[treat == 1])
atc_sim <- mean(ite[treat == 0])

df_sim <- data.frame(y, y0, y1, ite, treat, x1, x2, x3)

head(df_sim)


# Data split
percentage_train <- 0.7
train <- sample(nrow(df_sim), percentage_train * nrow(df_sim), replace = FALSE)

data_train_sim <- df_sim[train,]
data_test_sim <- df_sim[-train,]



# BART
z <- treat
fit_ps_BART <- bartc(response = y, treatment = z, confounders = X, method.rsp = "bart", method.trt = "bart",
                     estimand = "ate", keepTrees = TRUE)

sim_summary <- summary(fit_ps_BART)


head(fit_ps_BART$mu.hat.obs)


# For each observation in train, put them in both treatment and control  
z_0 <- rep(0, nrow(data_test_sim))
z_1 <- rep(1, nrow(data_test_sim))

prediction_ps_BART_treated <-  predict(fit_ps_BART, newdata = data.frame(X[-train,], z = z_1), type = "mu")
prediction_ps_BART_control <-  predict(fit_ps_BART, newdata = data.frame(X[-train,], z = z_0), type = "mu")

pred_means_ps_BART_treated <- colMeans(prediction_ps_BART_treated)
pred_means_ps_BART_control <- colMeans(prediction_ps_BART_control)

ite_ps_BART <- pred_means_ps_BART_treated - pred_means_ps_BART_control
ate_ps_BART <- mean(ite_ps_BART)

pehe_ps_BART <- sqrt(mean((data_test_sim$ite - ite_ps_BART)^2))


# PS scores
glm_fit_ps <- glm(treat ~ x1 + x2 + x3, family = binomial, data = df_sim)
df_sim$propensity_score <- predict(glm_fit_ps, type = "response")

hist(df_sim$propensity_score)
hist(df_sim$propensity_score[df_sim$treat == 1])
hist(df_sim$propensity_score[df_sim$treat == 0])

#--------------------------------------------------------------
#BART (two models)
treated <- data_train_sim[data_train_sim$treat == 1,]
control <- data_train_sim[data_train_sim$treat == 0,]


bart_fit_treated <- bart2(y ~ x1 + x2 + x3, data = treated, keepTrees = TRUE)
bart_fit_control <- bart2(y ~ x1 + x2 + x3, data = control, keepTrees = TRUE)

prediction_treated <- predict(bart_fit_treated, newdata = data_test_sim[c("x1", "x2", "x3")])
prediction_control <- predict(bart_fit_control, newdata = data_test_sim[c("x1", "x2", "x3")])

pred_means_treated <- colMeans(prediction_treated)
pred_means_control <- colMeans(prediction_control)

ite_two_models <- pred_means_treated - pred_means_control
ate_two_models <- mean(ite_two_models)

pehe_two_models <- sqrt(mean((data_test$ite - ite_two_models)^2))


# PS matching
match_model <- matchit(treat ~ x1 + x2 + x3,
                       data = data_train_sim,
                       method = "nearest",      
                       distance = "logit")

matched_data <- match.data(match_model)
head(matched_data)

att_model <- lm(y ~ treat, data = matched_data, weights = weights)
summary(att_model)



