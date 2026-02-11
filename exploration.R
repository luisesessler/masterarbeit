library(dplyr)

head(bart_fit_one_model$fit.rsp$yhat.train.mean, 10)
head(bart_fit_one_model$fit.rsp$yhat.test.mean, 10)
head(z, 10)

train_is_bigger <- ifelse(bart_fit_one_model$fit.rsp$yhat.train.mean > bart_fit_one_model$fit.rsp$yhat.test.mean, 1, 0)
sum(train_is_bigger == z)

sum(z == 0)

prediction_y1 <- predict(bart_fit_one_model, newdata = X, type = "y.1") %>% colMeans()
prediction_mu1 <- predict(bart_fit_one_model, newdata = X, type = "mu.1") %>% colMeans()

df_test <- data.frame(bart_fit_one_model$fit.rsp$yhat.train.mean, bart_fit_one_model$fit.rsp$yhat.test.mean,
                      prediction_y1, prediction_mu1)

set.seed(100)

predictions_y1 <-  predict(bart_fit, newdata = X, type = "y.1") 
predictions_y0 <-  predict(bart_fit, newdata = X, type = "y.0") 

difference_ys <- predictions_y1 - predictions_y0

predictions_mu1 <-  predict(bart_fit, newdata = X, type = "mu.1") 
predictions_mu0 <-  predict(bart_fit, newdata = X, type = "mu.0") 

difference_mus <- predictions_mu1 - predictions_mu0

mean_ys <- rowMeans(difference_ys)
mean_mus <- rowMeans(difference_mus)

predictions_ite <-  predict(bart_fit, newdata = X, type = "ite") 
mean_ites <- rowMeans(predictions_ite)
mean(mean_ites)
sd(mean_ites)

predictions_icate <-  predict(bart_fit, newdata = X, type = "icate") 
mean_icates <- rowMeans(predictions_icate)
mean(mean_icates)
sd(mean_icates)


quantile(mean_icates, probs = c(0.025, 0.975))
quantile(mean_ites, probs = c(0.025, 0.975))


fitted_ite <- extract(bart_fit, type="ite")
mean_extract_ite <- rowMeans(fitted_ite)

quantile(mean_extract_ite, probs = c(0.025, 0.975))

fitted_icate <- extract(bart_fit, type="icate")
mean_extract_icate <- rowMeans(fitted_icate)

quantile(mean_extract_icate, probs = c(0.025, 0.975))

