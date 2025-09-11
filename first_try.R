dataset <- read.csv("C:\\Users\\luise\\Documents\\Masterarbeit\\realcause-master\\test_lalonde_cps_1.csv")

ate <- mean(data_lalonde_cps$ite)

sum(data_lalonde_cps$T == 1)
sum(data_lalonde_cps$ite == 0)


# Outcome

# Treatment assignment


# Covariates
X <- data_lalonde_cps[, paste0("x", rep(1:7))]


# BART
bartFit_lalonde_cps <- bartc(response = data_lalonde_cps$y, treatment = data_lalonde_cps$T, confounders = X, method.rsp = "bart", method.trt = "bart",
                             estimand = "ate")


# BART
ite_samples <- fitted(bartFit_lalonde_cps, type = "icate")
ate_estimate <- mean(ite_samples)  # this gives the ATE

bias <- ate_lalonde_cps - ate_estimate 

ite_pred <- fitted(bartFit_lalonde_cps, type = "icate")
pehe <- sqrt(mean((data_lalonde_cps$ite - ite_pred)^2))




#Propensity Score Matching
vars <- paste0("x", 1:7)
form_str <- paste("T ~", paste(vars, collapse = " + "))
form <- as.formula(form_str)

match_model_lalonde_cps <- matchit(form,
                                   data = data_lalonde_cps,
                                   method = "nearest",   # matching method
                                   ratio = 1,
                                   estimate = "ATE")  

matched_df_lalonde_cps <- match.data(match_model_lalonde_cps)

design <- svydesign(ids = ~1, weights = ~weights, data = matched_df_lalonde_cps)
ate_result <- svyglm(y ~ T, design = design)
summary(ate_result)