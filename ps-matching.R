library(MatchIt)
library(cobalt)

X <- read.csv("C:\\Users\\luise\\Documents\\Masterarbeit\\data-ACIC-2016\\data\\x.csv")
counterfactuals <- read.csv("C:\\Users\\luise\\Documents\\Masterarbeit\\data-ACIC-2016\\data_cf_all\\1\\zymu_13.csv")
z <- counterfactuals$z
y1 <- counterfactuals$y1
y0 <- counterfactuals$y0
y <- ifelse(z == 1, y1, y0)
ite <- y1 - y0


formula_rightside_ps <- paste0("x_", 1:58, collapse = " + ")
ps_formula <- paste0("z ~ ", formula_rightside_ps) %>% as.formula


glm_fit <- glm(ps_formula, data = data_train, family = binomial)

pred_glm <- predict(glm_fit, newdata = data_test, type = "response")

hist(pred_glm[data_train$z == 1], xlim = c(0,1), breaks = 10) # treatment
hist(pred_glm[data_train$z == 0], xlim = c(0,1), breaks = 10) # control



# Perform nearest neighbor matching with logistic regression
match_model <- matchit(ps_formula,
                       data = data_train,
                       method = "nearest",      
                       distance = "logit")   #TODO maybe change?

# Check covariate balance
# Summary balance table
summary(match_model)

# Love plot
love.plot(match_model)



# Extract matched data set
matched_data <- match.data(match_model)

# Check what's in it
head(matched_data)





# BART propensity scores
