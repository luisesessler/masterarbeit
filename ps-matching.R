library(MatchIt)
library(cobalt)

formula_rightside_ps <- paste0("x_", 1:58, collapse = " + ")
ps_formula <- paste0("z ~ ", formula_rightside) %>% as.formula

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