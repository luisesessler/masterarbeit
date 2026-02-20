formula_rightside <- colnames(X) %>% paste0(collapse = " + ")

ps_formula <- as.formula(paste0("z ~ ", formula_rightside))
outcome_formula <- as.formula(paste0("y ~ z + ", formula_rightside))