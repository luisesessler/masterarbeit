datasets <- read.csv("W:/Masterarbeit/Daten/trueATE/lowDim_trueATE.csv")

dgp_overview <- datasets %>%
  group_by(DGPid) %>%
  slice(1) %>%
  ungroup()

# TODO: vlt noch Index hinzufügen?

for (i in 1:nrow(dgp_overview)){
  
}

dgp_number <- 57 #dynamisch über Schleifen

dgp57 <- datasets %>% filter(DGPid == dgp_number)
results_dgp57 <- make_results_list()

true_ate <- 0.5 #hardgecoded, noch auslesen aus file


for (i in 1:100){
  file_name <- dgp57[i, "filename"]
  data <- read.csv(paste0("W:/Masterarbeit/Daten/", file_name, ".csv"))
  y <- data$Y
  z <- data$A
  X <- data %>% select(starts_with("V"))
  
  # Regression formulas
  formula_rightside <- colnames(X) %>% paste0(collapse = " + ")
  ps_formula <- as.formula(paste0("z ~ ", formula_rightside))
  outcome_formula <- as.formula(paste0("y ~ z + ", formula_rightside))
  
  #PS Score calculation (needed for weighting, bcf, MAYBE psBART, MAYBE Matching?)
  fit_ps_scores <- glm(ps_formula, data = data, family = binomial)
  ps_scores_prediction <- predict(fit_ps_scores, type = "response")
  
  results_dgp57 <- bart_one_model(y, z, X, results_dgp57, true_ate)
  results_dgp57 <- bart_ps(y, z, X, results_dgp57, true_ate, "bart")
  results_dgp57 <- bart_ps(y, z, X, results_dgp57, true_ate, "glm")  
}



summary_dgp57 <- create_results_table(results_dgp57)