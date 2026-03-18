hyperparameter <- list(
  "bart" = c("ntrees", "k"),
  "causal_forest" = c("ntrees"),
  "matching" = c("treatment_mechanism", "outcome_formula",  )
)

hyper_bart <- expand.grid(
  ntrees = c()
)

hyper_cf <- expand.grid(
  
)

hyper_matching <- expand.grid(
  treatment_mechanism = c("bart", "glm_no_interactions", "glm_interactions"),
  outcome_formula = c("linear", "interactions")
)

results_tuning <- list()


dgps <- c(30) # For which DGPS do I tune?

# Matching
for (i in 1:length(dgps)){
  df_tuning_results <- data.frame(dgp = NA,
                                  iteration = NA,
                                  
                                  metric = NA,
                                  value = NA)  
}





