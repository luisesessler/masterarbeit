PATH_DATASETS <- "C:/Users/luise/Documents/Masterarbeit/Daten/"  
PATH_TRUE_ATES <- "C:/Users/luise/Documents/Masterarbeit/Daten/lowDim_trueATE.csv"  
PATH_OVERVIEW <- "C:/Users/luise/Documents/Masterarbeit/Daten/dgp_overview.csv" 

dgpis <- read.csv(PATH_OVERVIEW)
all_datasets <- read.csv(PATH_TRUE_ATES)
cont_dgpis <- dgpis %>% filter(binary == 0)

for(i in cont_dgpis){
  dgp_index <- cont_dgpis$DGPid
  true_ate <- cont_dgpis$trueATE
  
  results <- make_results_list()
  
  datasets_dgp <- all_datasets %>% filter(DGPid == dgp_index)
  
  for (j in 1:length(datasets_dgp)){
    file_name <- datasets_dgp[j, "filename"]
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
  
  
  
  # TODO: results mit name paste0("dgp", dgp_index)
}




summary_dgp57 <- create_results_table(results_dgp57)