# Mein Computer
# PATH_DATASETS <- "C:/Users/luise/Documents/Masterarbeit/Daten/"  
# PATH_TRUE_ATES <- "C:/Users/luise/Documents/Masterarbeit/Daten/lowDim_trueATE.csv"  
# PATH_OVERVIEW <- "C:/Users/luise/Documents/Masterarbeit/Daten/dgp_overview.csv" 

# Uni-Computer
PATH_DATASETS <- "W:/Masterarbeit/Daten/"  
PATH_TRUE_ATES <- "W:/Masterarbeit/Daten/trueATE/lowDim_trueATE.csv"  
PATH_OVERVIEW <- "W:/Masterarbeit/Daten/trueATE/cont_dgp_overview.csv" 
PATH_RESULTS <- "W:/Masterarbeit/Results/" 

dgps <- read.csv(PATH_OVERVIEW)
all_datasets <- read.csv(PATH_TRUE_ATES)

set.seed(213)

df_aiptw_results <- data.frame(
  dgp = character(),
  metric = character(),
  model = character(),
  iteration = numeric(),
  ps_model = character(),
  regression_model = character(),
  value = numeric()
)



#for(i in 1:length(cont_dgpis)){
for(i in 1:16){
  dgp_index <- dgps$DGPid[i]
  true_ate <- dgps$trueATE[i]
  
  datasets_dgp <- all_datasets %>% filter(DGPid == dgp_index)
  
  #for (j in 1:nrow(datasets_dgp)){
  for (j in 1:100){
    file_name <- datasets_dgp[j, "filename"]
    data <- read.csv(paste0(PATH_DATASETS, file_name, ".csv"))
    y <- data$Y
    z <- data$A
    X <- data %>% select(starts_with("V"))
    
    formula_rightside <- colnames(X) %>% paste0(collapse = " + ")
    
    ps_formula <- as.formula(paste0("z ~ ", formula_rightside))
    outcome_formula <- as.formula(
      paste0("y ~ ", formula_rightside)
    )
    
    metrics_run <- aiptw(
      y = y,
      z = z,
      X = X,
      true_ate = true_ate
    )
    
    df_aiptw_results <- rbind(
      df_aiptw_results,
      data.frame(
        dgp = rep(paste0("dgp", dgp_index), 4),
        metric = c("ate_estimate",
                   "ate_bias",
                   "ci_length",
                   "coverage"),
        model = rep("aiptw", 4),
        iteration = rep(j, 4),
        ps_model = rep("linear_basic", 4),
        regression_model = rep("linear_basic", 4),
        value = metrics_run
      )
    )
    print(paste("DGP: ", i, "Iteration:", j))
  }
  
}

 write.csv(df_aiptw_results, paste0(PATH_RESULTS, "2026-06-29_aiptw_default", dgp_index,".csv"))
 write.csv(summary_aiptw, paste0(PATH_RESULTS, "2026-06-29_aiptw_dgp_summary", dgp_index,".csv"))
 

aiptw <- function(y, z, X, true_ate) {
  
  data <- data.frame(
    y = y,
    z = z,
    X
  )
  
  fit <- AIPW$new(
    Y = data$y,
    A = data$z,
    W = data[, !(names(data) %in% c("y", "z"))],
    Q.SL.library = "SL.glm",
    g.SL.library = "SL.glm",
    k_split = 1
  )
  
  fit$fit()
  
  result <- fit$summary(g.bound = 0.1)
  
  ate_est <- fit$result["Mean Difference", "Estimate"]
  lower_ci  <- fit$result["Mean Difference", "95% LCL"]
  upper_ci  <- fit$result["Mean Difference", "95% UCL"]
  
  
  get_metrics_aiptw(
    ate_estimate = ate_est,
    lower_ci = lower_ci,
    upper_ci = upper_ci,
    ate_true = true_ate
  )
}

get_metrics_aiptw <- function(ate_estimate, lower_ci, upper_ci, ate_true){
  # Bias
  ate_bias <- ate_true - ate_estimate
  
  # Confidence intervals
  ci_lower <- lower_ci
  ci_upper <- upper_ci
  
  # CI length
  ci_length <- ci_upper - ci_lower
  
  # Coverage
  is_covered <- ifelse(ci_lower <= ate_true && ate_true <= ci_upper, 1, 0)
  
  # Save results
  metrics <- c(
    ate_estimate = ate_estimate,
    ate_bias = ate_bias,
    ci_length = ci_length,
    is_covered = is_covered
  )
  return(metrics)
}
