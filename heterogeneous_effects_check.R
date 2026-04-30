# Uni-Computer
PATH_DATASETS <- "W:/Masterarbeit/Daten/"
PATH_TRUE_ATES <- "W:/Masterarbeit/Daten/trueATE/lowDim_trueATE.csv"
PATH_OVERVIEW <- "W:/Masterarbeit/Daten/trueATE/cont_dgp_overview.csv"
PATH_RESULTS <- "W:/Masterarbeit/Results/"

dgps <- read.csv(PATH_OVERVIEW)
all_datasets <- read.csv(PATH_TRUE_ATES)

set.seed(213)


df_results <- data.frame(
  dgp = character(),
  iteration = numeric(),
  cf_sd_ite = numeric(),
  bart_sd_ite = numeric()
)



for(i in 1:16){
  dgp_index <- dgps$DGPid[i]
  true_ate <- dgps$trueATE[i]
  datasets_dgp <- all_datasets %>% filter(DGPid == dgp_index)
  
  #for (j in 1:nrow(datasets_dgp)){
  for (j in 40:45){
      file_name <- datasets_dgp[j, "filename"]
      data <- read.csv(paste0(PATH_DATASETS, file_name, ".csv"))
      y <- data$Y
      z <- data$A
      X <- data %>% select(starts_with("V"))
      
      cf <- causal_forest(X, y, z)  
      cf_tau_hat <- predict(cf)$predictions  # estimated individual treatment effects
      cf_sd_tau <- sd(cf_tau_hat)

      bart_fit <- bartc(response = y, treatment = z, confounders = X, method.trt = "none", p.scoreAsCovariate = FALSE,
                        method.rsp = "bart", estimand = "ate", keepTrees = TRUE)
      
      predictions_treated <- predict(bart_fit, X, type = "mu.1") %>% colMeans()
      predictions_control <- predict(bart_fit, X, type = "mu.0") %>% colMeans()
      bart_tau_hat <- predictions_treated - predictions_control
      
      bart_sd_tau <- sd(bart_tau_hat)
      
      df_results <- rbind(df_results, data.frame(
        dgp = paste0("dgp", dgp_index),
        iteration =  j,
        cf_sd_ite = cf_sd_tau,
        bart_sd_ite = bart_sd_tau
      )
      )
      
      print(paste("dgp:", i, "iteration:", j))
    }
}


sd_summaries <- df_results %>% group_by(dgp) %>%
  summarise(sd_ite_cf = mean(cf_sd_ite),
    sd_ite_bart = mean(bart_sd_ite))

sd_summaries$dgp <- str_remove(sd_summaries$dgp, "dgp") %>% as.numeric()

sd_summaries_normalized <- sd_summaries %>% left_join(dgps %>% select(DGPid, trueATE, sd_y), by = c("dgp" = "DGPid"))

sd_summaries_normalized$sd_bart_rel <- abs(sd_summaries_normalized$sd_ite_bart / sd_summaries_normalized$trueATE)
sd_summaries_normalized$sd_cf_rel <- abs(sd_summaries_normalized$sd_ite_cf / sd_summaries_normalized$trueATE)
sd_summaries_normalized$sd_bart_var <- abs(sd_summaries_normalized$sd_ite_bart / sd_summaries_normalized$sd_y)
sd_summaries_normalized$sd_cf_var <- abs(sd_summaries_normalized$sd_ite_cf / sd_summaries_normalized$sd_y)

write.csv(sd_summaries_normalized, paste0(PATH_RESULTS, "2026-04-30_heterogenity_checks_results.csv"), row.names = FALSE)


bart_s_learner <- function(y, z, X, true_ate, method_trt = "none", ps_as_covariate = FALSE, 
                           k, n_trees, nu, q, testdata = X){
  bart_fit <- bartc(response = y, treatment = z, confounders = X,
                    method.rsp = "bart", method.trt = method_trt, 
                    estimand = "ate", keepTrees = TRUE, 
                    p.scoreAsCovariate = ps_as_covariate,
                    n.trees = n_trees, k = k,
                    sigdf = nu, sigquant = q)
  
  #----
  # In sample metrics
  # Predict each observation in treatment and in control group
  predictions_treated <- predict(bart_fit, testdata, type = "mu.1") 
  predictions_control <- predict(bart_fit, testdata, type = "mu.0") 
  predictions_ite <- predictions_treated - predictions_control #TODO vlt CATE oder ICATE nennen?
  results_run <- get_metrics(predictions_ite, true_ate)
  return(results_run)
}