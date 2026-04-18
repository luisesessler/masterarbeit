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

df_results <- data.frame(
  dgp = character(),
  metric = character(),
  model = character(),
  iteration = numeric(),
  n.samples = numeric(),
  n.chains = numeric(),
  value = numeric()
)

#for(i in 1:length(cont_dgpis)){
for(i in 10){
  dgp_index <- dgps$DGPid[i]
  true_ate <- dgps$trueATE[i]
  datasets_dgp <- all_datasets %>% filter(DGPid == dgp_index)

  #for (j in 1:nrow(datasets_dgp)){
  for (j in 1:5){
    file_name <- datasets_dgp[j, "filename"]
    data <- read.csv(paste0(PATH_DATASETS, file_name, ".csv"))
    y <- data$Y
    z <- data$A
    X <- data %>% select(starts_with("V"))
    
    # results <- bart_s_learner(y, z, X, n_samples, true_ate)
    
    s_1000_5 <- bart_s_learner(y, z, X, n_samples = 1000, n_chains = 5, true_ate = true_ate)
    s_500_10 <- bart_s_learner(y, z, X, n_samples = 500, n_chains = 10, true_ate = true_ate)
    
    
    
    df_results <- rbind(df_results, data.frame(
      dgp = rep(paste0("dgp", dgp_index), 4),
      metric = c("ate_estiamte", "ate_bias", "ci_length", "coverage"),
      model = rep("bart_s-learner", 4),
      iteration =  rep(j, 4),
      n.samples = rep(1000, 4),
      n.chains = rep(5, 4),
      value = s_1000_5
    )
    )
    
    
    df_results <- rbind(df_results, data.frame(
      dgp = rep(paste0("dgp", dgp_index), 4),
      metric = c("ate_estiamte", "ate_bias", "ci_length", "coverage"),
      model = rep("bart_s-learner", 4),
      iteration =  rep(j, 4),
      n.samples = rep(500, 4),
      n.chains = rep(10, 4),
      value = s_500_10
    )
    )
    
    
    print(paste("DGP: ", i, "Iteration:", j))
  }
  
}

saveRDS(all_results, paste0(PATH_RESULTS, "2026-03-19_ps-bart_dgp5-6.RData"))