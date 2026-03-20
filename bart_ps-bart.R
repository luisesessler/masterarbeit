# Mein Computer
# PATH_DATASETS <- "C:/Users/luise/Documents/Masterarbeit/Daten/"  
# PATH_TRUE_ATES <- "C:/Users/luise/Documents/Masterarbeit/Daten/lowDim_trueATE.csv"  
# PATH_OVERVIEW <- "C:/Users/luise/Documents/Masterarbeit/Daten/dgp_overview.csv" 

# Uni-Computer
PATH_DATASETS <- "W:/Masterarbeit/Daten/"  
PATH_TRUE_ATES <- "W:/Masterarbeit/Daten/trueATE/lowDim_trueATE.csv"  
PATH_OVERVIEW <- "W:/Masterarbeit/Daten/trueATE/dgp_overview.csv" 
PATH_RESULTS <- "W:/Masterarbeit/Results/" 

dgpis <- read.csv(PATH_OVERVIEW)
all_datasets <- read.csv(PATH_TRUE_ATES)
cont_dgpis <- dgpis %>% filter(binary == 0)

all_results <- list()
all_summaries <- list()

set.seed(213)

#for(i in 1:length(cont_dgpis)){
for(i in 5:6){
  dgp_index <- cont_dgpis$DGPid[i]
  true_ate <- cont_dgpis$trueATE[i]
  
  results <- make_results_list()
  
  datasets_dgp <- all_datasets %>% filter(DGPid == dgp_index)
  
  #for (j in 1:nrow(datasets_dgp)){
  for (j in 1:100){
    file_name <- datasets_dgp[j, "filename"]
    data <- read.csv(paste0(PATH_DATASETS, file_name, ".csv"))
    y <- data$Y
    z <- data$A
    X <- data %>% select(starts_with("V"))
    
    results <- bart_ps_bart_new(y, z, X, results, true_ate)
    print(paste("DGP: ", i, "Iteration:", j))
  }
  
  all_results[[paste0("dgp", dgp_index)]] <- results
  saveRDS(results, paste0(PATH_RESULTS, "2026-03-19_ps-bart_",dgp_index,".RData"))
}

saveRDS(all_results, paste0(PATH_RESULTS, "2026-03-19_ps-bart_dgp5-6.RData"))