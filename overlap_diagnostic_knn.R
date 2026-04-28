# Uni-Computer
PATH_DATASETS <- "W:/Masterarbeit/Daten/"  
PATH_TRUE_ATES <- "W:/Masterarbeit/Daten/trueATE/lowDim_trueATE.csv"  
PATH_OVERVIEW <- "W:/Masterarbeit/Daten/trueATE/dgp_overview.csv" 
PATH_RESULTS <- "W:/Masterarbeit/Results/" 

dgpis <- read.csv(PATH_OVERVIEW)
all_datasets <- read.csv(PATH_TRUE_ATES)
cont_dgpis <- dgpis %>% filter(binary == 0)

set.seed(213)

overlap_bart <- data.frame(dgp = character(),
                           iteration = numeric(),
                           discarded_ratio10 = numeric(),
                           discarded_ratio5 = numeric(),
                           discarded_max = numeric())

k <-  5

for (i in 1:nrow(cont_dgpis)){
  dgp_index <- cont_dgpis$DGPid[i]
  datasets_dgp <- all_datasets %>% filter(DGPid == dgp_index)
  dataset_idx <- sample(1:100, 5)
  for(j in dataset_idx){
  file_name <- datasets_dgp[j, "filename"]
  data <- read.csv(paste0(PATH_DATASETS, file_name, ".csv"))
  y <- data$Y
  z <- data$A
  X <- data %>% select(starts_with("V"))
  
  n <- nrow(X)
  
  nn <- get.knn(X, k = k + 1)  # +1 because the point itself is included
  
  overlap_scores <- numeric(n)
  
  for (l in 1:n) {
    neighbors <- nn$nn.index[l, ]
    neighbors <- neighbors[neighbors != l]  # remove self
    neighbor_labels <- y[neighbors]
    
    # Fraction of neighbors with different class
    overlap_scores[l] <- sum(neighbor_labels != y[l]) / k
  }
  
  # Results
  mean_overlap <- mean(overlap_scores)
  mean_overlap
  head(overlap_scores)
  
  
  
  overlap_bart <- rbind(overlap_bart, data.frame(dpg = dgp_index, 
                                                 iteration = 1, 
                                                 discarded_ratio10 = sum(discard_10), 
                                                 discarded_ratio5 = sum(discard_05), 
                                                 discarded_max = sum(discard)))
  }
}

write.csv(overlap_bart, paste0(PATH_RESULTS, "2026-04-16_overlap_diagnostics_BART.csv"))