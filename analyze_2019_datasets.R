PATH_TRUE_ATES <- "C:/Users/luise/Documents/Masterarbeit/Daten/lowDim_trueATE.csv"
PATH_DATASETS <- "C:/Users/luise/Documents/Masterarbeit/Daten/"  

datasets <- read.csv(PATH_TRUE_ATES) 

dgp_overview <- datasets %>%
  group_by(DGPid) %>%
  slice(1) %>%
  ungroup()

dgp_overview$binary <- NA
dgp_overview$n_covariates <- NA


# TODO: vlt noch Index hinzufügen?

for (i in 1:nrow(dgp_overview)){
  dgp_index <- dgp_overview[i, "DGPid"]
  all_filenames <- datasets[datasets$DGPid == dgp_index$DGPid, "filename"]
  
  is_binary <- logical()
  n_cols <- integer()
  
  for (j in 1:length(all_filenames)){
    file_name <- all_filenames[j]
    dataset_j <- read.csv(paste0(PATH_DATASETS, file_name, ".csv")) %>% head(10)
    n_cols[j] <- ncol(dataset_j) - 2
    
    is_binary[j] <- all(dataset_j$Y == 0 | dataset_j$Y == 1)
  }
  
  # Check if they are all the same DGP
  dgp_overview$n_covariates[i] <- ifelse(length(unique(n_cols)) == 1, n_cols[1], -99)
  if (sum(is_binary) == length(all_filenames)){
    dgp_overview$binary[i] <- 1
  } else if (sum(is_binary) == 0){
    dgp_overview$binary[i] <- 0
  } else {
    dgp_overview$binary[i] <- -99
  }
}

write.csv(dgp_overview, "C:/Users/luise/Documents/Masterarbeit/Daten/dgp_overview.csv", row.names=FALSE)