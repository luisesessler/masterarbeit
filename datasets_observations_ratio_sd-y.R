# TODO: merge with analyze_2019_datasets.R

# Uni-Computer
PATH_DATASETS <- "W:/Masterarbeit/Daten/"  
PATH_TRUE_ATES <- "W:/Masterarbeit/Daten/trueATE/lowDim_trueATE.csv"  
PATH_OVERVIEW <- "W:/Masterarbeit/Daten/trueATE/dgp_overview.csv" 
PATH_RESULTS <- "W:/Masterarbeit/Results/" 

dgpis <- read.csv(PATH_OVERVIEW)
all_datasets <- read.csv(PATH_TRUE_ATES)
cont_dgpis <- dgpis %>% filter(binary == 0)

cont_dgpis$n_observation <- NA
cont_dgpis$treated_share <- NA
cont_dgpis$sd_y <- NA


for(i in 1:nrow(cont_dgpis)){
  dgp_index <- cont_dgpis$DGPid[i]
  datasets_dgp <- all_datasets %>% filter(DGPid == dgp_index)
  
  file_name <- datasets_dgp[1, "filename"]
  data <- read.csv(paste0(PATH_DATASETS, file_name, ".csv"))
  
  n_treated <- data %>% filter(A == 1) %>%
    nrow()
   share_treated <- n_treated/nrow(data)
  
  
  cont_dgpis$n_observation[i] <- nrow(data)
  cont_dgpis$treated_share[i] <-  share_treated
  
  cont_dgpis$sd_y[i] <- sd(data$Y)
}

write.csv(cont_dgpis, "W:/Masterarbeit/Daten/trueATE/cont_dgp_overview.csv", row.names=FALSE)