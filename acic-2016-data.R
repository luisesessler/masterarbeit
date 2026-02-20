path <- "C:/Users/luise/Documents/Masterarbeit/data-ACIC-2016"
X <- read.csv(paste0(path, "/x.csv"))

dgp_folders <- 
  list.files(paste0(path, "/data_cf_all"), pattern = ".", all.files = FALSE, recursive = FALSE, full.names = TRUE)

results_train <- make_results_list()
results_test  <- make_results_list()

# Iterates through all folders
#for (i in 1:length(dgp_folders)){
for (i in 1:1){ # löschen
  folder_path <- dgp_folders[i]
  file_paths <- list.files(folder_path, pattern = ".", all.files = FALSE, recursive = FALSE, full.names = TRUE)
  
  # Iterates through all files in one folder
  #for (j in 1:length(file_paths)){
  for (j in 2:5){ # löschen
    df_zymu <- read.csv(file_paths[j])
    z <- df_zymu$z
    y_0 <- df_zymu$y0
    y_1 <- df_zymu$y1
    y <- z * y_1 + (1-z) * y_0
    mu_0 <- df_zymu$mu0
    mu_1 <- df_zymu$mu1
    
    # TODO true_ate <- aus ATE file rauslesen
    sample_ate <- mean(mu_1 - mu_0)
    true_ate <- sample_ate # TODO: löschen
    
    results_train <- bart_one_model(y, z, X, results_train, true_ate)
    results_train <- bart_two_models(y, z, X, results_train, true_ate)
    results_train <- bart_ps(y, z, X, results_train, true_ate, "bart")
    results_train <- bart_ps(y, z, X, results_train, true_ate, "glm")
  }
}
