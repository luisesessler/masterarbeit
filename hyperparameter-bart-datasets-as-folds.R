# Uni-Computer
PATH_DATASETS <- "W:/Masterarbeit/Daten/"  
PATH_TRUE_ATES <- "W:/Masterarbeit/Daten/trueATE/lowDim_trueATE.csv"  
PATH_OVERVIEW <- "W:/Masterarbeit/Daten/trueATE/dgp_overview.csv" 
PATH_RESULTS <- "W:/Masterarbeit/Results/" 

dgpis <- read.csv(PATH_OVERVIEW)
all_datasets <- read.csv(PATH_TRUE_ATES)
cont_dgpis <- dgpis %>% filter(binary == 0)

hyper_bart <- expand.grid(
  k = c(1, 2, 3),
  nu = c(3, 7, 10),
  n_trees = c(75, 150, 200),
  n_samples = c(500, 1000)
)

# Create results dataframe
all_tuning_results <- data.frame(
  dgp = character(),
  metric = character(),
  model = character(),
  data_idx = numeric(),
  fold_idx = numeric(),
  k = numeric(),
  nu = numeric(),
  n_trees = numeric(),
  n_samples = numeric(),
  mse = numeric()
)

dgps <- c(29)
set.seed(123)

for (i in dgps) {
  dgp_index <- cont_dgpis$DGPid[i]
  true_ate <- cont_dgpis$trueATE[i]
  datasets_dgp <- all_datasets %>% filter(DGPid == dgp_index)
  
  n <- 10
  results_list <- vector("list", n)
  
  for (i in 1:n) {
    results_list[[i]] <- data.frame(
      id = i,
      value = rnorm(5)
    )
  }
  
  for (j in 1:10) { #Datensätze
    file_name <- datasets_dgp[j, "filename"]
    data <- read.csv(paste0(PATH_DATASETS, file_name, ".csv"))
    
    dataset_folds <- 
    
    # In training und test data set splitten
    # TODO: vlt mache ich das doch nicht? Mal sehen
    #train_indices <- sample(nrow(data), nrow(data)*0.7, replace = FALSE) #TODO: Prozent bestimmen
    #tuning_data <- data[train_indices,]
    #test_data <- data[-train_indices,]
    
    tuning_data <- data
    
    # Split dataset into 10 folds
    #k <- 10 #TODO: Größe der Datensätze checken, evtl runter, wenn folds dann sonst zu klein werden 
    #fold_ids <- rep(NA, nrow(tuning_data))
    
    #for (treatment in 0:1) {
      #idx <- which(tuning_data$A == treatment)
     # fold_ids[idx] <- sample(rep(1:k, length.out = length(idx)))
    #}
    
    for (fold in 1:K){ # k-folds
      train_data <- tuning_data[fold_ids != fold,] 
      eval_data <- tuning_data[fold_ids == fold,] 
      
      for (l in 1:nrow(hyper_bart)) {
        hyperparams <- hyper_bart[l,]
        
        bart_fit <- bartc(train_data$Y, train_data$A, train_data %>% select(starts_with("V")),
                          method.rsp = "bart", method.trt = "none", n.chains = 10,
                          estimand = "ate", keepTrees = TRUE, p.scoreAsCovariate = FALSE,
                          k = hyperparams$k, n.samples = hyperparams$n_samples, n.trees = hyperparams$n_trees,
                          sigdf = hyperparams$nu)
        
        pred_mu1 <- predict(bart_fit, eval_data %>% select(starts_with("V")), type="mu.1") %>% colMeans()
        pred_mu0 <- predict(bart_fit, eval_data %>% select(starts_with("V")), type="mu.0") %>% colMeans()
        
        # MSE 
        mse <- mean((eval_data$A * (eval_data$Y - pred_mu1) + (1 - eval_data$A) * (eval_data$Y - pred_mu0))^2)
        
        all_tuning_results <- rbind(all_tuning_results, data.frame(
          dgp = paste0("dgp", i),
          metric = "mse",
          model = "bart_s-learner",
          data_idx = j,
          fold_idx = k,
          k = hyperparams$k,
          nu = hyperparams$nu,
          n_trees = hyperparams$n_trees,
          n_samples = hyperparams$n_samples,
          mse = mse
        ))
      }
    }
    
  }
}