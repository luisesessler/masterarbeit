hyper_bart <- expand.grid(
  k = c(1, 2, 3),
  nu = c(3, 7, 10),
  n_trees = c(75, 150, 200),
  n_samples = c(500, 1000, 2000)
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

dgps <- c(30)
set.seed(123)

for (i in 1:nrow(dgps)) {
  dgp_index <- cont_dgpis$DGPid[i]
  true_ate <- cont_dgpis$trueATE[i]
  datasets_dgp <- all_datasets %>% filter(DGPid == dgp_index)
  
  for (j in 1:10) { #Datensätze
    file_name <- datasets_dgp[j, "filename"]
    data <- read.csv(paste0(PATH_DATASETS, file_name, ".csv"))
    y <- data$Y
    z <- data$A
    X <- data %>% select(starts_with("V"))
    
    # In training und test data set splitten
    # TODO: vlt mache ich das doch nicht? Mal sehen
    train_indices <- sample(nrow(data), nrow(data)*0.7, replace = FALSE) #TODO: Prozent bestimmen
    tuning_data <- data[train_indices,]
    test_data <- data[-train_indices,]
    
    # Split dataset into 10 folds
    k <- 10 #TODO: Größe der Datensätze checken, evtl runter, wenn folds dann sonst zu klein werden 
    fold_ids <- rep(NA, nrow(tuning_data))
    
    for (treatment in 0:1) {
      idx <- which(data$z == treatment)
      fold_ids[idx] <- sample(rep(1:k, length.out = length(idx)))
    }
    
    for (k in 1:K){ # k-folds
      train_data <- tuning_data[fold_ids != k,] 
      eval_data <- tuning_data[fold_ids == k,] 
      
      for (l in 1:nrow(hyper_bart)) {
        hyperparams <- hyper_bart[, l]
        
        fit_bart <- bartc(train_data$y, train_data$z, train_data %>% select(starts_with("V")),
                          method.rsp = "bart", method.trt = "none",
                          estimand = "ate", keepTrees = TRUE, p.scoreAsCovariate = FALSE)
        
        pred_mu1 <- predict(bart_fit, eval_data %>% select(starts_with("V")), type="mu.1")
        pred_mu0 <- predict(bart_fit, eval_data %>% select(starts_with("V")), type="mu.0")
        
        # MSE 
        mse <- mean((eval_data$z * (eval_data$y - pred_mu1) + (1 - eval_data$z) * (eval_data$y - pred_mu0))^2)
     
        all_tuning_results <- rbind(all_tuning_results, data.frame(
          dgp = paste0("dgp", i),
          metric = mse,
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
      
      
      
      
      
      
      
      
      
      # Model evaluation:
      # Wir fitten jetzt mit den besten gewählten Hyperparametern ein Modell und berechnen dann nochmal alle Metriken wie vorher
      
      # TODO: entweder ganzen Datensatz verwenden oder eben test data
      
      results_run <- bart_s_learner(y, z, X, true_ate) # TODO: abklären, ob mit Cross-Validation oder nicht?
      
      data.frame(
        dgp = paste0("dgp", i),
        metric = character(),
        #todo
        model = "bart_s-learner",
        iteration = j,
        #todo
        k = numeric(),
        nu = numeric(),
        n_trees = numeric(),
        n_samples = numeric(),
      )
      
      all_results <-  rbind(all_results, results_run)
      
    }
    
  }
}