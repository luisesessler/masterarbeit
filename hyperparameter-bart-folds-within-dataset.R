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
  mse = numeric(),
  test_ate_estimate = numeric(),
  test_ate_bias = numeric()
)

dgps <- c(29)
set.seed(123)

for (i in 1) {
  dgp_index <- cont_dgpis$DGPid[i]
  true_ate <- cont_dgpis$trueATE[i]
  datasets_dgp <- all_datasets %>% filter(DGPid == dgp_index)
  
  for (j in 1:1) { #Datensätze
    file_name <- datasets_dgp[j, "filename"]
    data <- read.csv(paste0(PATH_DATASETS, file_name, ".csv"))

    # In training und test data set splitten
    # TODO: vlt mache ich das doch nicht? Mal sehen
    train_indices <- sample(nrow(data), nrow(data)*0.7, replace = FALSE) #TODO: Prozent bestimmen
    train_data <- data[train_indices,]
    test_data <- data[-train_indices,]
    
    
    # Split tuning dataset into 10 folds
    k <- 10 #TODO: Größe der Datensätze checken, evtl runter, wenn folds dann sonst zu klein werden 
    fold_ids <- rep(NA, nrow(train_data))
    
    for (treatment in 0:1) {
      idx <- which(train_data$A == treatment)
      fold_ids[idx] <- sample(rep(1:k, length.out = length(idx)))
    }
    
    for (l in 1:1) {
      hyperparams <- hyper_bart[l,]
      mses <- numeric(k)
    
      for (fold in 1:k){ # k-folds
        tuning_data <- train_data[fold_ids != fold,] 
        eval_data <- train_data[fold_ids == fold,] 
        
        bart_fit <- bartc(tuning_data$Y, tuning_data$A, tuning_data %>% select(starts_with("V")),
                          method.rsp = "bart", method.trt = "none", n.chains = 10,
                          estimand = "ate", keepTrees = TRUE, p.scoreAsCovariate = FALSE,
                          k = hyperparams$k, n.samples = hyperparams$n_samples, n.trees = hyperparams$n_trees,
                          sigdf = hyperparams$nu)
        
        pred_mu1 <- predict(bart_fit, eval_data %>% select(starts_with("V")), type="mu.1") %>% colMeans()
        pred_mu0 <- predict(bart_fit, eval_data %>% select(starts_with("V")), type="mu.0") %>% colMeans()
        
        # MSE 
        mses[fold] <- mean((eval_data$A * (eval_data$Y - pred_mu1) + (1 - eval_data$A) * (eval_data$Y - pred_mu0))^2)
        
      }
      mean_mse_all_folds <- mean(mses)
    
        
        # Fit BART model to all the training data
        bart_fit <- bartc(train_data$Y, train_data$A, train_data %>% select(starts_with("V")),
                          method.rsp = "bart", method.trt = "none", n.chains = 10,
                          estimand = "ate", keepTrees = TRUE, p.scoreAsCovariate = FALSE,
                          k = hyperparams$k, n.samples = hyperparams$n_samples, n.trees = hyperparams$n_trees,
                          sigdf = hyperparams$nu)
        
        # Test & calculate bias/coverage/etc
        pred_mu1 <- predict(bart_fit, test_data %>% select(starts_with("V")), type="mu.1") %>% colMeans()
        pred_mu0 <- predict(bart_fit, test_data %>% select(starts_with("V")), type="mu.0") %>% colMeans()
        pred_ite <- pred_mu1 - pred_mu0
        
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
          mse = mean_mse_all_folds,
          test_ate_estimate = mean(pred_ite),
          test_ate_bias = mean(pred_ite) - true_ate
        ))
      }
    }

  }
