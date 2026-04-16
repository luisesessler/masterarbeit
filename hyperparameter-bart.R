hyper_bart <- expand.grid(
  k = c(1, 2, 3),
  nu = c(3, 7, 10),
  n_trees = c(75, 150, 200),
  n_samples = c(500, 1000, 2000)
)

# Create results dataframe
all_results <- data.frame(
  dgp = character(),
  metric = character(),
  model = character(),
  iteration = numeric(),
  k = numeric(),
  nu = numeric(),
  n_trees = numeric(),
  n_samples = numeric(),
  ate_estimate = numeric(),
  ate_bias = numeric(),
  ci_length = numeric(),
  coverage = numeric()
)

dgps <- c(30)
set.seed(123)

for (i in 1:nrow(dgps)) {
  dgp_index <- cont_dgpis$DGPid[i]
  true_ate <- cont_dgpis$trueATE[i]
  datasets_dgp <- all_datasets %>% filter(DGPid == dgp_index)
  
  for (j in 1:10) {
    file_name <- datasets_dgp[j, "filename"]
    data <- read.csv(paste0(PATH_DATASETS, file_name, ".csv"))
    y <- data$Y
    z <- data$A
    X <- data %>% select(starts_with("V"))
    
    # In training und test data set splitten
    # TODO: vlt mache ich das doch nicht? Mal sehen
    train_indices <- sample(nrow(data), nrow(data)*0.7, replace = FALSE) #TODO: Prozent bestimmen
    train <- data[train_indices,]
    test <- data[-train_indices,]
    
    # Split dataset into 10 folds
    
    
    
    for (l in 1:nrow(hyper_bart)) {
      hyperparams <- hyper_bart[, l]
      
      
      
      
      
      
      
      
      
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