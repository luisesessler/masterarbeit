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
#all_summaries <- list()

set.seed(213)

smd_per_dgp <- data.frame(dgp = character(),
           smd_mean = numeric(),
          smd_max = numeric(),
          smd_too_big = numeric())

for(i in 2){
  # true_ate <- cont_dgpis$trueATE[i]
  
  # results <- make_results_list()
  dgp_index <- cont_dgpis$DGPid[i]
  datasets_dgp <- all_datasets %>% filter(DGPid == dgp_index)
  
  #for (j in 1:nrow(datasets_dgp)){
  for (j in 1:1){
    file_name <- datasets_dgp[j, "filename"]
    data <- read.csv(paste0(PATH_DATASETS, file_name, ".csv"))
    y <- data$Y
    z <- data$A
    X <- data %>% select(starts_with("V"))
    X_mm <- model.matrix(~ . - 1, data = X)
    data_ps_mm <- data.frame(X_mm, z)
    
    formula_rightside <- colnames(X) %>% paste0(collapse = " + ")
    
    ps_formula <- as.formula(paste0("z ~ ", formula_rightside))
    outcome_formula <- as.formula(paste0("y ~ z + ", formula_rightside))

    data <- data.frame(X, z, y)
    # Calculate Propensity scores
    
    # GLM PS
    ps_fit_glm <- glm(ps_formula, data = data, family = binomial)
    ps_raw_glm <- predict(ps_fit_glm, type = "response")
    
    df_plot_glm <- data.frame(
      ps_raw_glm = ps_raw_glm,
      z = factor(z)
    )
    
    ggplot(df_plot_glm, aes(x = ps_raw_glm, color = z, fill = z)) +
      geom_density(alpha = 0.3) +
      labs(
        title = "Propensity Score Density (BART)",
        x = "Propensity Score"
      ) +
      theme_minimal()
    
    
    # BART PS
    ps_fit_bart <- bart2(formula = ps_formula, data = data_ps_mm, keepTrees = TRUE, combineChains = TRUE,
                    n.trees = 200, n.burn = 1000, n.samples = 1000, n.chains = 4)
    ps_raw_bart <- predict(ps_fit_bart, data_ps_mm, type = "ev") %>% colMeans()    
    
    df_plot_bart <- data.frame(
      ps_raw_bart = ps_raw_bart,
      z = factor(z)
    )
    
    ggplot(df_plot_bart, aes(x = ps_raw_bart, color = z, fill = z)) +
      geom_density(alpha = 0.3) +
      labs(
        title = "Propensity Score Density (BART)",
        x = "Propensity Score"
      ) +
      theme_minimal()
    
    
    
    # Clip extreme PS
    eps <- 0.01
    ps_bart <- pmin(pmax(ps_raw_bart, eps), 1 - eps)
    ps_glm <- pmin(pmax(ps_raw_bart, eps), 1 - eps)
    

    # Stabilized ATE weights
    p_treat <- mean(z)
    weights <- ifelse(z == 1,
                      p_treat / ps,
                      (1 - p_treat) / (1 - ps))
    
    # Check balance
    balance_measures <- bal.tab(ps_formula, 
    data = data, 
    weights = weights,  
    method = "weighting")
    print(balance_measures)
    
    # Compute standardized mean differences
    smd_mean_weighting <- mean(balance_measures$Balance$Diff.Adj)
    smd_max_weighting <- max(balance_measures$Balance$Diff.Adj)
    smd_too_big_weighting <- sum(abs(balance_measures$Balance$Diff.Adj) >= 0.1)
    
    ate_ipw <- mean(weights * z * y) / mean(weights * z) -
      mean(weights * (1 - z) * y) / mean(weights * (1 - z))
    
    print(ate_ipw)
    
    # smd_per_dgp <- rbind(smd_per_dgp, c(dgp_index, smd_mean_weighting, smd_max_weighting, smd_too_big_weighting))
    # love.plot(bal.tab)
  }
}


sample_y1_mean <- data %>% filter(z == 1) %>%
  pull(y) %>% mean()
  
sample_y0_mean <- data %>% filter(z == 0) %>%
  pull(y) %>% mean()

sample_ate <- sample_y1_mean - sample_y0_mean