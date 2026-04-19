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

for (i in 1:nrow(cont_dgpis)){
  dgp_index <- cont_dgpis$DGPid[i]
  datasets_dgp <- all_datasets %>% filter(DGPid == dgp_index)
  file_name <- datasets_dgp[1, "filename"]
  data <- read.csv(paste0(PATH_DATASETS, file_name, ".csv"))
  y <- data$Y
  z <- data$A
  X <- data %>% select(starts_with("V"))
  
  # Fit BART model
  bart_fit <- bartc(
    response = y,
    treatment = z,
    confounders = X,
    method.rsp = "bart",
    method.trt = "none",
    estimand = "ate",
    keepTrees = TRUE,
    p.scoreAsCovariate = FALSE
  )
  
  # Posterior draws
  mu1_draws <- predict(bart_fit, X, type = "mu.1")
  mu0_draws <- predict(bart_fit, X, type = "mu.0")
  
  # Posterior SDs
  s1 <- apply(mu1_draws, 2, sd)  # SD(mu.1)
  s0 <- apply(mu0_draws, 2, sd)  # SD(mu.0)
  
  # Initialize ratio
  ratio <- rep(NA, length(z))
  
  # For treated units: counterfactual = mu.0, factual = mu.1
  ratio[z == 1] <- (s0[z == 1]^2) / (s1[z == 1]^2)
  
  # For control units: counterfactual = mu.1, factual = mu.0
  ratio[z == 0] <- (s1[z == 0]^2) / (s0[z == 0]^2)
  
  # --- Apply thresholds ---
  
  # 0.10 rule (chi-square cutoff ≈ 2.706)
  discard_10 <- ratio > 2.706
  
  # 0.05 rule (chi-square cutoff ≈ 3.841)
  discard_05 <- ratio > 3.841
  
  # Max approach
  # --- Compute thresholds ---
  
  # Factual SDs within groups
  s1_treated <- s1[z == 1]  # observed for treated
  s0_control <- s0[z == 0]  # observed for control
  
  # Thresholds: max + buffer (SD of SDs)
  threshold_treated <- max(s1_treated) + sd(s1_treated)
  threshold_control <- max(s0_control) + sd(s0_control)
  
  # --- Apply discard rules ---
  
  # Initialize
  discard_treated <- rep(FALSE, length(z))
  discard_control <- rep(FALSE, length(z))
  
  # For treated units: use counterfactual SD = s0
  discard_treated[z == 1] <- s0[z == 1] > threshold_treated
  
  # For control units: use counterfactual SD = s1
  discard_control[z == 0] <- s1[z == 0] > threshold_control
  
  # Combine (units to discard)
  discard <- discard_treated | discard_control
  
  
  overlap_bart <- rbind(overlap_bart, data.frame(dpg = dgp_index, 
                                                 iteration = 1, 
                                                 discarded_ratio10 = sum(discard_10), 
                                                 discarded_ratio5 = sum(discard_05), 
                                                 discarded_max = sum(discard)))
}

write.csv(overlap_bart, paste0(PATH_RESULTS, "2026-04-16_overlap_diagnostics_BART.csv"))