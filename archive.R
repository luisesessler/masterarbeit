# Archiv

list_names <- c("BART_one_model", "BART_two_models", "BART_ps_BART", 
                "BART_ps_glm", "causal_forest", "matching", "weighting")

n_metrics <- length(list_names)
template <- list()

pehe_results_train <- replicate(n_metrics, template, simplify = FALSE)
ate_bias_results_train <- replicate(n_metrics, template, simplify = FALSE)
ci_length_results_train <- replicate(n_metrics, template, simplify = FALSE)
coverage_results_train <- replicate(n_metrics, template, simplify = FALSE)

pehe_results_test <- replicate(n_metrics, template, simplify = FALSE)
ate_bias_results_test <- replicate(n_metrics, template, simplify = FALSE)
ci_length_results_test <- replicate(n_metrics, template, simplify = FALSE)
coverage_results_test <- replicate(n_metrics, template, simplify = FALSE)

names(pehe_results_train) <- list_names
names(ate_bias_results_train) <- list_names
names(ci_length_results_train) <- list_names
names(coverage_results_train) <- list_names

names(pehe_results_test) <- list_names
names(ate_bias_results_test) <- list_names
names(ci_length_results_test) <- list_names
names(coverage_results_test) <- list_names

results_train<- list(pehe_results_train = pehe_results_train, 
                     ate_bias_results_train = ate_bias_results_train, 
                     ci_length_results_train = ci_length_results_train, 
                     coverage_results_train = coverage_results_train)
results_test <- list(pehe_results_test = pehe_results_test, 
                     ate_bias_results_test = ate_bias_results_test, 
                     ci_length_results_test = ci_length_results_test, 
                     coverage_results_test = coverage_results_test)