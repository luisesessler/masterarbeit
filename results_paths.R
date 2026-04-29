# IPTW
iptw_bart <- read.csv(paste0(PATH_RESULTS,"2026-04-23_tuning_NO-CV_weighting_bart_all_dgps.csv"))
iptw_glm <- read.csv(paste0(PATH_RESULTS,"2026-04-23_weighting_basic.csv")) %>% filter(iteration < 21)
iptw_tuning <- rbind(iptw_bart, iptw_glm)

# DGP 30

dgp30_bart_tuning <- read.csv(paste0(PATH_RESULTS, "2026-04-29_tuning_NO-CV_dgp30_ALL_bart-models.csv"))
dgp30_bart_cf <- read.csv(paste0(PATH_RESULTS, "2026-04-24_tuning_NO-CV_dgp30_causal-forest.csv"))
dgp30_bart_ipwt <- iptw_tuning %>% filter(dgp == "dgp30")

# DGP 60
dgp60_bart_tuning <- read.csv(paste0(PATH_RESULTS, "2026-04-28_tuning_NO-CV_dgp60_ALL_bart-models_sigma_complete.csv"))
tuning_results_60_cf <- read.csv(paste0(PATH_RESULTS, "2026-04-24_tuning_NO-CV_dgp60_causal-forest_.csv"))
dgp60_bart_ipwt <- iptw_tuning %>% filter(dgp == "dgp60")


# DGP 63
dgp63_bart_tuning <- read.csv(paste0(PATH_RESULTS, "2026-04-29_tuning_NO-CV_dgp63_ALL_bart-models.csv"))
dgp63_cf_tuning <- read.csv(paste0(PATH_RESULTS, "2026-04-24_tuning_NO-CV_dgp63_causal-forest.csv"))
dgp63_bart_ipwt <- iptw_tuning %>% filter(dgp == "dgp63")


# DGP 40
dgp40_bart_tuning <- read.csv(paste0(PATH_RESULTS, "2026-04-29_tuning_NO-CV_dgp40_ALL_bart-models.csv"))
dgp40_cf_tuning <- read.csv(paste0(PATH_RESULTS, "2026-04-24_tuning_NO-CV_dgp40_causal-forest.csv"))
dgp40_bart_ipwt <- iptw_tuning %>% filter(dgp == "dgp40")
