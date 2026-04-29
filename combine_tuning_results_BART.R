tuning_dgp30_s_1_10 <- read.csv(paste0(PATH_RESULTS, "2026-04-28_tuning_NO-CV_dgp30_s-learner_sigma_1-10.csv"))
tuning_dgp30_s_11_20 <- read.csv(paste0(PATH_RESULTS, "2026-04-29_tuning_NO-CV_dgp30_s-learner_sigma_11-20.csv"))

tuning_dgp30_t_1_10 <- read.csv(paste0(PATH_RESULTS, "2026-04-28_tuning_NO-CV_dgp30_t-learner_sigma_1-10.csv"))
tuning_dgp30_t_11_20 <- read.csv(paste0(PATH_RESULTS, "2026-04-29_tuning_NO-CV_dgp30_t-learner_sigma_11-20.csv"))

tuning_dgp30_psGLM_1_10 <- read.csv(paste0(PATH_RESULTS, "2026-04-28_tuning_NO-CV_dgp30_ps-glm_sigma_1-10.csv"))
tuning_dgp30_psGLM_11_20 <- read.csv(paste0(PATH_RESULTS, "2026-04-29_tuning_NO-CV_dgp30_ps-glm_sigma_11-20.csv"))

tuning_dgp30_psBART_1_5 <- read.csv(paste0(PATH_RESULTS, "2026-04-28_tuning_NO-CV_dgp30_ps-bart_sigma_1-5.csv"))
tuning_dgp30_psBART_6_10 <- read.csv(paste0(PATH_RESULTS, "2026-04-29_tuning_NO-CV_dgp30_ps-bart_sigma_6-10.csv"))
tuning_dgp30_psBART_11_15 <- read.csv(paste0(PATH_RESULTS, "2026-04-29_tuning_NO-CV_dgp30_ps-bart_sigma_11-15.csv"))
tuning_dgp30_psBART_16_20 <- read.csv(paste0(PATH_RESULTS, "2026-04-29_tuning_NO-CV_dgp30_ps-bart_sigma_16-20.csv"))

old_results <- read.csv(paste0(PATH_RESULTS, "2026-04-28_tuning_NO-CV_dgp30_bart-models_sigma.csv"))

tuning_dgp30_bart_complete <- bind_rows(
  tuning_dgp30_s_1_10,
  tuning_dgp30_s_11_20,
  
  tuning_dgp30_t_1_10,
  tuning_dgp30_t_11_20,
  
  tuning_dgp30_psGLM_1_10,
  tuning_dgp30_psGLM_11_20,
  
  tuning_dgp30_psBART_1_5,
  tuning_dgp30_psBART_6_10,
  tuning_dgp30_psBART_11_15,
  tuning_dgp30_psBART_16_20,
  
  old_results
)

write.csv(tuning_dgp30_bart_complete, paste0(PATH_RESULTS, "2026-04-29_tuning_NO-CV_dgp30_ALL_bart-models.csv"), row.names = FALSE)

# DGP 63

tuning_dgp63_s <- read.csv(paste0(PATH_RESULTS, "2026-04-29_tuning_NO-CV_dgp63_s-learner.csv"))
tuning_dgp63_t <- read.csv(paste0(PATH_RESULTS, "2026-04-29_tuning_NO-CV_dgp63_t-learner.csv"))
tuning_dgp63_psGLM <- read.csv(paste0(PATH_RESULTS, "2026-04-29_tuning_NO-CV_dgp63_ps-glm.csv"))
tuning_dgp63_psBART_1 <- read.csv(paste0(PATH_RESULTS, "2026-04-29_tuning_NO-CV_dgp63_ps-bart_1-10.csv"))
tuning_dgp63_psBART_2 <- read.csv(paste0(PATH_RESULTS, "2026-04-29_tuning_NO-CV_dgp63_ps-bart_11-20.csv"))


tuning_dgp63_bart_complete <- bind_rows(
  tuning_dgp63_s,
  tuning_dgp63_t,
  tuning_dgp63_psGLM,
  tuning_dgp63_psBART_1,
  tuning_dgp63_psBART_2,
)

write.csv(tuning_dgp63_bart_complete, paste0(PATH_RESULTS, "2026-04-29_tuning_NO-CV_dgp63_ALL_bart-models.csv"), row.names = FALSE)

# DGP 40

tuning_dgp40_s <- read.csv(paste0(PATH_RESULTS, "2026-04-29_tuning_NO-CV_dgp40_s-learner.csv"))
tuning_dgp40_t <- read.csv(paste0(PATH_RESULTS, "2026-04-29_tuning_NO-CV_dgp40_t-learner.csv"))
tuning_dgp40_psGLM <- read.csv(paste0(PATH_RESULTS, "2026-04-29_tuning_NO-CV_dgp40_ps-glm.csv"))
tuning_dgp40_psBART_1 <- read.csv(paste0(PATH_RESULTS, "2026-04-29_tuning_NO-CV_dgp40_ps-bart_1-10.csv"))
tuning_dgp40_psBART_2 <- read.csv(paste0(PATH_RESULTS, "2026-04-29_tuning_NO-CV_dgp40_ps-bart_11-20.csv"))

tuning_dgp40_bart_complete <- bind_rows(
  tuning_dgp40_s,
  tuning_dgp40_t,
  tuning_dgp40_psGLM,
  tuning_dgp40_psBART_1,
  tuning_dgp40_psBART_2
)

write.csv(tuning_dgp40_bart_complete, paste0(PATH_RESULTS, "2026-04-29_tuning_NO-CV_dgp40_ALL_bart-models.csv"), row.names = FALSE)
