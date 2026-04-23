# Uni-Computer
PATH_DATASETS <- "W:/Masterarbeit/Daten/"
PATH_TRUE_ATES <- "W:/Masterarbeit/Daten/trueATE/lowDim_trueATE.csv"
PATH_OVERVIEW <- "W:/Masterarbeit/Daten/trueATE/cont_dgp_overview.csv"
PATH_RESULTS <- "W:/Masterarbeit/Results/"


# DGP 60

dgp60_s_learner <- read.csv(paste0(PATH_RESULTS, "2026-04-22_tuning_NO-CV_dgp60_s-learner_NEW.csv")) 
dgp60_t_learner <- read.csv(paste0(PATH_RESULTS, "2026-04-23_tuning_NO-CV_dgp60_t-learner.csv"))
dgp60_ps_bart <- read.csv(paste0(PATH_RESULTS, "2026-04-21_tuning_NO-CV_dgp60_ps-bart.csv")) 
dgp60_ps_glm <- read.csv(paste0(PATH_RESULTS, "2026-04-21_tuning_NO-CV_dgp60_ps-glm.csv")) 


# DGP 30

dgp30_s_learner <- read.csv(paste0(PATH_RESULTS, "2026-04-22_tuning_NO-CV_dgp30_s-learner.csv")) 
dgp30_t_learner <- read.csv(paste0(PATH_RESULTS, "2026-04-23_tuning_NO-CV_dgp30_t-learner.csv"))
dgp30_ps_bart <- read.csv(paste0(PATH_RESULTS, "2026-04-23_tuning_NO-CV_dgp30_ps-bart.csv")) 
dgp30_ps_glm <- read.csv(paste0(PATH_RESULTS, "2026-04-23_tuning_NO-CV_dgp30_ps-glm.csv")) 

dgp_30_bart_models <- rbind(dgp30_s_learner, dgp30_t_learner ,dgp30_ps_bart, dgp_30_ps_glm)

write.csv(dgp_30_bart_models, paste0(PATH_RESULTS, "2026-04-23_tuning_NO-CV_dgp30_bart-models.csv"), row.names =  FALSE)
