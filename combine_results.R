# Uni-Computer
PATH_DATASETS <- "W:/Masterarbeit/Daten/"
PATH_TRUE_ATES <- "W:/Masterarbeit/Daten/trueATE/lowDim_trueATE.csv"
PATH_OVERVIEW <- "W:/Masterarbeit/Daten/trueATE/cont_dgp_overview.csv"
PATH_RESULTS <- "W:/Masterarbeit/Results/"

# S-Learner

dgp30_s_learner <- read.csv(paste0(PATH_RESULTS, "2026-04-21_tuning_NO-CV_dgp30_s-learner_20.csv")) %>% filter(dgp == "dgp30")

dgp30_s_learner_no_value <- dgp30_s_learner[,2:7] 

dgp_30_s_nodupes <- dgp30_s_learner[!duplicated(dgp30_s_learner_no_value),2:8]


write.csv(dgp_30_s_nodupes, paste0(PATH_RESULTS, "2026-04-22_tuning_NO-CV_dgp30_s-learner.csv"), row.names =  FALSE)

# PS GLM

dgp30_ps_glm1 <- read.csv(paste0(PATH_RESULTS, "2026-04-21_tuning_NO-CV_dgp30_ps-glm_1-10.csv")) %>% filter(dgp == "dgp30")
dgp30_ps_glm2 <- read.csv(paste0(PATH_RESULTS, "2026-04-21_tuning_NO-CV_dgp30_ps-glm_11-20.csv")) %>% filter(dgp == "dgp30")

dgp_30_ps_glm <- rbind(dgp30_ps_glm1, dgp30_ps_glm2)[,2:8]

write.csv(dgp_30_ps_glm, paste0(PATH_RESULTS, "2026-04-22_tuning_NO-CV_dgp30_ps-glm.csv"), row.names =  FALSE)


# PS-BART


dgp30_ps_bart_bis18 <- read.csv(paste0(PATH_RESULTS, "2026-04-21_tuning_NO-CV_dgp30_ps-bart_18.csv")) %>% filter(dgp == "dgp30")
# Checken, bis wo die geht 1-5, 11-18
table(dgp30_ps_bart_bis18$iteration)

dgp30_ps_bart_6_10 <- read.csv(paste0(PATH_RESULTS, "2026-04-21_tuning_NO-CV_dgp30_ps-bart_6-10.csv")) %>% filter(dgp == "dgp30") %>%
  filter(iteration %in% c(6,7,8,9,10)) %>%
  filter(model == "bart_ps-bart") 

table(dgp30_ps_bart_6_10$iteration)
duplicated(dgp30_ps_bart_6_10[,2:7]) %>% sum()


# Dumm, falscher Name!!
dgp30_ps_bart1920 <- read.csv(paste0(PATH_RESULTS, "2026-04-21_tuning_NO-CV_dgp60_s-learner.csv")) %>% filter(dgp == "dgp30")

dgp30_ps_bart <- rbind(dgp30_ps_bart_bis18, dgp30_ps_bart_6_10, dgp30_ps_bart1920)[,2:8]

table(dgp30_ps_bart)

write.csv(dgp_30_ps_glm, paste0(PATH_RESULTS, "2026-04-22_tuning_NO-CV_dgp30_ps-glm.csv"), row.names =  FALSE)



# T Learner

dgp30_t_bis19 <- read.csv(paste0(PATH_RESULTS, "2026-04-21_tuning_NO-CV_dgp30_t-learner_19.csv"))
dgp30_t_bis19_missing <- read.csv(paste0(PATH_RESULTS, "2026-04-23_tuning_NO-CV_dgp30_t-learner_19_9-12.csv"))
dgp30_t_bis19_complete <- rbind(dgp30_t_bis19, dgp30_t_bis19_missing)
table(dgp30_t_bis19_complete$metric)

dgp30_t_bis19_complete %>% filter(iteration == 20)

table(dgp30_t_bis19$iteration)


dgp_30_t_learner <- dgp30_t_bis19_complete[,2:8] 

write.csv(dgp_30_t_learner, paste0(PATH_RESULTS, "2026-04-23_tuning_NO-CV_dgp30_t-learner.csv"), row.names =  FALSE)



