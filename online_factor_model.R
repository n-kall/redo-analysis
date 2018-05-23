our_seed <- 123
set.seed(our_seed)

require(knitr)
require(lme4)
require(afex)
library(memisc)
require(cowplot)
require(DHARMa)
require(xtable)
require(rockchalk)
require(tikzDevice)
require(parallel)
require(fifer)
require(multcomp)
require(vcdExtra)
require(catspec)
require(data.table)


data <- read.csv("online_accept_data/raw/online_accept_data_clean.csv", fileEncoding="UTF16")


setnames(data, "exp_subject_id", "participant")
setnames(data, "PERSP", "perspective")
setnames(data, "DRIVER_TYPE", "motorist")
setnames(data, "first_vid", "first_anim")

data$perspective[ data$perspective == "" ] <- NA

data$perspective <- droplevels(data$perspective)

levels(data$perspective) <- c("car", "observer",
                              "pedestrian_fwd", "pedestrian_side")

data$motorist[ data$motorist == "" ] <- NA

data$motorist <- droplevels(data$motorist)


levels(data$motorist) <- c("self-driving", "human")

# ensure correct data types
data$first_anim <- factor(data$first_anim)
data$scenario <- factor(data$scenario)
data$participant <- factor(data$participant)
data$response <- factor(data$response)
data$perspective <- factor(data$perspective)
data$motorist <- factor(data$motorist)
data$country <- factor(data$country)
data$better_place <- ordered(data$better_place)
data$driving_experience <- ordered(data$driving_experience,
                                   levels = c("0",
                                              "5",
                                              "6-10",
                                              "10+"))
data$age_group <- ordered(data$age_group)

questions.sub <- subset(data, grepl("ques", data$Block_Name) & data$scenario == "")

questions.sub[ questions.sub == "" ] <- NA


questions.sub <- subset(questions.sub, select = c("participant",
                                                  "gender",
                                                  "knowledge_of_av",
                                                  "identify",
                                                  "driving_experience",
                                                  "age_group",
                                                  "better_place",
                                                  "country"))



data.clean <- subset(data, data$ratio != "" & data$scenario != "")

data.clean <- subset(data.clean, select = c("participant",
                                            "perspective", "motorist",
                                            "scenario", "ratio",
                                            "response", "first_anim"))


data.clean <- droplevels(data.clean)

data.use <- merge(questions.sub, data.clean)
data.use[ data.use == "" ] <- NA
data.use <- droplevels(data.use)



carped.sub <- subset(data, scenario == "carsac")
carped.sub$ratio_f <- ordered(carped.sub$ratio)
carped.sub$ratio_c <- as.numeric(carped.sub$ratio)
carped.sub <- droplevels(carped.sub)

carped.sub$perspective <- combineLevels(carped.sub$perspective,
                                        levs = c("pedestrian_fwd"),
                                        newLabel = c("pedestrian"))


pedped.sub <- subset(data, (scenario == "road" | scenario == "sidewalk"))


pedped.sub$ratio_f <- ordered(pedped.sub$ratio)
pedped.sub$ratio_c<- as.numeric(pedped.sub$ratio)

pedped.sub <- droplevels(pedped.sub)


(nc <- detectCores())
cl <- makeCluster(rep("localhost", nc))

carped_cov_fac_glmm <- mixed(response ~ ratio_f * perspective * motorist +
                          first_anim +
                          better_place +
                          knowledge_of_av +
                         (1 | participant),
                     family = "binomial", data = carped.sub,
                     method = "PB",
                     args_test = list(nsim = 1000, cl = cl), cl = cl,
                     control = glmerControl(optimizer = "bobyqa",
                                            optCtrl = list(maxfun = 2e5)))



carped_base_fac_iden_base <- mixed(response ~ ratio_f * identify * motorist +
                          first_anim +
                          better_place +
                          knowledge_of_av +
                         (1 | participant),
                     family = "binomial", data = carped.sub,
                     method = "PB",
                     args_test = list(nsim = 1000, cl = cl), cl = cl,
                     control = glmerControl(optimizer = "bobyqa",
                                            optCtrl = list(maxfun = 2e5)))



carped_cov_fac_iden_glmm <- mixed(response ~ ratio_f * identify * motorist +
                          first_anim +
                          better_place +
                          knowledge_of_av +
                         (1 | participant),
                     family = "binomial", data = carped.sub,
                     method = "PB", # change this to PB for final?
                     args_test = list(nsim = 1000, cl = cl), cl = cl,
                     control = glmerControl(optimizer = "bobyqa",
                                            optCtrl = list(maxfun = 2e5)))

save.image('online_study_carped_redo.RData')


#carped_ratio_followup <- emmeans(carped_cov_fac_glmm, pairwise ~ ratio_f,  type = "response")


pedped_cov_fac_glmm <- mixed(response ~ ratio_f * perspective * motorist +
                         ratio_f * scenario * motorist + first_anim +
                         better_place + knowledge_of_av +
                         (1 + ratio_f + scenario | participant),
                     family = "binomial", data = pedped.sub,
                     method = "LRT", # change this to PB for final?
                     args_test = list(nsim = 1000, cl = cl), cl = cl,
                     control = glmerControl(optimizer = "bobyqa",
                                            optCtrl = list(maxfun = 2e5)))


pedped_base_fac_iden_glmm <- mixed(response ~ ratio_f * identify * motorist +
                         ratio_f * scenario * motorist +
                         (1 + ratio_f + scenario | participant),
                     family = "binomial", data = pedped.sub,
                     method = "LRT", # change this to PB for final?
                     args_test = list(nsim = 1000, cl = cl), cl = cl,
                     control = glmerControl(optimizer = "bobyqa",
                                            optCtrl = list(maxfun = 2e5)))

pedped_cov_fac_iden_glmm <- mixed(response ~ ratio_f * identify * motorist +
                         ratio_f * scenario * motorist + first_anim +
                         better_place + knowledge_of_av +
                         (1 + ratio_f + scenario | participant),
                     family = "binomial", data = pedped.sub,
                     method = "LRT", # change this to PB for final?
                     args_test = list(nsim = 1000, cl = cl), cl = cl,
                     control = glmerControl(optimizer = "bobyqa",
                                            optCtrl = list(maxfun = 2e5)))




stopCluster(cl)


save.image('online_study_redo.RData')
