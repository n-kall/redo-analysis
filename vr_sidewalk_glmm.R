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

sidewalk.data <- read.csv("vr_data/sidewalkCSV.csv")

# sidewalk
# rename gender variable
colnames(sidewalk.data)[2] <- "gender"
# rename driver variable
colnames(sidewalk.data)[3] <- "motorist"
# set participant to factor
sidewalk.data$participant.ID <- factor(sidewalk.data$participant.ID)
# rename levels for driver
sidewalk.data$motorist <- factor(sidewalk.data$motorist,
                            levels = c("False", "True"),
                            labels = c("human", "self-driving"))
# rename levels for gender
sidewalk.data$gender <- factor(sidewalk.data$gender,
                            levels = c("False", "True"),
                            labels = c("female", "male"))
# set confidence to numeric
sidewalk.data$confidence <- as.numeric(sidewalk.data$confidence)
                                        # sanity check
sidewalk.sub <- subset(sidewalk.data,
                       sidewalk.data$passedSanCheck == "True" &
                       SecondSanCheck == "True")

sidewalk.sub$participant.ID <- droplevels(sidewalk.sub$participant.ID)


sidewalk.sub$opinAV <- ordered(sidewalk.sub$opinAV,
                            levels = c("Sehr negativ",
                                       "Negativ",
                                       "Neutral",
                                       "Positiv",
                                       "Sehr positiv"))

sidewalk.sub$education <- ordered(sidewalk.sub$education,
                               levels = c("keinen Hocschulabschluss",
                                          "Hochschulabschluss (Bachelor, Vordiplom,etc.)",
                                          "Hochschulabschluss (Master, Diplom, oder höher)"))



sidewalk.sub$drivExperience <- ordered(sidewalk.sub$drivExperience,
                                    levels = c("Ich fahre nicht Auto",
                                               "1-5 Jahre",
                                               "5-10",
                                               "10+"))





sidewalk.sub$perspective <- factor(sidewalk.sub$perspective,
                                      levels = c("Observer",
                                                 "Passenger",
                                                 "PedSmall",
                                                 "PedLarge"),
                                      labels = c("Bystander",
                                                 "Passenger",
                                                 "PedSmall",
                                                 "PedLarge"))

sidewalk.sub$age_c <- scale(sidewalk.sub$age)

sidewalk.sub$perceivedIden <- factor(sidewalk.sub$perceivedIden,
                                   levels = c("Beobachter",
                                              "Mitfahrer",
                                              "Fußgänger"),
                                   labels = c("Bystander",
                                              "Passenger",
                                              "Pedestrian"))


(nc <- detectCores())
cl <- makeCluster(rep("localhost", nc))




sidewalk_glmm_identify_base <- mixed(decision ~ perceivedIden + motorist +
                              perceivedIden:motorist +
                              (1 | participant.ID),
                          method = "LRT",
                          family = "binomial", data = sidewalk.sub,
                          args_test = list(nsim = 1000, cl = cl), cl = cl,
                          control = glmerControl(optimizer = "bobyqa",
                                                 optCtrl = list(maxfun = 2e5)))

sidewalk_glmm_identify_cov <- mixed(decision ~ perceivedIden + motorist +
                        perceivedIden:motorist + trial +
                        gender + age_c + opinAV +
                        education +  drivExperience + visImpairment +
                        (1 | participant.ID),
                    method = "LRT", # change to PB for final
                    family = "binomial", data = sidewalk.sub,
                    args_test = list(nsim = 1000, cl = cl), cl = cl,
                    control = glmerControl(optimizer = "bobyqa",
                                           optCtrl = list(maxfun = 2e5)))


sidewalk_glmm_base <- mixed(decision ~ perspective + motorist +
                                perspective:motorist +
                                (1 | participant.ID),
                            method = "LRT",
                            family = "binomial", data = sidewalk.sub,
                            args_test = list(nsim = 1000, cl = cl), cl = cl,
                            control = glmerControl(optimizer = "bobyqa",
                                                   optCtrl = list(maxfun = 2e5)))


sidewalk_glmm_cov <- mixed(decision ~ perspective + motorist +
                        perspective:motorist + trial +
                        gender + age_c + opinAV +
                        education +  drivExperience + visImpairment +
                        (1 | participant.ID),
                    method = "LRT", # change to PB for final
                    family = "binomial", data = sidewalk.sub,
                    args_test = list(nsim = 1000, cl = cl), cl = cl,
                    control = glmerControl(optimizer = "bobyqa",
                                           optCtrl = list(maxfun = 2e5)))

stopCluster(cl)


sidewalk_glmm.resid <- simulateResiduals(
    fittedModel = sidewalk_glmm_cov$full_model, n = 2000)
sidewalk_glmm_resid.plot <- plotSimulatedResiduals(
    simulationOutput = sidewalk_glmm.resid)


emm_sidewalk_i <- emmeans(sidewalk_glmm_cov, pairwise ~ perspective | motorist,
                          type = 'response')

emm_sidewalk_persp <- emmeans(sidewalk_glmm_cov, pairwise ~ perspective,
                              type = 'response')


emm_sidewalk_motorist <- emmeans(sidewalk_glmm_cov, pairwise ~ motorist,
                                 type = 'response')

save.image(file = "vr_sidewalk_glmm_redo.RData")
