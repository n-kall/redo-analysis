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

child.data <- read.csv("vr_data/childrenCSV.csv")


# rename gender variable
colnames(child.data)[2] <- "gender"
# rename driver variable
colnames(child.data)[3] <- "motorist"
# rename levels for driver
child.data$motorist <- factor(child.data$motorist,
                            levels = c("False", "True"),
                            labels = c("human", "self-driving"))
# set participant to factor
child.data$participant.ID <- factor(child.data$participant.ID)
# set confidence to numeric
child.data$confidence <- as.numeric(child.data$confidence)
# rename levels for gender
child.data$gender <- factor(child.data$gender,
                            levels = c("False", "True"),
                            labels = c("female", "male"))
# remove sanity check and perception check failures
child.sub <- subset(child.data, child.data$passedSanCheck == "True" &
                                child.data$SecondSanCheck == "True")

child.sub$participant.ID <- droplevels(child.sub$participant.ID)


# reorder perspective levels
child.sub$perspective <- factor(child.sub$perspective,
                                levels = c("Observer",
                                           "Passenger",
                                           "PedSmall",
                                           "PedLarge"),
                                labels = c("Bystander",
                                           "Passenger",
                                           "PedSmall",
                                           "PedLarge"))

child.sub$age_c <- scale(child.sub$age)

                                        # set variables to correct type


child.sub$opinAV <- ordered(child.sub$opinAV,
                            levels = c("Sehr negativ",
                                       "Negativ",
                                       "Neutral",
                                       "Positiv",
                                       "Sehr positiv"))

child.sub$education <- ordered(child.sub$education,
                               levels = c("keinen Hocschulabschluss",
                                          "Hochschulabschluss (Bachelor, Vordiplom,etc.)",
                                          "Hochschulabschluss (Master, Diplom, oder höher)"))



child.sub$drivExperience <- ordered(child.sub$drivExperience,
                                    levels = c("Ich fahre nicht Auto",
                                               "1-5 Jahre",
                                               "5-10",
                                               "10+"))

child.sub$perceivedIden <- factor(child.sub$perceivedIden,
                                   levels = c("Beobachter",
                                              "Mitfahrer",
                                              "Fußgänger"),
                                   labels = c("Bystander",
                                              "Passenger",
                                              "Pedestrian"))


(nc <- detectCores())
cl <- makeCluster(rep("localhost", nc))

## child_glmm_base<- mixed(decision ~ perspective + motorist +
##                         perspective:motorist + trial +
##                         (1 | participant.ID),
##                     method = "PB", # change to PB for final
##                     family = "binomial", data = child.sub,
##                     args_test = list(nsim = 1000, cl = cl), cl = cl,
##                     control = glmerControl(optimizer = "bobyqa",
##                                            optCtrl = list(maxfun = 2e5)))


child_glmm_cov <- mixed(decision ~ perspective + motorist +
                        perspective:motorist +
                        trial + gender + age_c + opinAV +
                        education +  drivExperience + visImpairment +
                        (1 | participant.ID),
                    method = "PB",
                    family = "binomial", data = child.sub,
                    args_test = list(nsim = 1000, cl = cl), cl = cl,
                    control = glmerControl(optimizer = "bobyqa",
                                           optCtrl = list(maxfun = 2e5)))



child_glmm_identify_base <- mixed(decision ~ perceivedIden + motorist +
                        perceivedIden:motorist + trial +
                        (1 | participant.ID),
                    method = "PB",
                    family = "binomial", data = child.sub,
                    args_test = list(nsim = 1000, cl = cl), cl = cl,
                    control = glmerControl(optimizer = "bobyqa",
                                           optCtrl = list(maxfun = 2e5)))




child_glmm_identify_cov <- mixed(decision ~ perceivedIden + motorist +
                        perceivedIden:motorist +
                        trial + gender + age_c + opinAV +
                        education +  drivExperience + visImpairment +
                        (1 | participant.ID),
                    method = "PB",
                    family = "binomial", data = child.sub,
                    args_test = list(nsim = 1000, cl = cl), cl = cl,
                    control = glmerControl(optimizer = "bobyqa",
                                           optCtrl = list(maxfun = 2e5)))


stopCluster(cl)

## child_glmm.resid <- simulateResiduals(
##     fittedModel = child_glmm_cov$full_model,
##     n = 2000)

## child_glmm_resid.plot <- plotSimulatedResiduals(
##     simulationOutput = child_glmm.resid)

## emm_child_i <- emmeans(child_glmm_cov, ~ perspective | motorist,
##                        type = "response",
##                        contr = "pairwise")

## emm_child_persp <- emmeans(child_glmm_cov, pairwise ~ perspective,
##                            type = 'response')

## emm_child_motorist <- emmeans(child_glmm_cov, pairwise ~ motorist,
##                               type = 'response')


save.image(file = "vr_child_glmm_redo_bootstrap.RData")
