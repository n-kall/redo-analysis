set.seed(123)

load("online_covariate_nomiss_models.RData")

library(afex)
library(parallel)

(nc <- detectCores())
cl <- makeCluster(rep("localhost", nc))

carped_cov_fac_glmm <- mixed(response ~ ratio_f * perspective * motorist +
                          first_anim +
                          better_place +
                          knowledge_of_av +
                         (1 | participant),
                     family = "binomial", data = carped.sub,
                     method = "LRT", # change this to PB for final
                     args_test = list(nsim = 1000, cl = cl), cl = cl,
                     control = glmerControl(optimizer = "bobyqa",
                                            optCtrl = list(maxfun = 2e5)))


carped_ratio_followup <- emmeans(carped_cov_fac_glmm, pairwise ~ ratio_f,  type = "response")


pedped_cov_fac_glmm <- mixed(response ~ ratio_f * perspective * motorist +
                         ratio_f * scenario * motorist + #first_anim +
#                         better_place + identify + knowledge_of_av +
                         (1 + ratio_f + scenario | participant),
                     family = "binomial", data = pedped.sub,
                     method = "LRT", # change this to PB for final
                     args_test = list(nsim = 1000, cl = cl), cl = cl,
                     control = glmerControl(optimizer = "bobyqa",
                                            optCtrl = list(maxfun = 2e5)))

stopCluster(cl)
