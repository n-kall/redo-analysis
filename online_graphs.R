library(emmeans)
library(tidyverse)
library(cowplot)
library(ggpubr)
library(xtable)

load("online_study_redo2.RData")

motorist_names <- c("self-driving" = "Self-driving car",
                    "human" = "Human driver",
                    "road" = "Road",
                    "sidewalk" = "Sidewalk")

a <- emmip(carped_cov_fac_glmm, ~ ratio_f | perspective | motorist,
           type = "response") +
  facet_grid(motorist ~ perspective, labeller = as_labeller(motorist_names)) +
  xlab("Lives-at-risk") +
  ylab("Probability of preferring swerve") +
  ylim(0, 1) + 
  geom_bar(stat="identity", aes(fill=perspective), color = "black") +
  #geom_line(aes(group=perspective)) +
  #geom_point(size = 1) +
  scale_fill_manual(name = "Perspective",
                    values = c("#E41A1C",
                               "#377EB8",
                               "#FF7F00"),
                    labels = c("Car occupant",
                               "Observer",
                               "Pedestrian")) +
  theme(strip.text.x = element_blank(),
        panel.grid = element_blank(),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 12))

a$layers[[1]] <- NULL
a$layers[[1]] <- NULL
a

ggsave("online_carped.pdf", plot = a, width = 7, height = 4)


b <- emmip(pedped_cov_fac_glmm, ~ ratio_f | perspective | motorist | scenario,
      type = "response") +
  facet_grid(motorist ~ scenario ~ perspective, labeller = as_labeller(motorist_names)) +
  xlab("Lives-at-risk") +
  ylab("Probability of preferring swerve") +
  ylim(0, 1) + 
  geom_bar(stat="identity", aes(fill=perspective), color = "black") +
  #geom_line(aes(group=perspective)) +
  #geom_point(size = 1) +
  scale_fill_manual(name = "Perspective",
                    values = c("#E41A1C",
                               "#377EB8",
                               "#FF7F00",
                               "#4DAF4A"),
                    labels = c("Car occupant",
                               "Observer",
                               "Pedestrian (forward)",
                               "Pedestrian (side)")) +
  theme(strip.text.x = element_blank(),
        panel.grid = element_blank(),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 12)) 

b$layers[[1]] <- NULL
b$layers[[1]] <- NULL
b

ggsave("online_pedped.pdf", plot = b, height = 10,
       width = 13)



c <- emmip(carped_cov_fac_iden_glmm, ~ ratio_f | identify | motorist,
           type = "response") +
  facet_grid(motorist ~ identify, labeller = as_labeller(motorist_names)) +
  xlab("Lives-at-risk") +
  ylab("Probability of preferring swerve") +
  ylim(0, 1) + 
  geom_bar(stat="identity", aes(fill=identify), color = "black") +
  #geom_line(aes(group=identify)) +
  #geom_point(size = 1) +
  scale_fill_manual(name = "Identification",
                    values = c("#FB8072",
                               "#FDB462"),
                    labels = c("Car occupant",
                               "Pedestrian")) +
  theme(strip.text.x = element_blank(),
        panel.grid = element_blank(),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 12))

c$layers[[1]] <- NULL
c$layers[[1]] <- NULL
c

ggsave("online_carped_ident.pdf", plot = c, height = 4,
       width = 6)



test <- ggarrange(a,c, 
                  labels = c("A", "B"),
                  widths = c(2.2, 1.75))

ggsave("online_carped_combine.pdf", plot = test, height = 4,
       width = 13)



carped_followup <- emmeans(carped_cov_fac_glmm, trt.vs.ctrl ~ ratio_f | perspective | motorist,
        type = "response")

carped_ident_followup <- emmeans(carped_cov_fac_iden_glmm, trt.vs.ctrl ~ ratio_f | motorist,
                           type = "response")

carped_ident_followup2 <- emmeans(carped_cov_fac_iden_glmm, pairwise ~ identify | ratio_f | motorist,
                                  type = "response")


pedped_follup <- emmeans(pedped_cov_fac_glmm, trt.vs.ctrl ~ ratio_f | perspective | motorist,
                         type = "response")

pedped_sidewalk_followup <- emmeans(pedped_cov_fac_glmm, pairwise ~ scenario)
