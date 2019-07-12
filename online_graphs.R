library(emmeans)
library(tidyverse)
library(cowplot)
library(ggpubr)
library(xtable)

load("online_study_redo2.RData")

motorist_names <- c("self-driving" = "Self-driving car",
                    "human" = "Human driver",
                    "road" = "Connecting road",
                    "sidewalk" = "Sidewalk",
                    "pedestrian" = "Pedestrian",
                    "car" = "Car occupant",
                    "observer" = "Observer",
                    "pedestrian_fwd" = "Pedstrian (in front)",
                    "pedestrian_side" = "Pedestrian (to side)")

a <- emmip(carped_cov_fac_glmm, ~ ratio_f | perspective | motorist,
           type = "response") +
  facet_grid(motorist ~ perspective, labeller = as_labeller(motorist_names)) +
  xlab("Lives-at-risk") +
  ylab("Probability of preferring 'swerve'") +
  ylim(0, 1) + 
  geom_bar(stat="identity", aes(fill=perspective), color = "black") +
  #geom_line(aes(group=perspective)) +
  #geom_point(size = 1) +
  scale_fill_manual(name = "Perspective",
                    values = c("#FB6A4A",
                               "#6BAED6",
                               "#FD8D3C")) +
  theme(panel.grid = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 12),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12)) +
  guides(fill = FALSE) + theme_cowplot()

a$layers[[1]] <- NULL
a$layers[[1]] <- NULL
a

ggsave("study2-carped-graph.png", dpi = 300, plot = a, width = 6, height = 4)


b <- emmip(pedped_cov_fac_glmm, ~ ratio_f | perspective | motorist | scenario,
      type = "response") +
  facet_grid(motorist ~ scenario ~ perspective, labeller = as_labeller(motorist_names)) +
  xlab("Lives-at-risk") +
  ylab("Probability of preferring 'swerve'") +
  ylim(0, 1) + 
  geom_bar(stat="identity", aes(fill=perspective), color = "black") +
  #geom_line(aes(group=perspective)) +
  #geom_point(size = 1) +
  scale_fill_manual(name = "Perspective",
                    values = c("#FB6A4A",
                               "#6BAED6",
                               "#FD8D3C",
                               "#74C476")) +
  theme(panel.grid = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 12),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12)) +
  guides(fill = FALSE) + theme_cowplot()

b$layers[[1]] <- NULL
b$layers[[1]] <- NULL
b

ggsave("study2-pedped-graph.png", plot = b, height = 10*0.75,
       width = 11*0.75, dpi = 300)



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
