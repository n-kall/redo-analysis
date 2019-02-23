library(emmeans)
library(tidyverse)
library(cowplot)

load("online_study_redo2.RData")

motorist_names <- c("self-driving" = "Self-driving car",
                    "human" = "Human driver")

a <- emmip(carped_cov_fac_glmm, ~ ratio_f | perspective | motorist,
           type = "response") +
  facet_grid(motorist ~ perspective, labeller = as_labeller(motorist_names)) +
  xlab("Lives-at-risk") +
  ylab("Probability of preferring swerve") +
  ylim(0, 1) + 
  geom_bar(stat="identity", aes(fill=perspective), color = "black") +
  geom_line(aes(group=perspective)) +
  geom_point(size = 1) +
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
a

ggsave("online_carped.pdf", plot = a, width = 7, height = 4)


b <- emmip(pedped_cov_fac_glmm, ~ ratio_f | perspective | motorist,
      type = "response") +
  facet_grid(motorist ~ perspective, labeller = as_labeller(motorist_names)) +
  xlab("Lives-at-risk") +
  ylab("Probability of preferring swerve") +
  ylim(0, 1) + 
  geom_bar(stat="identity", aes(fill=perspective), color = "black") +
  geom_line(aes(group=perspective)) +
  geom_point(size = 1) +
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
b

ggsave("online_pedped.pdf", plot = b, height = 4,
       width = 10)

