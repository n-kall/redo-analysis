
sidewalk_dec <- as_tibble(emmeans(sidewalk_glmm_cov, ~ perspective|motorist, type = "response"))

sidewalk_conf <- as_tibble(emmeans(sidewalk_lmm_cov, ~ perspective|motorist|decision, type = "response"))


motorist_names <- c("self-driving" = "Self-driving car",
                    "human" = "Human driver")

sidewalk_conf$prob <- sidewalk_dec$prob

sidewalk_conf  <-  mutate(sidewalk_conf, prob = ifelse(decision == "hitSidewalk",
                                       -1 * (1-prob), prob))


sidewalk_plot <- sidewalk_conf %>%
    mutate(emmean = ifelse(decision == "hitSidewalk", -1 * emmean, emmean),
           lower.CL = ifelse(decision == "hitSidewalk", -1 * lower.CL, lower.CL),
           upper.CL = ifelse(decision == "hitSidewalk", -1 * upper.CL, upper.CL)) %>%
    ggplot(mapping = aes(x = perspective)) +
    geom_bar(aes(y = prob * 100, fill = decision), stat = "identity", color = "black") +
    geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL, width = 0.1)) +
    geom_point(mapping = aes(y = emmean,
                             shape = decision), size = 2.5, color = "black", fill = "white") +
    ylim(-130, 130) +
    facet_grid(motorist ~ ., labeller = as_labeller(motorist_names)) +
    coord_flip() +
        scale_x_discrete(name = "Perspective",
                     limits = rev(levels(sidewalk_conf$perspective)),
                     labels = c("Pedestrian\non road",
                                "Pedestrian\non sidewalk",
                                "Passenger", "Bystander")) +
    scale_fill_manual(        values = c("#FB6A4A", "#A50F15"),
                              name = "Predicted probability of decision",
        labels = c("Endanger fewer pedestrians",
                   "Endanger more pedestrians", "", "")) +
    scale_y_continuous(name = "Confidence", limits = c(-112,112),
                       sec.axis = sec_axis(
                           ~., name = "Probability",
                           labels = c("1.0", "0.5", "0", "0.5", "1.0")),
                       labels = c(100, 50, 0, 50, 100)) +
        scale_shape_manual(
        values = c(15, 22),
        name = "Predicted mean (95% CI) confidence in decision",
        labels = c("Endanger more pedestrians (adults / on road)",
                   "Endanger fewer pedestrians (children / on sidewalk)")) +
        guides(fill = guide_legend(order = 1),
           shape = guide_legend(order = 2)) +
    theme_cowplot(font_size = 9) + theme(legend.position = "bottom",
                         legend.direction = "vertical")




motorist_names <- c("self-driving" = "Self-driving car",
                    "human" = "Human driver")


child_dec <- as_tibble(emmeans(child_glmm_cov, ~ perspective|motorist, type = "response"))

child_conf <- as_tibble(emmeans(child_lmm_cov, ~ perspective|motorist|decision, type = "response"))

child_conf$prob <- child_dec$prob

child_conf  <-  mutate(child_conf, prob = ifelse(decision == "hitChildren",
                                       -1 * (prob), 1 - prob))


child_plot  <- child_conf %>%
    mutate(emmean = ifelse(decision == "hitAdults", -1 * emmean, emmean),
           lower.CL = ifelse(decision == "hitAdults", -1 * lower.CL, lower.CL),
           upper.CL = ifelse(decision == "hitAdults", -1 * upper.CL, upper.CL)) %>%
    ggplot(mapping = aes(x = perspective)) +
    geom_bar(aes(y = prob * 100, fill = decision), stat = "identity", color = "black") +
    geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL, width = 0.1)) +
    geom_point(mapping = aes(y = emmean,
                             shape = decision), size = 2.5, color = "black", fill = "white") +
    ylim(-130, 130) +
    facet_grid(motorist ~ ., labeller = as_labeller(motorist_names)) +
    coord_flip() +
        scale_x_discrete(name = "Perspective",
                     limits = rev(levels(child_conf$perspective)),
                     labels = c("Pedestrian\n with adults",
                                "Pedestrian\n with children",
                                "Passenger", "Bystander")) +
    scale_fill_manual(        values = c("#A50F15", "#FB6A4A"),
                              name = "Predicted probability of decision",
                      labels = c("Endanger more pedestrians (adults / on road)",
                                 "Endanger fewer pedestrians (children / on sidewalk)")) +
    scale_y_continuous(name = "Confidence", limits = c(-112,112),
                       sec.axis = sec_axis(
                           ~., name = "Probability",
                           labels = c("1.0", "0.5", "0", "0.5", "1.0")),
                                      labels = c(100, 50, 0, 50, 100)) +
        scale_shape_manual(
        values = c(15, 22),
        name = "Predicted mean (95% CI) confidence in decision",
        labels = c("Endanger fewer pedestrians (children / on sidewalk)",
                   "Endanger more pedestrians (adults / on road)")) +
        guides(fill = guide_legend(order = 1),
           shape = guide_legend(order = 2)) +
    theme_cowplot(font_size = 9) + theme(legend.position = "bottom",
                         legend.direction = "vertical")


child_sidewalk.plot <- ggarrange(child_plot, sidewalk_plot, common.legend = TRUE,
                                 nrow = 2,
                                 legend = "bottom", labels = c("A", "B"))






carsac_dec <- as_tibble(emmeans(carsac_glmm_cov,
                                ~ perspective|motorist|trial,
                                type = "response"))


carsac_conf <- as_tibble(emmeans(carsac_lmm_cov, ~ perspective|motorist|decision|trial,
                                type = "response"))



carsac_cliff_dec <- subset(carsac_dec, carsac_dec$trial == "mountain")
carsac_cliff_conf <- subset(carsac_conf, carsac_conf$trial == "mountain")


carsac_cliff_conf$prob <- carsac_cliff_dec$prob

carsac_cliff_conf  <-  mutate(carsac_cliff_conf, prob = ifelse(decision == "selfSacrifice",
                                       -1 * (1-prob), prob))


carsac_cliff_plot  <- carsac_cliff_conf %>%
    mutate(emmean = ifelse(decision == "selfSacrifice", -1 * emmean, emmean),
           lower.CL = ifelse(decision == "selfSacrifice", -1 * lower.CL, lower.CL),
           upper.CL = ifelse(decision == "selfSacrifice", -1 * upper.CL, upper.CL)) %>%
    ggplot(mapping = aes(x = perspective)) +
    geom_bar(aes(y = prob * 100, fill = decision), stat = "identity", color = "black") +
    geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL, width = 0.1)) +
    geom_point(mapping = aes(y = emmean,
                             shape = decision), size = 2.5, color = "black", fill = "white") +
    ylim(-130, 130) +
    facet_grid(motorist ~ ., labeller = as_labeller(motorist_names)) +
    coord_flip() +
        scale_x_discrete(name = "Perspective",
                     limits = rev(levels(carsac_cliff_conf$perspective)),
                     labels = c("Pedestrian\non road",
                                "Passenger", "Bystander")) +
    scale_fill_manual(        values = c("#b2abd2", "#fdb863"),
                              name = "Predicted probability of decision",
        labels = c("Endanger car occupants",
                   "Endanger pedestrians on road")) +
    scale_y_continuous(name = "Confidence", limits = c(-112,112),
                       sec.axis = sec_axis(
                           ~., name = "Probability",
                           labels = c("1.0", "0.5", "0", "0.5", "1.0")),
                       labels = c(100, 50, 0, 50, 100)) +
        scale_shape_manual(
        values = c(22, 15),
        name = "Predicted mean (95% CI) confidence in decision",
        labels = c("Endanger fewer pedestrians",
                   "Endanger more pedestrians")) +
        guides(fill = guide_legend(order = 1),
           shape = guide_legend(order = 2)) +
    theme_cowplot(font_size = 9) + theme(legend.position = "bottom",
                         legend.direction = "vertical")







carsac_van_dec <- subset(carsac_dec, carsac_dec$trial == "cityR")
carsac_van_conf <- subset(carsac_conf, carsac_conf$trial == "cityR")


carsac_van_conf$prob <- carsac_van_dec$prob

carsac_van_conf  <-  mutate(carsac_van_conf, prob = ifelse(decision == "selfSacrifice",
                                       -1 * (1-prob), prob))


carsac_van_plot  <- carsac_van_conf %>%
    mutate(emmean = ifelse(decision == "selfSacrifice", -1 * emmean, emmean),
           lower.CL = ifelse(decision == "selfSacrifice", -1 * lower.CL, lower.CL),
           upper.CL = ifelse(decision == "selfSacrifice", -1 * upper.CL, upper.CL)) %>%
    ggplot(mapping = aes(x = perspective)) +
    geom_bar(aes(y = prob * 100, fill = decision), stat = "identity", color = "black") +
    geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL, width = 0.1)) +
    geom_point(mapping = aes(y = emmean,
                             shape = decision), size = 2.5, color = "black", fill = "white") +
    ylim(-130, 130) +
    facet_grid(motorist ~ ., labeller = as_labeller(motorist_names)) +
    coord_flip() +
        scale_x_discrete(name = "Perspective",
                     limits = rev(levels(carsac_van_conf$perspective)),
                     labels = c("Pedestrian\non road",
                                "Passenger", "Bystander")) +
    scale_fill_manual(        values = c("#b2abd2", "#fdb863"),
                              name = "Predicted probability of decision",
        labels = c("Endanger car occupants",
                   "Endanger pedestrians on road")) +
    scale_y_continuous(name = "Confidence", limits = c(-112,112),
                       sec.axis = sec_axis(
                           ~., name = "Probability",
                           labels = c("1.0", "0.5", "0", "0.5", "1.0")),
                       labels = c(100, 50, 0, 50, 100)) +
        scale_shape_manual(
        values = c(22, 15),
        name = "Predicted mean (95% CI) confidence in decision",
        labels = c("Endanger car occupants",
                   "Endanger pedestrians on road")) +
        guides(fill = guide_legend(order = 1),
           shape = guide_legend(order = 2)) +
    theme_cowplot(font_size = 9) + theme(legend.position = "bottom",
                         legend.direction = "vertical")


carsac_joint.plot <- ggarrange(carsac_van_plot,
                               carsac_cliff_plot,
                               labels = c("C", "D"),
                               nrow = 2,
                               common.legend = TRUE, legend = "bottom")

vr_combined_pred.plot <- ggarrange(child_sidewalk.plot, carsac_joint.plot)


# save as pdf
pdf('vr_combined_pred_plot.pdf', width = 12, height = 12)
vr_combined_pred.plot
dev.off()
