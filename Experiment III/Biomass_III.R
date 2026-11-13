setwd("~/Library/CloudStorage/OneDrive-TexasTechUniversity/Experiment III")

library(dplyr)
library(readr)
library(stringr)
library(readxl)
library(tidyr)
library(reshape)
library(car)
library(emmeans)
library(multcompView)
library(lme4)
library(ggplot2)
library(ggsignif)


Total_biomass <- read_excel("Dry_weight_master_sheet.xlsx")  

# LCHN Myco 3 plant died

Total_biomass_clean <- Total_biomass %>% mutate(Root_shoot_ratio = `Total root weight (g)` / `Total aboveground weight (g)`) %>%
  mutate(Total_biomass = `Total root weight (g)` + `Total aboveground weight (g)`)


# Root shoot ratio

RS_lmer <- lmer(log(Root_shoot_ratio) ~ CO2_treatment * Nutrient_treatment * AMF + (1|Block_ID), data = Total_biomass_clean)

Anova(RS_lmer)
summary(RS_lmer)
hist(residuals(RS_lmer))
shapiro.test(residuals(RS_lmer)) 

plot(RS_lmer)

emmeans(RS_lmer, pairwise ~ Nutrient_treatment, adjust = "tukey") # RS ratio is low in high nitrogen

# emmeans for root shoot ratio

em_RS <- as.data.frame(emmeans(RS_lmer, ~ AMF * Nutrient_treatment * CO2_treatment, type = "response"))


# ggplot with facet grid

Treatment_RS <- ggplot(Total_biomass_clean, aes(y = Root_shoot_ratio, x = AMF , color = AMF)) + 
  geom_jitter(width = 0.1, size = 2, alpha = 0.6)  +
  geom_point(data = em_RS, aes(y = response, x = AMF), 
             shape = 18, size = 3, color = "black", inherit.aes = FALSE) +
  geom_errorbar(data = em_RS, aes(x = AMF, y = response, ymin = response - SE, ymax = response + SE),  width = 0.15, color = "black", alpha = 0.6, inherit.aes = FALSE) +  
  scale_color_manual(values = c("red", "dodgerblue")) +  
  labs(x = "AMF Presence/Absence", 
       y =  "Root : Shoot ratio") +
  facet_grid(CO2_treatment ~ Nutrient_treatment,  
             labeller = labeller(CO2_treatment = c("Elevated CO2" = "Elevated CO2", "Ambient CO2" = "Ambient CO2"),
                                 Nutrient_treatment = c("Low nitrogen" = "Low Nitrogen", "High nitrogen" = "High Nitrogen"))) +  
  theme_bw(base_size = 17) 


+ geom_signif(comparisons = list(c("No", "Yes")), map_signif_level = TRUE, y_position = c(3.5, 4)) +
  ylim(NA, 4)


ggsave("Treatment_RS.jpg", width = 10, height = 7, units = "in", dpi = 600)

# Biomass

B_lmer <- lmer(sqrt(Total_biomass) ~ CO2_treatment * Nutrient_treatment * AMF + (1|Block_ID), data = Total_biomass_clean)

Anova(B_lmer)
summary(B_lmer)
hist(residuals(B_lmer))
shapiro.test(residuals(B_lmer)) 

plot(B_lmer)

emmeans(B_lmer, pairwise ~ Nutrient_treatment, adjust = "tukey") # Biomass is higher in high nitrogen

# emmeans for biomass

em_B <- as.data.frame(emmeans(B_lmer, ~ AMF * Nutrient_treatment * CO2_treatment, type = "response"))


# ggplot with facet grid

Treatment_B <- ggplot(Total_biomass_clean, aes(y = Total_biomass, x = AMF , color = AMF)) + 
  geom_jitter(width = 0.1, size = 2, alpha = 0.6)  +
  geom_point(data = em_B, aes(y = response, x = AMF), 
             shape = 18, size = 3, color = "black", inherit.aes = FALSE) +
  geom_errorbar(data = em_B, aes(x = AMF, y = response, ymin = response - SE, ymax = response + SE),  width = 0.15, color = "black", alpha = 0.6, inherit.aes = FALSE) +  
  scale_color_manual(values = c("red", "dodgerblue")) +  
  labs(x = "AMF Presence/Absence", 
       y =  "Total biomass (g)") +
  facet_grid(CO2_treatment ~ Nutrient_treatment,  
             labeller = labeller(CO2_treatment = c("Elevated CO2" = "Elevated CO2", "Ambient CO2" = "Ambient CO2"),
                                 Nutrient_treatment = c("Low nitrogen" = "Low Nitrogen", "High nitrogen" = "High Nitrogen"))) +  
  theme_bw(base_size = 17) 

+ 
  geom_signif(comparisons = list(c("No", "Yes")), map_signif_level = TRUE, y_position = c(15.5,15)) +
  ylim(NA, 17)


ggsave("Treatment_B.jpg", width = 10, height = 7, units = "in", dpi = 600)


library(tidyverse)
library(patchwork)
library(car)

dodge  <- position_dodge(width = 0.8)
jdodge <- position_jitterdodge(jitter.width = 0.15, dodge.width = 0.8)

# ---- Build the significance label from your model ----
sig_label <- function(model, type = 3) {
  a <- as.data.frame(car::Anova(model, type = type))
  p <- a[[ncol(a)]]                      # works for lm (Pr(>F)) and lmer (Pr(>Chisq))
  terms <- rownames(a)
  keep <- !is.na(p) & p < 0.05 & terms != "(Intercept)"
  if (!any(keep)) return("n.s.")
  stars <- as.character(cut(p[keep], c(-Inf, 0.001, 0.01, 0.05),
                            labels = c("***", "**", "*")))
  nice <- terms[keep] |>
    str_replace_all(c("CO2_treatment" = "CO\u2082",
                      "Nutrient_treatment" = "N",
                      ":" = " × "))
  paste0(nice, stars, collapse = "\n")
}

# ---- One panel ----
plot_trait <- function(raw, em, yvar, ylab, sig) {
  ggplot(em, aes(CO2_treatment, response, color = AMF)) +
    geom_point(data = raw, aes(y = .data[[yvar]]),
               size = 2.5, alpha = 0.3, position = jdodge) +
    geom_errorbar(aes(ymin = response - SE, ymax = response + SE),
                  width = 0.2, linewidth = 1, position = dodge) +
    geom_point(shape = 15, size = 5, position = dodge) +
    geom_text(data = tibble(Nutrient_treatment = "Low Nitrogen", label = sig),
              aes(x = Inf, y = Inf, label = label), inherit.aes = FALSE,
              hjust = 1.05, vjust = 1.2, size = 4, lineheight = 0.9) +
    scale_color_manual(values = c(No = "red", Yes = "darkgreen"),
                       labels = c(No = "AMF absent", Yes = "AMF present")) +
    scale_x_discrete(labels = c("Ambient CO2"  = expression(Ambient~CO[2]),
                                "Elevated CO2" = expression(Elevated~CO[2]))) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
    expand_limits(y = 0) +
    facet_wrap(~ Nutrient_treatment,
               labeller = as_labeller(c("High Nitrogen" = "High Nitrogen",
                                        "Low Nitrogen"  = "Low Nitrogen"))) +
    labs(x = NULL, y = ylab) +
    theme_classic(base_size = 14) +
    theme(legend.title = element_blank(),
          legend.position = "top",
          strip.background = element_blank(),
          strip.text = element_text(size = 14))
}

# ---- Panels ----
p_a <- plot_trait(Total_biomass_clean, em_B, "Total_biomass",
                  "Total biomass (g)", sig_label(B_lmer))


# p_b <- plot_trait(<raw_data>, <em_object>, "<column>", "<y label>", sig_label(<model>))

# ---- Combine ----
(p_a / p_b) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "a") &
  theme(legend.position = "top")


# PDF, using the default device
ggsave("Total_biomassII.pdf", p_a, width = 7, height = 11)

# or PNG
ggsave("Total_biomassII.png", p_a, width = 7, height = 11, dpi = 300)

