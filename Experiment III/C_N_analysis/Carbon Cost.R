setwd("~/Library/CloudStorage/OneDrive-TexasTechUniversity/Experiment III/C_N_analysis/")

# Loading all the packages
library(dplyr)
library(readxl)
library(tidyr)
library(car)
library(emmeans)
library(lme4)
library(ggplot2)
library(ggpubr)
library(ggsignif)
library(cowplot)


# Reading excel sheet data

Leaf_costech <- read_excel("Leaf_C_N.xlsx")
Stem_costech <- read_excel("Stem_C_N.xlsx")
Root_costech <- read_excel("Root_C_N.xlsx")
metadata <- read_excel("Metadata.xlsx")

# Merging leaf, stem, root and metadata sheets

merge_costech <- Leaf_costech %>%
  left_join(Stem_costech, by = "Sample") %>%
  left_join(Root_costech, by = "Sample") %>%
  left_join( metadata, by = "Sample")

# Making two columns of whole plant nitrogen biomass and carbon cost to
# acquire and use nutrients

merge_costech <- merge_costech %>%
  mutate(Whole_plant_nitrogen_biomass = `Total leaf biomass nitrogen`+ `Total root biomass nitrogen`+ `Total stem biomass nitrogen`,
         Carbon_cost_to_acquire_nutrients = `Total root biomass carbon` / Whole_plant_nitrogen_biomass)

# Root carbon biomass

Root_lmer <- lmer(sqrt(`Total root biomass carbon`) ~ CO2_treatment * Nutrient_treatment * AMF + (1|Block_ID), data = merge_costech)

Anova(Root_lmer)
summary(Root_lmer)
hist(residuals(Root_lmer))
shapiro.test(residuals(Root_lmer)) 

plot(Root_lmer)

emmeans(Root_lmer, pairwise ~ Nutrient_treatment, adjust = "tukey") # Root biomass carbon was higher with nitrogen, no AMF effect

#ggplot 

em_Root <- as.data.frame(emmeans(Root_lmer, ~ AMF * Nutrient_treatment * CO2_treatment, type = "response"))


Treatment_Root <- ggplot(merge_costech, aes(y = `Total root biomass carbon`, x = AMF , color = AMF)) + 
    geom_jitter(width = 0.2, size = 2, alpha = 0.8)  +
    geom_point(data = em_Root, aes(y = response , x = AMF), 
               shape = 18, size = 3, color = "black", inherit.aes = FALSE) +
    geom_errorbar(data = em_Root, aes(x = AMF, y = response, ymin = response - SE, ymax = response + SE),  width = 0.15, color = "black", alpha = 0.6, inherit.aes = FALSE) +  
    scale_color_manual(values = c("red", "dodgerblue")) +  
    labs(x = "AMF Presence/Absence", 
         y = "Root biomass carbon (gC)") +
    facet_grid(CO2_treatment ~ Nutrient_treatment,  
               labeller = labeller(CO2_treatment = c("Elevated CO2" = "Elevated CO2", "Ambient CO2" = "Ambient CO2"),
                                   Nutrient_treatment = c("Low Nitrogen" = "Low Nitrogen", "High Nitrogen" = "High Nitrogen"))) +  
    theme_bw(base_size = 17)

+ draw_label("(B)", x = 0.9, y = 0.09, hjust = 0, vjust = 1,
                                           size = 20, fontface = "bold", color = "blue")

ggsave("Treatment_Root.jpg", width = 10, height = 7, units = "in", dpi = 600)

# Whole plant nitrogen biomass

WPN_lmer <- lmer(log(Whole_plant_nitrogen_biomass) ~ CO2_treatment * Nutrient_treatment * AMF + (1|Block_ID), data = merge_costech)

Anova(WPN_lmer)
summary(WPN_lmer)
hist(residuals(WPN_lmer))

shapiro.test(residuals(WPN_lmer)) 

plot(WPN_lmer)

emmeans(WPN_lmer, pairwise ~ Nutrient_treatment, adjust = "tukey") # Whole plant biomass N increased with High N
emmeans(WPN_lmer, pairwise ~ CO2_treatment, adjust = "tukey") # WPBN was marginally significantly higher in ambient CO2
emmeans(WPN_lmer, pairwise ~ Nutrient_treatment * AMF, adjust = "tukey") # WPN was significantly higher when HN & AMF were present compared to LN & AMF
emmeans(WPN_lmer, pairwise ~ Nutrient_treatment * CO2_treatment, adjust = "tukey") # WPN was significantly higher with HN & elevated CO2 combined than Low N & Elevated CO2


#ggplot 

em_WPN <- as.data.frame(emmeans(WPN_lmer, ~AMF * Nutrient_treatment * CO2_treatment, type = "response")) 


Treatment_WPN <- ggplot(merge_costech, aes(y = Whole_plant_nitrogen_biomass, x = AMF , color = AMF)) + geom_jitter(width = 0.2, size = 2, alpha = 0.8)  +
    geom_point(data = em_WPN, aes(y = response, x = AMF), 
               shape = 18, size = 3, color = "black", inherit.aes = FALSE) +
    geom_errorbar(data = em_WPN, aes(x = AMF, y = response,ymin = response - SE, ymax = response + SE),  width = 0.15, color = "black", alpha = 0.6, inherit.aes = FALSE) +  
    scale_color_manual(values = c("red", "dodgerblue")) +  
    labs(x = "AMF Presence/Absence", 
         y = "Whole plant biomass nitrogen (gN)") +
    facet_grid(CO2_treatment ~ Nutrient_treatment,  
               labeller = labeller(CO2_treatment = c("Elevated CO2" = "Elevated CO2", "Ambient CO2" = "Ambient CO2"),
                                   Nutrient_treatment = c("Low Nitrogen" = "Low Nitrogen", "High Nitrogen" = "High Nitrogen"))) + 
    theme_bw(base_size = 17)

+ draw_label("(A)", x = 0.9, y = 0.09, hjust = 0, vjust = 1,
                                            size = 20, fontface = "bold", color = "blue")

ggsave("Treatment_WPN.jpg", width = 10, height = 7, units = "in", dpi = 600)


# Model for carbon cost to acquire and use nutrients

CC_lmer <- lmer(Carbon_cost_to_acquire_nutrients ~ CO2_treatment * Nutrient_treatment * AMF + (1|Block_ID), data = merge_costech)

Anova(CC_lmer)
summary(CC_lmer)
hist(residuals(CC_lmer))
shapiro.test(residuals(CC_lmer)) 

plot(CC_lmer)

emmeans(CC_lmer, pairwise ~ Nutrient_treatment, adjust = "tukey") # carbon cost decreases with High Nitrogen
emmeans(CC_lmer, pairwise ~ CO2_treatment * Nutrient_treatment, adjust = "tukey") # under low N, elevated CO2 increases CC

# emmeans for carbon cost
em_CC <- as.data.frame(emmeans(CC_lmer, ~AMF * Nutrient_treatment * CO2_treatment)) 

# ggplot for carbon cost nutrient (with signif)

Treatment_CC <- ggplot(merge_costech, aes(y = Carbon_cost_to_acquire_nutrients, x = AMF , color = AMF)) + geom_jitter(width = 0.2, size = 2, alpha = 0.8)  +
  geom_point(data = em_CC, aes(y = emmean, x = AMF), 
             shape = 18, size = 3, color = "black", inherit.aes = FALSE) +
  geom_errorbar(data = em_CC, aes(x = AMF, y = emmean,ymin = emmean - SE, ymax = emmean + SE),  width = 0.15, color = "black", alpha = 0.6, inherit.aes = FALSE) +  
  scale_color_manual(values = c("red", "dodgerblue")) +  
  labs(x = "AMF Presence/Absence", 
       y = expression("Carbon cost to acquire and use nutrients (gC gN"^{-1}*")")) +
  facet_grid(CO2_treatment ~ Nutrient_treatment,  
             labeller = labeller(CO2_treatment = c("Elevated CO2" = "Elevated CO2", "Ambient CO2" = "Ambient CO2"),
                                 Nutrient_treatment = c("Low Nitrogen" = "Low Nitrogen", "High Nitrogen" = "High Nitrogen"))) + theme_bw(base_size = 17)  


ggsave("Treatment_CC.jpg", width = 10, height = 7, units = "in", dpi = 600)


# ggplot for carbon cost nutrient (without signif)

Treatment_CC_wsig <- ggdraw(
  ggplot(merge_costech, aes(y = Carbon_cost_to_acquire_nutrients, x = AMF , color = AMF)) + geom_jitter(width = 0.2, size = 2, alpha = 0.8)  +
    geom_point(data = em_CC, aes(y = emmean, x = AMF), 
               shape = 18, size = 3, color = "black", inherit.aes = FALSE) +
    geom_errorbar(data = em_CC, aes(x = AMF, y = emmean,ymin = emmean - SE, ymax = emmean + SE),  width = 0.15, color = "black", alpha = 0.6, inherit.aes = FALSE) +  
    scale_color_manual(values = c("red", "dodgerblue")) +  
    labs(x = "AMF Presence/Absence", 
         y = expression("Carbon cost to acquire and use nutrients (gC gN"^{-1}*")")) +
    facet_grid(CO2_treatment ~ Nutrient_treatment,  
               labeller = labeller(CO2_treatment = c("Elevated CO2" = "Elevated CO2", "Ambient CO2" = "Ambient CO2"),
                                   Nutrient_treatment = c("Low Nitrogen" = "Low Nitrogen", "High Nitrogen" = "High Nitrogen"))) +  
    theme_bw(base_size = 17)) + draw_label("(A)", x = 0.9, y = 0.09, hjust = 0, vjust = 1,
                                           size = 20, fontface = "bold", color = "blue")

ggsave("Treatment_CC_wsig.jpg", width = 10, height = 7, units = "in", dpi = 600)