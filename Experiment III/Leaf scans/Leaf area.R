setwd("~/Library/CloudStorage/OneDrive-TexasTechUniversity/Experiment III/Leaf scans/ImageJ.app")
library(LeafArea)
library(jpeg)
library(png)

# Area of full leaves

# Making the yellow envelopes white, so that their area is not calculated
# This function will colour the envelopes white

whiten_envelopes <- function(folder,
                             hue_max = 0.14,  # orange hue cutoff
                             sat_min = 0.25,  # envelope is saturated
                             val_min = 0.60,  # envelope is BRIGHT (protects dark leaf)
                             grow_px = 12) {  # dilate mask to erase shadow edges
  
  files <- list.files(folder, pattern = "\\.(jpe?g|png)$",
                      ignore.case = TRUE, full.names = TRUE)
  if (length(files) == 0) stop("No .jpg/.png images found in ", folder)
  
  out_dir <- file.path(folder, "cleaned")
  dir.create(out_dir, showWarnings = FALSE)
  
  for (f in files) {
    ok <- tryCatch({
      is_png <- grepl("\\.png$", f, ignore.case = TRUE)
      img <- if (is_png) readPNG(f) else readJPEG(f)
      if (length(dim(img)) == 2) stop("grayscale image")
      nr <- dim(img)[1]; nc <- dim(img)[2]
      
      hsv <- rgb2hsv(as.vector(img[,,1]), as.vector(img[,,2]),
                     as.vector(img[,,3]), maxColorValue = 1)
      Hm <- matrix(hsv[1, ], nrow = nr)
      S  <- matrix(hsv[2, ], nrow = nr)
      V  <- matrix(hsv[3, ], nrow = nr)
      
      env <- (Hm < hue_max) & (S > sat_min) & (V > val_min)
      
      if (any(env)) {
        shift <- function(m, dr, dc) {
          out <- matrix(FALSE, nr, nc)
          out[max(1, 1 + dr):min(nr, nr + dr),
              max(1, 1 + dc):min(nc, nc + dc)] <-
            m[max(1, 1 - dr):min(nr, nr - dr),
              max(1, 1 - dc):min(nc, nc - dc)]
          out
        }
        for (i in seq_len(grow_px))
          env <- env | shift(env, 1, 0) | shift(env, -1, 0) |
            shift(env, 0, 1) | shift(env, 0, -1)
        
        img[,,1][env] <- 1
        img[,,2][env] <- 1
        img[,,3][env] <- 1
      }
      
      out_f <- file.path(out_dir, basename(f))
      if (is_png) writePNG(img, out_f) else writeJPEG(img, out_f, quality = 0.95)
      cat(sprintf("%s: whitened %.1f%% of pixels\n",
                  basename(f), 100 * mean(env)))
      TRUE
    }, error = function(e) {
      warning("FAILED on ", basename(f), ": ", conditionMessage(e))
      FALSE
    })
  }
  cat("\nCleaned images written to:", out_dir, "\n")
  invisible(out_dir)
}

# Running the code to whiten the envelopes and make them into a folder

whiten_envelopes("/Users/snehanjanachatterjee/Library/CloudStorage/OneDrive-TexasTechUniversity/Experiment III/Leaf scans")

leaf_areas <- run.ij(
  path.imagej    = "/Users/snehanjanachatterjee/Documents/Experiment_I/ImageJ.app",
  set.memory     = 4,
  set.directory  = "/Users/snehanjanachatterjee/Library/CloudStorage/OneDrive-TexasTechUniversity/Experiment III/Leaf scans/cleaned",
  distance.pixel = 591,   # <- replace with what you measure in step 2
  known.distance = 5,
  trim.pixel = 40, low.circ = 0.2, low.size = 5,
  upper.size = "Infinity", check.image = TRUE
)

class(leaf_areas)   # "data.frame"
str(leaf_areas)

write.csv(leaf_areas, "leaf_areas_expIII.csv", row.names = FALSE)

library(magick)

folder <- "/Users/snehanjanachatterjee/Library/CloudStorage/OneDrive-TexasTechUniversity/Experiment III/Leaf scans"
files <- list.files(folder, pattern = "\\.(jpe?g|png|tif+)$", ignore.case = TRUE, full.names = TRUE)
length(files)   # should be > 0 — if it's 0, the path is wrong
info <- do.call(rbind, lapply(files, function(f) {
  i <- image_info(image_read(f)[1])
  data.frame(file = basename(f), width = i$width, height = i$height)
}))
table(paste0(info$width, "x", info$height))
info

info$sample_key <- toupper(gsub("[0-9 ]+.*$", "", info$file))            # treatment letters
info$block      <- substr(gsub("[^0-9]", "", info$file), 1, 1)           # first digit
info$factor     <- ifelse(info$width > 4000, (591/1417.32)^2, 1)

library(tidyverse)

leaf_df <- leaf_areas %>%
  as_tibble() %>%
  mutate(
    rep      = str_sub(str_extract(sample, "[0-9]+"), 1, 1),
    big_scan = str_starts(sample, "HC") & rep %in% c("5", "6", "7", "8"),
    leaf_area_cm2 = if_else(big_scan, total.leaf.area * 0.17388, total.leaf.area),
    sample   = paste0(str_extract(sample, "^[A-Za-z]+"), rep),
    co2      = if_else(str_starts(sample, "HC"), "elevated CO2", "ambient CO2"),
    nitrogen = if_else(str_sub(sample, 3, 4) == "HN", "high nitrogen", "low nitrogen"),
    myco     = if_else(str_detect(sample, "Myco"), "Myco", "NoM")
  ) %>%
  select(sample, co2, nitrogen, myco, leaf_area_cm2)

write.csv(leaf_df,
          "/Users/snehanjanachatterjee/Library/CloudStorage/OneDrive-TexasTechUniversity/Experiment III/leaf_areas_expIII.csv",
          row.names = FALSE)

# Area of leaf punches

# Area was determined by taking the ara of the leaf hole punch on a white page.
# Scanning it with a scanner, determining the area through Image J and multiplying it
# with 5 (because 5 hole punches), each was 3.14 cm2

# Total leaf area

setwd("~/Library/CloudStorage/OneDrive-TexasTechUniversity/Experiment III/Leaf scans")

Total_leaf_area <- read_excel("Leaf area.xlsx")

metadata_exp3 <- read_excel("Metadata.xlsx")

Total_leaf_area <- merge(Total_leaf_area, metadata_exp3, by = "Sample")

TLA_lmer <- lmer(`Total leaf area` ~ CO2_treatment * Nutrient_treatment * AMF + (1|Block_ID), data = Total_leaf_area)

Anova(TLA_lmer)
summary(TLA_lmer)
hist(residuals(TLA_lmer))
shapiro.test(residuals(TLA_lmer)) 

plot(TLA_lmer)

# emmeans for TLA

emmeans(TLA_lmer, pairwise ~ CO2_treatment * AMF, adjust = "tukey") 
# Interaction has marginal effect on TLA through anova, but pairwise comparison has no significant effect through tukey's
emmeans(TLA_lmer, pairwise ~  Nutrient_treatment, adjust = "tukey") # TLA is higher with high nitrogen

em_TLA <- as.data.frame(emmeans(TLA_lmer, ~ AMF * Nutrient_treatment * CO2_treatment))


# ggplot with facet grid

Treatment_TLA <- ggplot(Total_leaf_area, aes(y = `Total leaf area`, x = AMF , color = AMF)) + 
  geom_jitter(width = 0.1, size = 2, alpha = 0.6)  +
  geom_point(data = em_TLA, aes(y = emmean, x = AMF), 
             shape = 18, size = 3, color = "black", inherit.aes = FALSE) +
  geom_errorbar(data = em_TLA, aes(x = AMF, y = emmean, ymin = emmean - SE, ymax = emmean + SE),  width = 0.15, color = "black", alpha = 0.6, inherit.aes = FALSE) +  
  scale_color_manual(values = c("red", "dodgerblue")) +  
  labs(x = "AMF Presence/Absence", 
       y =  expression(Total~leaf~area~(cm^2))) +
  facet_grid(CO2_treatment ~ Nutrient_treatment,  
             labeller = labeller(Light_treatment = c("Elevated CO2" = "Elevated CO2", "Ambient CO2" = "Ambient CO2"),
                                 Nutrient_treatment = c("Low nitrogen" = "Low Nitrogen", "High nitrogen" = "High Nitrogen"))) +  
  theme_bw(base_size = 17) 

+ 
  geom_signif(comparisons = list(c("No", "Yes")), map_signif_level = TRUE, y_position = c(15.5,15)) +
  ylim(NA, 17)

ggsave("Treatment_TLA.jpg", width = 10, height = 7, units = "in", dpi = 600)



