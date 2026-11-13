setwd("~/Library/CloudStorage/OneDrive-TexasTechUniversity/Experiment III")

library(readLicorData)
library(dplyr)
library(stringr)
library(readxl)
library(tidyr)
library(reshape)
library(car)
library(emmeans)
library(multcompView)
library(lme4)
library(ggplot2)
library(plantecophys)

# Light & resp curves from Gibson

setwd("~/Library/CloudStorage/OneDrive-TexasTechUniversity/Experiment III/ACi/Gibson")

## read all unix licor files and convert into .csv format.
## The following loop should run only once.

list.files()

files <- c(
  "2026-04-17_NxCO2xMyco_gibson_resp",
  "2026-04-17-NxCO2xMyco_gibson",
  "2026-05-26-gibson",
  "2026-05-2NxCO2xMyco_Gibson_resp"
)

for (i in seq_along(files)) {
  
  cat("Processing:", files[i], "\n")
  
  a <- licorData(location = files[i])
  
  write.csv(
    a,
    paste0(files[i], ".csv"),
    row.names = FALSE
  )
}

# Light curves from Ozzie

setwd("~/Library/CloudStorage/OneDrive-TexasTechUniversity/Experiment III/ACi/Ozzie")

list.files()

files <- c(
  "2026-04-19-NxCO2xMyco_Ozzie"
)

for (i in seq_along(files)) {
  
  cat("Processing:", files[i], "\n")
  
  a <- licorData(location = files[i])
  
  write.csv(
    a,
    paste0(files[i], ".csv"),
    row.names = FALSE
  )
}

# Light curves from Stan

setwd("~/Library/CloudStorage/OneDrive-TexasTechUniversity/Experiment III/ACi/Stan")

list.files()

files <- c(
  "2026-04-17-stan_sneha",
  "2026-04-19-NxCO2xMyco_Stan",
  "2026-04-19-NxCO2xMyco_Stan_resp",
  "2026-04-20-NxCO2xMyco_Stan"
)

for (i in seq_along(files)) {
  
  cat("Processing:", files[i], "\n")
  
  a <- licorData(location = files[i])
  
  write.csv(
    a,
    paste0(files[i], ".csv"),
    row.names = FALSE
  )
}

# Light curves from Yadi

setwd("~/Library/CloudStorage/OneDrive-TexasTechUniversity/Experiment III/ACi/Yadi")

list.files()

files <- c(
  "2026-04-17_NxCO2xMyco_Yadi_resp",
  "2026-04-17-NxCO2xMyco_Yadi",
  "2026-04-19-NxCO2xMyco_Yadi",
  "2026-04-19-NxCO2xMyco_Yadi_resp",
  "2026-04-20-NxCO2xMyco_Yadi",
  "2026-04-20-NxCO2xMyco_Yadi_resp",
  "2026-05-26-sneha_yadi_NxCO2xMyco"
)

for (i in seq_along(files)) {
  
  cat("Processing:", files[i], "\n")
  
  a <- licorData(location = files[i])
  
  write.csv(
    a,
    paste0(files[i], ".csv"),
    row.names = FALSE
  )
}

# Light curves from Albert

setwd("~/Library/CloudStorage/OneDrive-TexasTechUniversity/Experiment III/ACi/Albert")

list.files()

files <- c(
  "2026-04-17-NxCO2xMyco_albert",
  "2026-04-19_NxCO2xMyco_Albert_resp",
  "2026-04-19-NxCO2xMyco_Albert",
  "2026-04-20-NxCO2xMyco_albert",
  "2026-04-20-NxCO2xMyco_Albert_resp",
  "2026-05-26-Albert_NxCO2xMyco",
  "2026-05-26-NxCO2xMyco_albert_resp"
)

for (i in seq_along(files)) {
  
  cat("Processing:", files[i], "\n")
  
  a <- licorData(location = files[i])
  
  write.csv(
    a,
    paste0(files[i], ".csv"),
    row.names = FALSE
  )
}


# Final csv files for all machines

# Making a function for all csv files with proper columns

final_vars <- c("obs", "elapsed", "date", "id", "A", "Ci", "Ca",
  "gsw", "CO2_s", "CO2_r", "H2O_s", "H2O_r",
  "Qin", "VPDleaf", "Flow", "Tleaf")

make_final <- function(file) {
  
  data <- read.csv(file)
  
  data %>%
    select(all_of(final_vars))
}

# Gibson

setwd("~/Library/CloudStorage/OneDrive-TexasTechUniversity/Experiment III/ACi/Gibson")


Gibson_1_resp_final <- make_final("2026-04-17_NxCO2xMyco_gibson_resp.csv")
Gibson_1_final <- make_final("2026-04-17-NxCO2xMyco_gibson.csv")
Gibson_2_final <- make_final("2026-05-26-gibson.csv")
Gibson_2_resp_final<- make_final("2026-05-2NxCO2xMyco_Gibson_resp.csv")

# ozzie

setwd("~/Library/CloudStorage/OneDrive-TexasTechUniversity/Experiment III/ACi/Ozzie")


Ozzie_1_final <- make_final("2026-04-19-NxCO2xMyco_Ozzie.csv")

# Stan

setwd("~/Library/CloudStorage/OneDrive-TexasTechUniversity/Experiment III/ACi/Stan")


Stan_1_final <- make_final("2026-04-17-stan_sneha.csv")
Stan_2_final <- make_final("2026-04-19-NxCO2xMyco_Stan.csv")
Stan_1_resp_final <- make_final("2026-04-19-NxCO2xMyco_Stan_resp.csv")
Stan_3_final <- make_final("2026-04-20-NxCO2xMyco_Stan.csv")

# Yadi

setwd("~/Library/CloudStorage/OneDrive-TexasTechUniversity/Experiment III/ACi/Yadi")


Yadi_1_resp_final <- make_final("2026-04-17_NxCO2xMyco_Yadi_resp.csv")
Yadi_1_final <- make_final("2026-04-17-NxCO2xMyco_Yadi.csv")
Yadi_2_final <- make_final("2026-04-19-NxCO2xMyco_Yadi.csv")
Yadi_2_resp_final <- make_final("2026-04-19-NxCO2xMyco_Yadi_resp.csv")
Yadi_3_final <- make_final("2026-04-20-NxCO2xMyco_Yadi.csv")
Yadi_3_resp_final<- make_final("2026-04-20-NxCO2xMyco_Yadi_resp.csv")
Yadi_4_final <- make_final("2026-05-26-sneha_yadi_NxCO2xMyco.csv")


# Albert

setwd("~/Library/CloudStorage/OneDrive-TexasTechUniversity/Experiment III/ACi/Albert")


Albert_1_resp_final <- make_final("2026-04-19_NxCO2xMyco_Albert_resp.csv")
Albert_1_final <- make_final("2026-04-17-NxCO2xMyco_albert.csv")
Albert_2_final <- make_final("2026-04-19-NxCO2xMyco_Albert.csv")
Albert_2_resp_final <- make_final("2026-04-20-NxCO2xMyco_Albert_resp.csv")
Albert_3_final <- make_final("2026-04-20-NxCO2xMyco_albert.csv")
Albert_3_resp_final<- make_final("2026-05-26-NxCO2xMyco_albert_resp.csv")
Albert_4_final <- make_final("2026-05-26-Albert_NxCO2xMyco.csv")


# merging all of the cleaned csv files for A/Ci 

A_ci_merge <- rbind(Ozzie_1_final, Stan_1_final, Stan_2_final, Stan_3_final, Albert_1_final, Albert_2_final, Albert_3_final, Albert_4_final, Gibson_1_final, Gibson_2_final, Yadi_1_final, Yadi_2_final, Yadi_3_final, Yadi_4_final)

unique(A_ci_merge$id)

# The plant ID names had "_" in their name, so removing them
A_ci_merge$id <- gsub("_", "", A_ci_merge$id)

# merging all of the cleaned csv files for dark respiration

resp_merge <- rbind(Albert_1_resp_final, Albert_2_resp_final, Albert_3_resp_final, Yadi_1_resp_final, Yadi_2_resp_final, Yadi_3_resp_final, 
                    Stan_1_resp_final, Gibson_1_resp_final, Gibson_2_resp_final)

unique(resp_merge$id)

# The plant ID names had "_" in their name, so removing them
resp_merge$id <- gsub("_", "", resp_merge$id)

# doing mean of dark respiration
resp_merge_clean <- resp_merge %>%
  group_by(id) %>%
  summarise(resp = mean(A)) %>%
  filter(!id %in% c("LCLNMyco5true", "LCLNMyco5true1"))

# HCHN Myco 1

HCHNMyco1 <- fitaci(filter(A_ci_merge, id == "HCHNMyco1"),
                    varnames = list(ALEAF="A",
                                    Tleaf= "Tleaf",
                                    Ci= "Ci",
                                    PPFD= "Qin"),
                    fitTPU = T,
                    Tcorrect= TRUE)

summary(HCHNMyco1)
plot(HCHNMyco1)
HCHNMyco1_coef <- t(coef(HCHNMyco1)) 

# HCHN Myco 2
# maybe needs some adjustment

HCHNMyco2 <- fitaci(filter(A_ci_merge, id == "HCHNMyco2"),
                    varnames = list(ALEAF="A",
                                    Tleaf= "Tleaf",
                                    Ci= "Ci",
                                    PPFD= "Qin"),
                    fitTPU = T,
                    Tcorrect= TRUE)

summary(HCHNMyco2)
plot(HCHNMyco2)
HCHNMyco2_coef <- t(coef(HCHNMyco2)) 

# HCHN Myco 3

HCHNMyco3 <- fitaci(filter(A_ci_merge, id == "HCHNMyco3"),
                    varnames = list(ALEAF="A",
                                    Tleaf= "Tleaf",
                                    Ci= "Ci",
                                    PPFD= "Qin"),
                    fitTPU = T,
                    Tcorrect= TRUE)

summary(HCHNMyco3)
plot(HCHNMyco3)
HCHNMyco3_coef <- t(coef(HCHNMyco3)) 

# HCHN Myco 4

HCHNMyco4 <- fitaci(filter(A_ci_merge, id == "HCHNMyco4"),
                    varnames = list(ALEAF="A",
                                    Tleaf= "Tleaf",
                                    Ci= "Ci",
                                    PPFD= "Qin"),
                    fitTPU = T,
                    Tcorrect= TRUE)

summary(HCHNMyco4)
plot(HCHNMyco4)
HCHNMyco4_coef <- t(coef(HCHNMyco4)) 

# HCHN Myco 5
# Jmax is weirdly high (should I drop?)
# Nick said not to drop it

HCHNMyco5 <- fitaci(filter(A_ci_merge, id == "HCHNMyco5again"),
                    varnames = list(ALEAF="A",
                                    Tleaf= "Tleaf",
                                    Ci= "Ci",
                                    PPFD= "Qin"),
                    fitTPU = F,
                    Tcorrect= TRUE)

summary(HCHNMyco5)
plot(HCHNMyco5)
HCHNMyco5_coef <- t(coef(HCHNMyco5)) 

# HCHN Myco 6

HCHNMyco6 <- fitaci(filter(A_ci_merge, id == "HCHNMyco6"),
                    varnames = list(ALEAF="A",
                                    Tleaf= "Tleaf",
                                    Ci= "Ci",
                                    PPFD= "Qin"),
                    fitTPU = T,
                    Tcorrect= TRUE)

summary(HCHNMyco6)
plot(HCHNMyco6)
HCHNMyco6_coef <- t(coef(HCHNMyco6)) 

# HCHN Myco 7

HCHNMyco7 <- fitaci(filter(A_ci_merge, id == "HCHNMyco7"),
                    varnames = list(ALEAF="A",
                                    Tleaf= "Tleaf",
                                    Ci= "Ci",
                                    PPFD= "Qin"),
                    fitTPU = T,
                    Tcorrect= TRUE)

summary(HCHNMyco7)
plot(HCHNMyco7)
HCHNMyco7_coef <- t(coef(HCHNMyco7)) 

# HCHN Myco 8

HCHNMyco8 <- fitaci(filter(A_ci_merge, id == "HCHNMyco8"),
                    varnames = list(ALEAF="A",
                                    Tleaf= "Tleaf",
                                    Ci= "Ci",
                                    PPFD= "Qin"),
                    fitTPU = T,
                    Tcorrect= TRUE)

summary(HCHNMyco8)
plot(HCHNMyco8)
HCHNMyco8_coef <- t(coef(HCHNMyco8)) 


# HCLN Myco 1

HCLNMyco1 <- fitaci(filter(A_ci_merge, id == "HCLNMyco1"),
                    varnames = list(ALEAF="A",
                                    Tleaf= "Tleaf",
                                    Ci= "Ci",
                                    PPFD= "Qin"),
                    fitTPU = T,
                    Tcorrect= TRUE)

summary(HCLNMyco1)
plot(HCLNMyco1)
HCLNMyco1_coef <- t(coef(HCLNMyco1)) 

# HCLN Myco 2

HCLNMyco2 <- fitaci(filter(A_ci_merge, id == "HCLNMyco2"),
                    varnames = list(ALEAF="A",
                                    Tleaf= "Tleaf",
                                    Ci= "Ci",
                                    PPFD= "Qin"),
                    fitTPU = T,
                    Tcorrect= T,
                    citransition = 270)

summary(HCLNMyco2)
plot(HCLNMyco2)
HCLNMyco2_coef <- t(coef(HCLNMyco2)) 

# HCLN Myco 3

HCLNMyco3 <- fitaci(filter(A_ci_merge, id == "HCLNMyco3"),
                    varnames = list(ALEAF="A",
                                    Tleaf= "Tleaf",
                                    Ci= "Ci",
                                    PPFD= "Qin"),
                    fitTPU = T,
                    Tcorrect= T,
                    citransition = 600)

summary(HCLNMyco3)
plot(HCLNMyco3)
HCLNMyco3_coef <- t(coef(HCLNMyco3))

# HCLN Myco 4

HCLNMyco4 <- fitaci(filter(A_ci_merge, id == "HCLNMyco4"),
                    varnames = list(ALEAF="A",
                                    Tleaf= "Tleaf",
                                    Ci= "Ci",
                                    PPFD= "Qin"),
                    fitTPU = T,
                    Tcorrect= T,
                    citransition = 450)

summary(HCLNMyco4)
plot(HCLNMyco4)
HCLNMyco4_coef <- t(coef(HCLNMyco4))

# HCLN Myco 5
# Jmax is high

HCLNMyco5 <- fitaci(filter(A_ci_merge, id == "HCLNMyco5"),
                    varnames = list(ALEAF="A",
                                    Tleaf= "Tleaf",
                                    Ci= "Ci",
                                    PPFD= "Qin"),
                    fitTPU = T,
                    Tcorrect= T,
                    citransition = 500)

summary(HCLNMyco5)
plot(HCLNMyco5)
HCLNMyco5_coef <- t(coef(HCLNMyco5))

# HCLN Myco 6

HCLNMyco6 <- fitaci(filter(A_ci_merge, id == "HCLNMyco6"),
                    varnames = list(ALEAF="A",
                                    Tleaf= "Tleaf",
                                    Ci= "Ci",
                                    PPFD= "Qin"),
                    fitTPU = T,
                    Tcorrect= T,
                    citransition = 450)

summary(HCLNMyco6)
plot(HCLNMyco6)
HCLNMyco6_coef <- t(coef(HCLNMyco6))

# HCLN Myco 7

HCLNMyco7 <- fitaci(filter(A_ci_merge, id == "HCLNMyco7"),
                    varnames = list(ALEAF="A",
                                    Tleaf= "Tleaf",
                                    Ci= "Ci",
                                    PPFD= "Qin"),
                    fitTPU = T,
                    Tcorrect= T)

summary(HCLNMyco7)
plot(HCLNMyco7)
HCLNMyco7_coef <- t(coef(HCLNMyco7))

# HCLN Myco 8

HCLNMyco8 <- fitaci(filter(A_ci_merge, id == "HCLNMyco8"),
                    varnames = list(ALEAF="A",
                                    Tleaf= "Tleaf",
                                    Ci= "Ci",
                                    PPFD= "Qin"),
                    fitTPU = T,
                    Tcorrect= T)

summary(HCLNMyco8)
plot(HCLNMyco8)
HCLNMyco8_coef <- t(coef(HCLNMyco8))

# HCHN No Myco 1

HCHNNoM1 <- fitaci(filter(A_ci_merge, id == "HCHNNoM1"),
                    varnames = list(ALEAF="A",
                                    Tleaf= "Tleaf",
                                    Ci= "Ci",
                                    PPFD= "Qin"),
                    fitTPU = T,
                    Tcorrect= T)

summary(HCHNNoM1)
plot(HCHNNoM1)
HCHNNoM1_coef <- t(coef(HCHNNoM1))

# HCHN No Myco 2

HCHNNoM2 <- fitaci(filter(A_ci_merge, id == "HCHNNoM2"),
                   varnames = list(ALEAF="A",
                                   Tleaf= "Tleaf",
                                   Ci= "Ci",
                                   PPFD= "Qin"),
                   fitTPU = T,
                   Tcorrect= T)

summary(HCHNNoM2)
plot(HCHNNoM2)
HCHNNoM2_coef <- t(coef(HCHNNoM2))

# HCHN No Myco 3

HCHNNoM3 <- fitaci(filter(A_ci_merge, id == "HCHNNoM3"),
                   varnames = list(ALEAF="A",
                                   Tleaf= "Tleaf",
                                   Ci= "Ci",
                                   PPFD= "Qin"),
                   fitTPU = T,
                   Tcorrect= T)

summary(HCHNNoM3)
plot(HCHNNoM3)
HCHNNoM3_coef <- t(coef(HCHNNoM3))

# HCHN No Myco 4

HCHNNoM4 <- fitaci(filter(A_ci_merge, id == "HCHNNoM4"),
                   varnames = list(ALEAF="A",
                                   Tleaf= "Tleaf",
                                   Ci= "Ci",
                                   PPFD= "Qin"),
                   fitTPU = T,
                   Tcorrect= T)

summary(HCHNNoM4)
plot(HCHNNoM4)
HCHNNoM4_coef <- t(coef(HCHNNoM4))

# HCHN No Myco 5

HCHNNoM5 <- fitaci(filter(A_ci_merge, id == "HCHNNoM5"),
                   varnames = list(ALEAF="A",
                                   Tleaf= "Tleaf",
                                   Ci= "Ci",
                                   PPFD= "Qin"),
                   fitTPU = T,
                   Tcorrect= T,
                   citransition = 560)

summary(HCHNNoM5)
plot(HCHNNoM5)
HCHNNoM5_coef <- t(coef(HCHNNoM5))

# HCHN No Myco 6

HCHNNoM6 <- fitaci(filter(A_ci_merge, id == "HCHNNoM6"),
                   varnames = list(ALEAF="A",
                                   Tleaf= "Tleaf",
                                   Ci= "Ci",
                                   PPFD= "Qin"),
                   fitTPU = T,
                   Tcorrect= T)

summary(HCHNNoM6)
plot(HCHNNoM6)
HCHNNoM6_coef <- t(coef(HCHNNoM6))

# HCHN No Myco 7

HCHNNoM7 <- fitaci(filter(A_ci_merge, id == "HCHNNoM7again"),
                   varnames = list(ALEAF="A",
                                   Tleaf= "Tleaf",
                                   Ci= "Ci",
                                   PPFD= "Qin"),
                   fitTPU = T,
                   Tcorrect= T,
                   citransition = 450)

summary(HCHNNoM7)
plot(HCHNNoM7)
HCHNNoM7_coef <- t(coef(HCHNNoM7))

# HCHN No Myco 8

HCHNNoM8 <- fitaci(filter(A_ci_merge, id == "HCHNNoM8"),
                   varnames = list(ALEAF="A",
                                   Tleaf= "Tleaf",
                                   Ci= "Ci",
                                   PPFD= "Qin"),
                   fitTPU = T,
                   Tcorrect= T)

summary(HCHNNoM8)
plot(HCHNNoM8)
HCHNNoM8_coef <- t(coef(HCHNNoM8))
                   

# HCLN No Myco 1

HCLNNoM1 <- fitaci(filter(A_ci_merge, id == "HCLNNoM1"),
                   varnames = list(ALEAF="A",
                                   Tleaf= "Tleaf",
                                   Ci= "Ci",
                                   PPFD= "Qin"),
                   fitTPU = T,
                   Tcorrect= T)

summary(HCLNNoM1)
plot(HCLNNoM1)
HCLNNoM1_coef <- t(coef(HCLNNoM1))

# HCLN No Myco 2

HCLNNoM2 <- fitaci(filter(A_ci_merge, id == "HCLNNoM2"),
                   varnames = list(ALEAF="A",
                                   Tleaf= "Tleaf",
                                   Ci= "Ci",
                                   PPFD= "Qin"),
                   fitTPU = T,
                   Tcorrect= T)

summary(HCLNNoM2)
plot(HCLNNoM2)
HCLNNoM2_coef <- t(coef(HCLNNoM2))

# HCLN No Myco 3
# needs adjustment

HCLNNoM3 <- fitaci(filter(A_ci_merge, id == "HCLNNoM3"),
                   varnames = list(ALEAF="A",
                                   Tleaf= "Tleaf",
                                   Ci= "Ci",
                                   PPFD= "Qin"),
                   fitTPU = T,
                   Tcorrect= T,
                   citransition = 470)

summary(HCLNNoM3)
plot(HCLNNoM3)
HCLNNoM3_coef <- t(coef(HCLNNoM3))

# HCLN No Myco 4

HCLNNoM4 <- fitaci(filter(A_ci_merge, id == "HCLNNoM4"),
                   varnames = list(ALEAF="A",
                                   Tleaf= "Tleaf",
                                   Ci= "Ci",
                                   PPFD= "Qin"),
                   fitTPU = T,
                   Tcorrect= T)

summary(HCLNNoM4)
plot(HCLNNoM4)
HCLNNoM4_coef <- t(coef(HCLNNoM4))

# HCLN No Myco 5

HCLNNoM5 <- fitaci(filter(A_ci_merge, id == "HCLNNoM5"),
                   varnames = list(ALEAF="A",
                                   Tleaf= "Tleaf",
                                   Ci= "Ci",
                                   PPFD= "Qin"),
                   fitTPU = T,
                   Tcorrect= T)

summary(HCLNNoM5)
plot(HCLNNoM5)
HCLNNoM5_coef <- t(coef(HCLNNoM5))

# HCLN No Myco 6

HCLNNoM6 <- fitaci(filter(A_ci_merge, id == "HCLNNoM6"),
                   varnames = list(ALEAF="A",
                                   Tleaf= "Tleaf",
                                   Ci= "Ci",
                                   PPFD= "Qin"),
                   fitTPU = T,
                   Tcorrect= T)

summary(HCLNNoM6)
plot(HCLNNoM6)
HCLNNoM6_coef <- t(coef(HCLNNoM6))

# HCLN No Myco 7

HCLNNoM7 <- fitaci(filter(A_ci_merge, id == "HCLNNoM7"),
                   varnames = list(ALEAF="A",
                                   Tleaf= "Tleaf",
                                   Ci= "Ci",
                                   PPFD= "Qin"),
                   fitTPU = T,
                   Tcorrect= T)

summary(HCLNNoM7)
plot(HCLNNoM7)
HCLNNoM7_coef <- t(coef(HCLNNoM7))

# HCLN No Myco 8

HCLNNoM8 <- fitaci(filter(A_ci_merge, id == "HCLNNoM8"),
                   varnames = list(ALEAF="A",
                                   Tleaf= "Tleaf",
                                   Ci= "Ci",
                                   PPFD= "Qin"),
                   fitTPU = T,
                   Tcorrect= T,
                   citransition = 270)

summary(HCLNNoM8)
plot(HCLNNoM8)
HCLNNoM8_coef <- t(coef(HCLNNoM8))

# LCHN Myco 1
# needs adjustment

LCHNMyco1 <- fitaci(filter(A_ci_merge, id == "LCHNMyco1"),
                   varnames = list(ALEAF="A",
                                   Tleaf= "Tleaf",
                                   Ci= "Ci",
                                   PPFD= "Qin"),
                   fitTPU = T,
                   Tcorrect= T,
                   citransition = 350)

summary(LCHNMyco1)
plot(LCHNMyco1)
LCHNMyco1_coef <- t(coef(LCHNMyco1))

# LCHN Myco 2

LCHNMyco2 <- fitaci(filter(A_ci_merge, id == "LCHNMyco2"),
                    varnames = list(ALEAF="A",
                                    Tleaf= "Tleaf",
                                    Ci= "Ci",
                                    PPFD= "Qin"),
                    fitTPU = T,
                    Tcorrect= T)

summary(LCHNMyco2)
plot(LCHNMyco2)
LCHNMyco2_coef <- t(coef(LCHNMyco2))

# LCHNMyco 3 plant died due to mold

# LCHN Myco 4

LCHNMyco4 <- fitaci(filter(A_ci_merge, id == "LCHNMyco4"),
                    varnames = list(ALEAF="A",
                                    Tleaf= "Tleaf",
                                    Ci= "Ci",
                                    PPFD= "Qin"),
                    fitTPU = T,
                    Tcorrect= T)

summary(LCHNMyco4)
plot(LCHNMyco4)
LCHNMyco4_coef <- t(coef(LCHNMyco4))

# LCHN Myco 5

LCHNMyco5 <- fitaci(filter(A_ci_merge, id == "LCHNMyco5"),
                    varnames = list(ALEAF="A",
                                    Tleaf= "Tleaf",
                                    Ci= "Ci",
                                    PPFD= "Qin"),
                    fitTPU = T,
                    Tcorrect= T)

summary(LCHNMyco5)
plot(LCHNMyco5)
LCHNMyco5_coef <- t(coef(LCHNMyco5))

# LCHN Myco 6

LCHNMyco6 <- fitaci(filter(A_ci_merge, id == "LCHNMyco6"),
                    varnames = list(ALEAF="A",
                                    Tleaf= "Tleaf",
                                    Ci= "Ci",
                                    PPFD= "Qin"),
                    fitTPU = T,
                    Tcorrect= T)

summary(LCHNMyco6)
plot(LCHNMyco6)
LCHNMyco6_coef <- t(coef(LCHNMyco6))

# LCHN Myco 7

LCHNMyco7 <- fitaci(filter(A_ci_merge, id == "LCHNMyco7"),
                    varnames = list(ALEAF="A",
                                    Tleaf= "Tleaf",
                                    Ci= "Ci",
                                    PPFD= "Qin"),
                    fitTPU = T,
                    Tcorrect= T)

summary(LCHNMyco7)
plot(LCHNMyco7)
LCHNMyco7_coef <- t(coef(LCHNMyco7))

# LCHN Myco 8

LCHNMyco8 <- fitaci(filter(A_ci_merge, id == "LCHNMyco8"),
                    varnames = list(ALEAF="A",
                                    Tleaf= "Tleaf",
                                    Ci= "Ci",
                                    PPFD= "Qin"),
                    fitTPU = T,
                    Tcorrect= T)

summary(LCHNMyco8)
plot(LCHNMyco8)
LCHNMyco8_coef <- t(coef(LCHNMyco8))

# LCHN No Myco 1

LCHNNoM1 <- fitaci(filter(A_ci_merge, id == "LCHNNoM1"),
                    varnames = list(ALEAF="A",
                                    Tleaf= "Tleaf",
                                    Ci= "Ci",
                                    PPFD= "Qin"),
                    fitTPU = T,
                    Tcorrect= T)

summary(LCHNNoM1)
plot(LCHNNoM1)
LCHNNoM1_coef <- t(coef(LCHNNoM1))

# LCHN No Myco 2

LCHNNoM2 <- fitaci(filter(A_ci_merge, id == "LCHNNoM2"),
                   varnames = list(ALEAF="A",
                                   Tleaf= "Tleaf",
                                   Ci= "Ci",
                                   PPFD= "Qin"),
                   fitTPU = T,
                   Tcorrect= T)

summary(LCHNNoM2)
plot(LCHNNoM2)
LCHNNoM2_coef <- t(coef(LCHNNoM2))

# LCHN No Myco 3

LCHNNoM3 <- fitaci(filter(A_ci_merge, id == "LCHNNoM3"),
                   varnames = list(ALEAF="A",
                                   Tleaf= "Tleaf",
                                   Ci= "Ci",
                                   PPFD= "Qin"),
                   fitTPU = T,
                   Tcorrect= T)

summary(LCHNNoM3)
plot(LCHNNoM3)
LCHNNoM3_coef <- t(coef(LCHNNoM3))

# LCHN No Myco 4

LCHNNoM4 <- fitaci(filter(A_ci_merge, id == "LCHNNoM4"),
                   varnames = list(ALEAF="A",
                                   Tleaf= "Tleaf",
                                   Ci= "Ci",
                                   PPFD= "Qin"),
                   fitTPU = T,
                   Tcorrect= T)

summary(LCHNNoM4)
plot(LCHNNoM4)
LCHNNoM4_coef <- t(coef(LCHNNoM4))

# LCHN No Myco 5

LCHNNoM5 <- fitaci(filter(A_ci_merge, id == "LCHNNoM5"),
                   varnames = list(ALEAF="A",
                                   Tleaf= "Tleaf",
                                   Ci= "Ci",
                                   PPFD= "Qin"),
                   fitTPU = T,
                   Tcorrect= T)

summary(LCHNNoM5)
plot(LCHNNoM5)
LCHNNoM5_coef <- t(coef(LCHNNoM5))

# LCHN No Myco 6
# needs some adjustment

LCHNNoM6 <- fitaci(filter(A_ci_merge, id == "LCHNNoM6true"),
                   varnames = list(ALEAF="A",
                                   Tleaf= "Tleaf",
                                   Ci= "Ci",
                                   PPFD= "Qin"),
                   fitTPU = T,
                   Tcorrect= T)

summary(LCHNNoM6)
plot(LCHNNoM6)
LCHNNoM6_coef <- t(coef(LCHNNoM6))

# LCHN No Myco 7

LCHNNoM7 <- fitaci(filter(A_ci_merge, id == "LCHNNoM7"),
                   varnames = list(ALEAF="A",
                                   Tleaf= "Tleaf",
                                   Ci= "Ci",
                                   PPFD= "Qin"),
                   fitTPU = T,
                   Tcorrect= T)

summary(LCHNNoM7)
plot(LCHNNoM7)
LCHNNoM7_coef <- t(coef(LCHNNoM7))

# LCHN No Myco 8
# needs adjustment

LCHNNoM8 <- fitaci(filter(A_ci_merge, id == "LCHNNoM8"),
                   varnames = list(ALEAF="A",
                                   Tleaf= "Tleaf",
                                   Ci= "Ci",
                                   PPFD= "Qin"),
                   fitTPU = T,
                   Tcorrect= T)

summary(LCHNNoM8)
plot(LCHNNoM8)
LCHNNoM8_coef <- t(coef(LCHNNoM8))


# LCLN Myco 1

LCLNMyco1 <- fitaci(filter(A_ci_merge, id == "LCLNMyco1"),
                    varnames = list(ALEAF="A",
                                    Tleaf= "Tleaf",
                                    Ci= "Ci",
                                    PPFD= "Qin"),
                    fitTPU = T,
                    Tcorrect= T)

summary(LCLNMyco1)
plot(LCLNMyco1)
LCLNMyco1_coef <- t(coef(LCLNMyco1))

# LCLN Myco 2

LCLNMyco2 <- fitaci(filter(A_ci_merge, id == "LCLNMyco2"),
                    varnames = list(ALEAF="A",
                                    Tleaf= "Tleaf",
                                    Ci= "Ci",
                                    PPFD= "Qin"),
                    fitTPU = T,
                    Tcorrect= T)

summary(LCLNMyco2)
plot(LCLNMyco2)
LCLNMyco2_coef <- t(coef(LCLNMyco2))

# LCLN Myco 3

LCLNMyco3 <- fitaci(filter(A_ci_merge, id == "LCLNMyco3"),
                    varnames = list(ALEAF="A",
                                    Tleaf= "Tleaf",
                                    Ci= "Ci",
                                    PPFD= "Qin"),
                    fitTPU = T,
                    Tcorrect= T)

summary(LCLNMyco3)
plot(LCLNMyco3)
LCLNMyco3_coef <- t(coef(LCLNMyco3))

# LCLN Myco 4

LCLNMyco4 <- fitaci(filter(A_ci_merge, id == "LCLNMyco4"),
                    varnames = list(ALEAF="A",
                                    Tleaf= "Tleaf",
                                    Ci= "Ci",
                                    PPFD= "Qin"),
                    fitTPU = T,
                    Tcorrect= T)

summary(LCLNMyco4)
plot(LCLNMyco4)
LCLNMyco4_coef <- t(coef(LCLNMyco4))

# LCLN Myco 5

LCLNMyco5 <- fitaci(filter(A_ci_merge, id == "LCLNMyco5"),
                    varnames = list(ALEAF="A",
                                    Tleaf= "Tleaf",
                                    Ci= "Ci",
                                    PPFD= "Qin"),
                    fitTPU = T,
                    Tcorrect= T)

summary(LCLNMyco5)
plot(LCLNMyco5)
LCLNMyco5_coef <- t(coef(LCLNMyco5))

# LCLN Myco 6

LCLNMyco6 <- fitaci(filter(A_ci_merge, id == "LCLNMyco6"),
                    varnames = list(ALEAF="A",
                                    Tleaf= "Tleaf",
                                    Ci= "Ci",
                                    PPFD= "Qin"),
                    fitTPU = T,
                    Tcorrect= T)

summary(LCLNMyco6)
plot(LCLNMyco6)
LCLNMyco6_coef <- t(coef(LCLNMyco6))

# LCLN Myco 7

LCLNMyco7 <- fitaci(filter(A_ci_merge, id == "LCLNMyco7"),
                    varnames = list(ALEAF="A",
                                    Tleaf= "Tleaf",
                                    Ci= "Ci",
                                    PPFD= "Qin"),
                    fitTPU = T,
                    Tcorrect= T)

summary(LCLNMyco7)
plot(LCLNMyco7)
LCLNMyco7_coef <- t(coef(LCLNMyco7))

# LCLN Myco 8

LCLNMyco8 <- fitaci(filter(A_ci_merge, id == "LCLNMyco8"),
                    varnames = list(ALEAF="A",
                                    Tleaf= "Tleaf",
                                    Ci= "Ci",
                                    PPFD= "Qin"),
                    fitTPU = T,
                    Tcorrect= T)

summary(LCLNMyco8)
plot(LCLNMyco8)
LCLNMyco8_coef <- t(coef(LCLNMyco8))

# LCLN No Myco 1

LCLNNoM1 <- fitaci(filter(A_ci_merge, id == "LCLNNoM1", A > 0),
                   varnames = list(ALEAF = "A",
                                   Tleaf = "Tleaf",
                                   Ci    = "Ci",
                                   PPFD  = "Qin"),
                   fitTPU = T,
                   Tcorrect = T,
                   citransition = 370)

summary(LCLNNoM1)
plot(LCLNNoM1)
LCLNNoM1_coef <- t(coef(LCLNNoM1))

# LCLN No Myco 2

LCLNNoM2 <- fitaci(filter(A_ci_merge, id == "LCLNNoM2true1"),
                   varnames = list(ALEAF="A",
                                   Tleaf= "Tleaf",
                                   Ci= "Ci",
                                   PPFD= "Qin"),
                   fitTPU = T,
                   Tcorrect= T,
                   citransition = 350)

summary(LCLNNoM2)
plot(LCLNNoM2)
LCLNNoM2_coef <- t(coef(LCLNNoM2))

# LCLN No Myco 3

LCLNNoM3 <- fitaci(filter(A_ci_merge, id == "LCLNNoM3"),
                   varnames = list(ALEAF="A",
                                   Tleaf= "Tleaf",
                                   Ci= "Ci",
                                   PPFD= "Qin"),
                   fitTPU = T,
                   Tcorrect= T)

summary(LCLNNoM3)
plot(LCLNNoM3)
LCLNNoM3_coef <- t(coef(LCLNNoM3))

# LCLN No Myco 4

LCLNNoM4 <- fitaci(filter(A_ci_merge, id == "LCLNNoM4"),
                   varnames = list(ALEAF="A",
                                   Tleaf= "Tleaf",
                                   Ci= "Ci",
                                   PPFD= "Qin"),
                   fitTPU = T,
                   Tcorrect= T)

summary(LCLNNoM4)
plot(LCLNNoM4)
LCLNNoM4_coef <- t(coef(LCLNNoM4))

# LCLN No Myco 5

LCLNNoM5 <- fitaci(filter(A_ci_merge, id == "LCLNNoM5"),
                   varnames = list(ALEAF="A",
                                   Tleaf= "Tleaf",
                                   Ci= "Ci",
                                   PPFD= "Qin"),
                   fitTPU = T,
                   Tcorrect= T)

summary(LCLNNoM5)
plot(LCLNNoM5)
LCLNNoM5_coef <- t(coef(LCLNNoM5))

# LCLN No Myco 6

LCLNNoM6 <- fitaci(filter(A_ci_merge, id == "LCLNNoM6"),
                   varnames = list(ALEAF="A",
                                   Tleaf= "Tleaf",
                                   Ci= "Ci",
                                   PPFD= "Qin"),
                   fitTPU = T,
                   Tcorrect= T)

summary(LCLNNoM6)
plot(LCLNNoM6)
LCLNNoM6_coef <- t(coef(LCLNNoM6))

# LCLN No Myco 7

LCLNNoM7 <- fitaci(filter(A_ci_merge, id == "LCLNNoM7"),
                   varnames = list(ALEAF="A",
                                   Tleaf= "Tleaf",
                                   Ci= "Ci",
                                   PPFD= "Qin"),
                   fitTPU = T,
                   Tcorrect= T)

summary(LCLNNoM7)
plot(LCLNNoM7)
LCLNNoM7_coef <- t(coef(LCLNNoM7))

# LCLN No Myco 8

LCLNNoM8 <- fitaci(filter(A_ci_merge, id == "LCLNNoM8"),
                   varnames = list(ALEAF="A",
                                   Tleaf= "Tleaf",
                                   Ci= "Ci",
                                   PPFD= "Qin"),
                   fitTPU = T,
                   Tcorrect= T)

summary(LCLNNoM8)
plot(LCLNNoM8)
LCLNNoM8_coef <- t(coef(LCLNNoM8))


Vcmax_df <- rbind(HCHNMyco1_coef, HCHNMyco2_coef, HCHNMyco3_coef, HCHNMyco4_coef,
                  HCHNMyco5_coef, HCHNMyco6_coef, HCHNMyco6_coef, HCHNMyco7_coef,
                  HCHNMyco8_coef, HCHNNoM1_coef, HCHNNoM2_coef, HCHNNoM3_coef, HCHNNoM4_coef,
                  HCHNNoM5_coef, HCHNNoM6_coef, HCHNNoM7_coef, HCHNNoM8_coef,
                  HCLNMyco1_coef, HCLNMyco2_coef, HCLNMyco3_coef, HCLNMyco4_coef,
                  HCLNMyco5_coef, HCLNMyco6_coef, HCLNMyco7_coef, HCLNMyco8_coef,
                  HCLNNoM1_coef, HCLNNoM2_coef, HCLNNoM3_coef, HCLNNoM4_coef,
                  HCLNNoM5_coef, HCLNNoM6_coef, HCLNNoM7_coef, HCLNNoM8_coef,
                LCHNMyco1_coef, LCHNMyco2_coef, LCHNMyco4_coef, LCHNMyco5_coef,
                LCHNMyco6_coef, LCHNMyco7_coef, LCHNMyco8_coef, LCHNNoM1_coef,
                LCHNNoM2_coef, LCHNNoM3_coef, LCHNNoM4_coef, LCHNNoM5_coef,LCHNNoM6_coef,
                LCHNNoM8_coef, LCHNNoM7_coef, LCLNMyco1_coef, LCLNMyco2_coef,
                LCLNMyco3_coef, LCLNMyco4_coef, LCLNMyco5_coef, LCLNMyco6_coef,
                LCLNMyco7_coef, LCLNMyco8_coef, LCLNNoM1_coef, LCLNNoM2_coef,
                LCLNNoM3_coef, LCLNNoM4_coef, LCLNNoM5_coef, LCLNNoM6_coef,
                LCLNNoM7_coef, LCLNNoM8_coef)

Sample <- c ("HCHN Myco1", "HCHN Myco2", "HCHN Myco3", "HCHN Myco4",
             "HCHN Myco5", "HCHN Myco6", "HCHN Myco6", "HCHN Myco7",
             "HCHN Myco8", "HCHN NoM1", "HCHN NoM2", "HCHN NoM3", "HCHN NoM4",
             "HCHN NoM5", "HCHN NoM6", "HCHN NoM7", "HCHN NoM8",
             "HCLN Myco1", "HCLN Myco2", "HCLN Myco3", "HCLN Myco4",
             "HCLN Myco5", "HCLN Myco6", "HCLN Myco7" , "HCLN Myco8",
             "HCLN NoM1", "HCLN NoM2", "HCLN NoM3", "HCLN NoM4",
             "HCLN NoM5", "HCLN NoM6", "HCLN NoM7", "HCLN NoM8",
             "LCHN Myco1", "LCHN Myco2", "LCHN Myco4", "LCHN Myco5",
             "LCHN Myco6", "LCHN Myco7", "LCHN Myco8", "LCHN NoM1",
             "LCHN NoM2", "LCHN NoM3", "LCHN NoM4", "LCHN NoM5","LCHN NoM6",
             "LCHN NoM8", "LCHN NoM7", "LCLN Myco1", "LCLN Myco2",
             "LCLN Myco3", "LCLN Myco4", "LCLN Myco5", "LCLN Myco6",
             "LCLN Myco7", "LCLN Myco8", "LCLN NoM1", "LCLN NoM2",
             "LCLN NoM3", "LCLN NoM4", "LCLN NoM5", "LCLN NoM6",
             "LCLN NoM7", "LCLN NoM8")

Vcmax_df_1 <- cbind(Sample, Vcmax_df)    

setwd("~/Library/CloudStorage/OneDrive-TexasTechUniversity/Experiment III/ACi")

# Loading metadata
metadata <- read_excel("Metadata.xlsx")

# merging metadata with vcmax, jmax, Rd, TPu dataset
merge_stats <-merge(Vcmax_df_1, metadata, by = "Sample") 

# making vcmax, jmax as numeric
merge_stats$Vcmax <- as.numeric(merge_stats$Vcmax)
merge_stats$Jmax <- as.numeric(merge_stats$Jmax)

# fit model based on hypothesis (Vcmax), doing sqrt because log transformation, glm did not work
Vcmax_lmer <- lmer(Vcmax ~ CO2_treatment * Nutrient_treatment * AMF + (1|Block_ID), data = merge_stats)

Anova(Vcmax_lmer)
summary(Vcmax_lmer)
hist(residuals(Vcmax_lmer))
shapiro.test(residuals(Vcmax_lmer)) 

plot(Vcmax_lmer)

# emmeans

emmeans(Vcmax_lmer, pairwise ~ CO2_treatment, adjust = "tukey") # Vcmax was higher with ambient CO2
emmeans(Vcmax_lmer, pairwise ~ AMF, adjust = "tukey") # Vcmax was significantly higher when AMF is present in all treatments


# emmeans for Vcmax

em_Vcmax <- as.data.frame(emmeans(Vcmax_lmer, ~AMF * Nutrient_treatment * CO2_treatment))

# ggplot for Vcmax

Treatment_Vcmax_w_sig <- ggplot(merge_stats, aes(y = Vcmax, x = AMF , color = AMF)) + 
  geom_jitter(width = 0.2, size = 2, alpha = 0.8)  +
  geom_point(data = em_Vcmax, aes(y = emmean, x = AMF), 
             shape = 18, size = 3, color = "black", inherit.aes = FALSE) +
  geom_errorbar(data = em_Vcmax, aes(x = AMF, y = emmean, ymin = emmean - SE, ymax = emmean + SE),  width = 0.15, color = "black", alpha = 0.6, inherit.aes = FALSE) +  
  scale_color_manual(values = c("red", "dodgerblue")) +  
  labs(x = "AMF Presence/Absence", 
       y = expression(Vcmax[25]~ "(µmol"~m^{-2}~s^{-1}*")")) +
  facet_grid(CO2_treatment ~ Nutrient_treatment,  
             labeller = labeller(CO2_treatment = c("Elevated CO2" = "Elevated CO2", "Ambient CO2" = "Ambient CO2"),
                                 Nutrient_treatment = c("Low Nitrogen" = "Low Nitrogen",  "High Nitrogen" = "High Nitrogen"))) +  
  theme_bw(base_size = 17)  

ggsave("Treatment_Vcmax_w_sig.jpg", width = 10, height = 7, units = "in", dpi = 600)


# Jmax

# fit model based on hypothesis (Jmax)
Jmax_lmer <- lmer(log(Jmax) ~ CO2_treatment * Nutrient_treatment * AMF + (1|Block_ID),  data = merge_stats)

Anova(Jmax_lmer)
summary(Jmax_lmer)
hist(residuals(Jmax_lmer))
shapiro.test(residuals(Jmax_lmer)) 

plot(Jmax_lmer)

#emmeans

emmeans(Jmax_lmer, pairwise ~ CO2_treatment, adjust = "tukey") # Jmax was higher in Ambient CO2
emmeans(Jmax_lmer, pairwise ~ AMF * CO2_treatment , adjust = "tukey") # AMF presence with elevated CO2 significantly increased Jmax than without its presence
emmeans(Jmax_lmer, pairwise ~ AMF , adjust = "tukey") # Jmax was significantly higher with AMF

# emmeans for Jmax

em_Jmax <- as.data.frame(emmeans(Jmax_lmer, ~AMF * Nutrient_treatment * CO2_treatment, type= "response"))

# ggplot for Jmax

Treatment_Jmax <- ggplot(merge_stats, aes(y = Jmax, x = AMF, color = AMF)) +
  geom_jitter( width = 0.2,size = 2,alpha = 0.8) +
  geom_point(data = em_Jmax, aes(y = response, x = AMF), shape = 18, size = 3, color = "black",inherit.aes = FALSE) +
  geom_errorbar(data = em_Jmax, aes(x = AMF, y = response, ymin = response - SE, ymax = response + SE), width = 0.15, color = "black", alpha = 0.6, inherit.aes = FALSE) +
  scale_color_manual(values = c("red", "dodgerblue")) +
  labs(x = "AMF Presence/Absence",
    y = expression(Jmax[25] ~ "(µmol" ~ m^{-2} ~ s^{-1} * ")")) +
  facet_grid(CO2_treatment ~ Nutrient_treatment,
    labeller = labeller(CO2_treatment = c("Elevated CO2" = "Elevated CO2", "Ambient CO2" = "Ambient CO2"),
      Nutrient_treatment = c("Low Nitrogen" = "Low Nitrogen", "High Nitrogen" = "High Nitrogen"))) +
  theme_bw(base_size = 17)

ggsave("Treatment_Jmax.jpg", width = 10, height = 7, units = "in", dpi = 600)


# Rd

# Loading metadata for Rd
# LCHN NoM 8 was is missing Rd data
Metadata <- read_excel("Metadata_exp3.xlsx")

# merging Rd data with metadata
merge_resp <- merge(resp_merge_clean, Metadata, by = "id") 

# fit model based on hypothesis (Rd)
Rd_lmer <- lmer(resp ~ CO2_treatment * Nutrient_treatment * AMF + (1|Block_ID), data = merge_resp)

Anova(Rd_lmer)
summary(Rd_lmer)
hist(residuals(Rd_lmer))
shapiro.test(residuals(Rd_lmer)) 

plot(Rd_lmer)

# No significant effect found

# emmeans for Rd

em_Rd <- as.data.frame(emmeans(Rd_lmer, ~AMF * Nutrient_treatment * CO2_treatment))

# ggplot for Rd

Treatment_Rd <- ggplot(merge_resp, aes(y = resp, x = AMF , color = AMF)) + 
  geom_jitter(width = 0.2, size = 2, alpha = 0.8)  +
  geom_point(data = em_Rd, aes(y = emmean, x = AMF), 
             shape = 18, size = 3, color = "black", inherit.aes = FALSE) +
  geom_errorbar(data = em_Rd, aes(x = AMF, y = emmean,ymin = emmean - SE, ymax = emmean + SE),  width = 0.15, color = "black", alpha = 0.6, inherit.aes = FALSE) +  
  scale_color_manual(values = c("red", "dodgerblue")) +  
  labs(x = "AMF Presence/Absence", 
       y =expression(R[d] ~ "(µmol"~m^{-2}~s^{-1}*")")) +
  facet_grid(CO2_treatment ~ Nutrient_treatment,
             labeller = labeller(CO2_treatment = c("Elevated CO2" = "Elevated CO2", "Ambient CO2" = "Ambient CO2"),
                                 Nutrient_treatment = c("Low Nitrogen" = "Low Nitrogen", "High Nitrogen" = "High Nitrogen"))) +  
  theme_bw(base_size = 17)  

ggsave("Treatment_Rd.jpg", width = 10, height = 7, units = "in", dpi = 600)


# Anet

Anet_data <- A_ci_merge %>% filter(CO2_r > 419.5 & CO2_r < 420.5)

# Eliminating extra id, group by id, summarising mean A, mean gsw and Tair
Anet_data <- Anet_data %>%
  group_by(id) %>%
  summarise(A = mean(A),
            gsw = mean(gsw),
            Tair = first(Tleaf)) %>%
  filter(A > 0) %>%
  arrange(id)  %>% filter(!id %in% c("LCHNNoM6", "LCHNMyco5true", "HCHNMyco1true", "HCLNNoM3true", "LCHNMyco5true1",
                                     "HCLNMyco2true", "HCHNMyco7again", "LCLNNoM2", "HCHNMyco5", "LCHNNoM6true", "HCHNNoM7"))


metadata_exp2 <- read_excel("metadata_exp.xlsx")

metadata_exp2 <- metadata_exp2 %>% mutate(id = str_replace_all(id, " ", ""))

# merging Anet data with metadata
merge_Anet <- merge(Anet_data, metadata_exp2, by = "id") 

# fit model based on hypothesis (Anet)
Anet_lmer <- lmer(A ~ Light_treatment * Nutrient_treatment * AMF + (1|Block_ID), data = merge_Anet)

Anova(Anet_lmer)
summary(Anet_lmer)
hist(residuals(Anet_lmer))
shapiro.test(residuals(Anet_lmer)) # normal distribution

plot(Anet_lmer)

emmeans(Anet_lmer, pairwise ~ Nutrient_treatment, adjust = "tukey")
emmeans(Anet_lmer, pairwise ~ Light_treatment, adjust = "tukey")
emmeans(Anet_lmer, pairwise ~ Light_treatment * Nutrient_treatment, adjust = "tukey")

# emmeans for Anet

em_Anet <- as.data.frame(emmeans(Anet_lmer, ~AMF * Nutrient_treatment * Light_treatment))

# ggplot for Anet

Treatment_Anet <- ggplot(merge_Anet, aes(y = A, x = AMF , color = AMF)) + 
  geom_jitter(width = 0.2, size = 2, alpha = 0.8)  +
  geom_point(data = em_Anet, aes(y = emmean, x = AMF), 
             shape = 18, size = 3, color = "black", inherit.aes = FALSE) +
  geom_errorbar(data = em_Anet, aes(x = AMF, y = emmean,ymin = emmean - SE, ymax = emmean + SE),  width = 0.15, color = "black", alpha = 0.6, inherit.aes = FALSE) +  
  scale_color_manual(values = c("red", "dodgerblue")) +  
  labs(x = "AMF Presence/Absence", 
       y = expression(A[net] ~ "(µmol"~m^{-2}~s^{-1}*")")) +
  facet_grid(Light_treatment ~ Nutrient_treatment,  
             labeller = labeller(Light_treatment = c("High light" = "High light", "Low light" = "Low light"),
                                 Nutrient_treatment = c("Low nitrogen" = "Low N",  "High nitrogen" = "High N"))) +  theme_bw(base_size = 22)  

ggsave("Treatment_Anet.jpg", width = 10, height = 7, units = "in", dpi = 600)





