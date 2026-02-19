library(ggplot2)
library(tidyverse)
library(readxl)
library(mvabund)

# Download data
DietData <- read_excel("Data/DietData.xlsx")
DietData_env <- read_excel("Data/DietData_env.xlsx")

### Running manyglm and anova on diet data

# Create combined environmental and diet matrix dataset
DietDataWhole <- cbind(DietData_env,DietData)

# Convert diet matrix into mvabund format
mva_DietData <- mvabund(DietDataWhole[,27:37])

# Run manyglm
Dietglm <- manyglm(mva_DietData ~ SpeciesCode,
                   data = DietDataWhole,
                   family = "negative.binomial")

Dietglmanova <- anova(Dietglm, p.uni = "adjusted")

# Attempt manyglm & anova with unknown-removed diets

colSums(DietData) < 10

# remove unknown, junk, and low count (<10) prey taxa
filtered_DietDataWhole <- DietDataWhole %>%  
  filter(LifeStage == "YoY") %>%
  select(-c(Unknown, unk_invert,unk_plant_material, unknown_fish)) %>% # remove unknowns
  select(-c(sand_gravel_total, trash, detritus_total, seed)) %>% #junk
  select(-c(Decapoda, Odonata, Oligochaeta, Bivalvia, Annelida, Thysanoptera, Siphonaptera, Neuroptera, Slug, Salmoniformes, Collembola, Orthoptera)) #organisms

mva_filtDietData <- mvabund(filtered_DietDataWhole[,27:43])

filtDietglm <- manyglm(mva_filtDietData ~ SpeciesCode,
                       data = filtered_DietDataWhole,
                       family = "negative.binomial")

filtDietglmanova <- anova(filtDietglm, p.uni = "adjusted")