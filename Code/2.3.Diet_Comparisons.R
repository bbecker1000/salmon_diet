# last updated: 06/03/2026
# notes:
  # many glm - sig. diff. taxa inc. Coleoptera, Diptera, Hemiptera, Trichoptera, Araneae, Hymnenoptera, Psocodea, & Plecoptera

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(mvabund)
library(ecodist)
library(vegan)

# Calling files -----------------------------------------------------------

diet_data_original <- read.csv("Data/Created_Data/Diet_Data_Original.csv")

# Statistical analysis ----------------------------------------------------

### many glm

diet_manyglm <- manyglm(mvabund(diet_data_original[,20:36]) ~ SpeciesCode,
                   data = diet_data_original,
                   family = "negative.binomial")

diet_manyglm_anova <- anova(diet_manyglm, p.uni = "adjusted")

### PERMANOVA

temp <-diet_data_original %>%
  filter(rowSums(across(20:36), na.rm = TRUE) != 0)

diet_PERMANOVA <- adonis2(vegdist(temp[,20:36], method= "bray") ~ SpeciesCode + LifeStage + HabitatType + FultonConditionFactor,  
                           data= temp, 
                           by= "margin",
                           na.action = na.omit)

print(diet_PERMANOVA)
