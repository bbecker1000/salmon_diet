# last updated: 06/04/2026
# notes:
  # many glm - sig. diff. taxa inc. Coleoptera, Diptera, Hemiptera, Trichoptera, Araneae, Hymnenoptera, Psocodea, & Plecoptera
  # PERMANOVA - species code, life stage, habitat, and fulton condition factor
  # morisita-horn index - CH-CO [0.99] highly similar, CH-SH [0.45] & CO-SH [0.49] dissimilar... SH1+-SHYoY [0.67] moderately similar

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(mvabund)
library(ecodist)
library(vegan)

# Calling files -----------------------------------------------------------

diet_data_original <- read.csv("Data/Created_Data/Diet_Data_Original.csv")

# Statistical analysis ----------------------------------------------------

### many glm

# estimate an individual glm for measuring the occurrence of each diet item by species code
diet_manyglm <- manyglm(mvabund(diet_data_original[,21:37]) ~ SpeciesCode,
                   data = diet_data_original,
                   family = "negative.binomial")

# evaluate statistical significance of the probability of occurrence for each diet taxa across species using anova
diet_manyglm_anova <- anova(diet_manyglm, p.uni = "adjusted")

### PERMANOVA

# remove empty stomachs (PERMANOVA can not evaluate 0 observations)
diet_data_empty_stomachs_removed <- diet_data_original %>%
  filter(rowSums(across(21:37), na.rm = TRUE) != 0)

# bray-curtis PERMANOVA test on diet items across species, life stage, habitat, and fulton condition factor fixed effects
diet_PERMANOVA <- adonis2(vegdist(diet_data_empty_stomachs_removed[,21:37], method = "bray") ~ SpeciesCode + LifeStage + HabitatType + FultonConditionFactor,  
                           data = diet_data_empty_stomachs_removed, 
                           by = "margin",
                          na.action = na.omit)

### morisita-horn index

# create function for morisita-horn index
morisita_horn_index <- function(n, P_a, P_b) {
  Numerator_Total <- 0
  Denominator_Total <- 0
  for (i in 1:n) {
    # P_a should be a list or vector
    Numerator <- P_a[i] * P_b[i]
    Denominator <- P_a[i]^2 + P_b[i]^2
    
    Numerator_Total <- Numerator_Total + Numerator
    Denominator_Total <- Denominator_Total + Denominator
  }
  O = 2*Numerator_Total / Denominator_Total
  return(O)
}

# create dataframe for taxa items by percentage of diet by species
diet_comparison <- diet_data_original %>%
  filter(LifeStage == "YoY") %>%
  select(SpeciesCode, 21:37) %>%
  pivot_longer(cols = 2:18, names_to = "Taxa", values_to = "Count") %>%
  group_by(SpeciesCode, Taxa) %>%
  summarize(Count = sum(Count)) %>%
  group_by(SpeciesCode) %>%
  mutate(Total = sum(Count),
         Proportion = Count/Total) %>%
  select(SpeciesCode,Taxa, Proportion) %>%
  pivot_wider(names_from = SpeciesCode, values_from = Proportion)

N <- nrow(diet_comparison)

morisita_horn_index(N, diet_comparison$CH, diet_comparison$CO)
morisita_horn_index(N, diet_comparison$CH, diet_comparison$SH)
morisita_horn_index(N, diet_comparison$CO, diet_comparison$SH)

diet_comparison_SH <- diet_data_original %>%
  filter(SpeciesCode == "SH") %>%
  select(LifeStage, 21:37) %>%
  pivot_longer(cols = 2:18, names_to = "Taxa", values_to = "Count") %>%
  group_by(LifeStage, Taxa) %>%
  summarize(Count = sum(Count)) %>%
  group_by(LifeStage) %>%
  mutate(Total = sum(Count),
         Proportion = Count/Total) %>%
  select(LifeStage,Taxa, Proportion) %>%
  pivot_wider(names_from = LifeStage, values_from = Proportion)

morisita_horn_index(N, diet_comparison_SH$`1+`, diet_comparison_SH$YoY)

### species rarefaction

species_groups_matrix <- as.matrix(diet_data_raw %>%
    filter(LifeStage == "YoY") %>%
    select(SpeciesCode, 21:49) %>%
    pivot_longer(cols = 2:30, names_to = "Taxa", values_to = "Count") %>%
    group_by(SpeciesCode, Taxa) %>%
    summarize(Count = sum(Count)) %>%
    pivot_wider(names_from = Taxa, values_from = Count) %>%
    column_to_rownames(var = "SpeciesCode"))

CH_river_reach_matrix <- as.matrix(diet_data_raw %>%
     filter(LifeStage == "YoY", SpeciesCode == "CH") %>%
     select(RiverReach, 21:49) %>%
     pivot_longer(cols = 2:30, names_to = "Taxa", values_to = "Count") %>%
     group_by(RiverReach, Taxa) %>%
     summarize(Count = sum(Count)) %>%
     pivot_wider(names_from = Taxa, values_from = Count) %>%
     column_to_rownames(var = "RiverReach"))

CO_river_reach_matrix <- as.matrix(diet_data_raw %>%
                                     filter(LifeStage == "YoY", SpeciesCode == "CO") %>%
                                     select(RiverReach, 21:49) %>%
                                     pivot_longer(cols = 2:30, names_to = "Taxa", values_to = "Count") %>%
                                     group_by(RiverReach, Taxa) %>%
                                     summarize(Count = sum(Count)) %>%
                                     pivot_wider(names_from = Taxa, values_from = Count) %>%
                                     column_to_rownames(var = "RiverReach"))

SH_river_reach_matrix <- as.matrix(diet_data_raw %>%
                                     filter(LifeStage == "YoY", SpeciesCode == "SH") %>%
                                     select(RiverReach, 21:49) %>%
                                     pivot_longer(cols = 2:30, names_to = "Taxa", values_to = "Count") %>%
                                     group_by(RiverReach, Taxa) %>%
                                     summarize(Count = sum(Count)) %>%
                                     pivot_wider(names_from = Taxa, values_from = Count) %>%
                                     column_to_rownames(var = "RiverReach"))

# Raw plots ---------------------------------------------------------------

species_rarecurve_plot <- rarecurve(species_groups_matrix, step = 5, 
          col = c("red","blue","green"),
          label= TRUE,
          xlab = "Sample Size", 
          ylab = "Species Richness",
          main= "Rarefaction curves for prey accumulation")

CH_habitat_rarecurve_plot <- rarecurve(CH_river_reach_matrix, step = 5, 
          col = c("red","blue","green","orange","purple"),
          label= TRUE,
          xlab = "Sample Size", 
          ylab = "Species Richness",
          main= "CH prey accumulation by habitat")

CO_habitat_rarecurve_plot <- rarecurve(CO_river_reach_matrix, step = 5, 
                                       col = c("red","blue","green","orange","purple"),
                                       label= TRUE,
                                       xlab = "Sample Size", 
                                       ylab = "Species Richness",
                                       main= "CO prey accumulation by habitat")

SH_habitat_rarecurve_plot <- rarecurve(SH_river_reach_matrix, step = 5, 
                                       col = c("red","blue","green","orange","purple"),
                                       label= TRUE,
                                       xlab = "Sample Size", 
                                       ylab = "Species Richness",
                                       main= "SH prey accumulation by habitat")

# Combined plots ----------------------------------------------------------

par(mfrow = c(2,2))

