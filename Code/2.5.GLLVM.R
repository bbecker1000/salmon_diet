# last updated: 06/23/26
# Results:
  # ZIP family distribution with 3 latent variables performs the best in model comparison using AIC
    # Explanation - ZIP is expected when count data displays zero-inflation with equidispersion (exc. to 0s)

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(mvabund)
library(grDevices)

# Calling files -----------------------------------------------------------

diet_data_original <- read_csv("Data/Created_Data/Diet_Data_Original.csv")
diet_data_original_traits <- read_csv("Data/Created_Data/Diet_Taxa_Original_Traits.csv")


# Modifying dataframe -----------------------------------------------------

# remove empty stomach  (zero rows)
diet_data_original_filtered <- diet_data_original[rowSums(diet_data_original[,21:37]) > 0,] %>% filter(LifeStage == "YoY")

# Model latent variable and distribution family selection -----------------------------------------------------

# Test from 1 - 5 latent variables for gaussian family
gaussian_AIC_comparison <- NULL
for(i in 1:5){
  fiti <- gllvm(diet_data_original[,21:37] %>% filter(rowSums(across(where(is.numeric))) != 0), 
                family = "gaussian", num.lv = i, sd.errors = FALSE, seed = 1234)
  gaussian_AIC_comparison[i] <- summary(fiti)$AICc
  names(gaussian_AIC_comparison)[i] = i
}
gaussian_AIC_comparison

# Test from 1 - 5 latent variables for poisson family
poisson_AIC_comparison <- NULL
for(i in 1:5){
  fiti <- gllvm(diet_data_original[,21:37] %>% filter(rowSums(across(where(is.numeric))) != 0),
                family = "poisson", num.lv = i, sd.errors = FALSE, seed = 1234)
  poisson_AIC_comparison[i] <- summary(fiti)$AICc
  names(poisson_AIC_comparison)[i] = i
}
poisson_AIC_comparison

# Test from 1 - 5 latent variables for negative binomial family
negative_binomial_AIC_comparison <- NULL
for(i in 1:5){
  fiti <- gllvm(diet_data_original[,21:37] %>% filter(rowSums(across(where(is.numeric))) != 0),
                family = "negative.binomial", num.lv = i, sd.errors = FALSE, seed = 1234)
  negative_binomial_AIC_comparison[i] <- summary(fiti)$AICc
  names(negative_binomial_AIC_comparison)[i] = i
}
negative_binomial_AIC_comparison

# Test from 1 - 5 latent variables for ZIP family
ZIP_AIC_comparison <- NULL
for(i in 1:5){
  fiti <- gllvm(diet_data_original[,21:37] %>% filter(rowSums(across(where(is.numeric))) != 0),
                family = "ZIP", num.lv = i, sd.errors = FALSE, seed = 1234)
  ZIP_AIC_comparison[i] <- summary(fiti)$AICc
  names(ZIP_AIC_comparison)[i] = i
}
ZIP_AIC_comparison

# Test from 1 - 5 latent variables for ZINB family
ZINB_AIC_comparison <- NULL
for(i in 1:5){
  fiti <- gllvm(diet_data_original[,21:37] %>% filter(rowSums(across(where(is.numeric))) != 0),
                family = "ZINB", num.lv = i, sd.errors = FALSE, seed = 1234)
  ZINB_AIC_comparison[i] <- summary(fiti)$AICc
  names(ZINB_AIC_comparison)[i] = i
}
ZINB_AIC_comparison

# ordination plot ---------------------------------------------------------

# assigning shape values to species code
pchSC = NULL
pchSC[diet_data_original_filtered$SpeciesCode == "CH"] = 1
pchSC[diet_data_original_filtered$SpeciesCode == "CO"] = 2
pchSC[diet_data_original_filtered$SpeciesCode == "SH"] = 3

# assigning color values to species code
ColorsSC <- NULL
ColorsSC[diet_data_original_filtered$SpeciesCode == "CH"] = 'red'
ColorsSC[diet_data_original_filtered$SpeciesCode == "CO"] = 'green'
ColorsSC[diet_data_original_filtered$SpeciesCode == "SH"] = 'purple'

simple_model <- gllvm(diet_data_original_filtered[,21:37],
              family = "ZIP", num.lv = 3, sd.errors = FALSE, seed = 1234)

ordiplot(simple_model, biplot = TRUE,
         main = "Ordination of ", symbols = TRUE, s.cex = 0.6, pch = pchSC, s.colors = ColorsSC)

legend("topleft", legend = c("CH", "CO", "SH"), pch = c(1, 2, 3), col = c('red','green','purple'), bty = "n")

sDesign <- data.frame(StreamNumber = as.factor(diet_data_original_filtered$StreamNumber))

site_model <- gllvm(diet_data_original_filtered[,21:37], studyDesign = sDesign, family = "ZIP", row.eff = ~(1|StreamNumber),
                    num.lv = 3, sd.errors = FALSE, seed = 1234)

ordiplot(site_model, biplot = TRUE,
         main = "Ordination of ", symbols = TRUE, s.cex = 0.6, pch = pchSC, s.colors = ColorsSC)

# Test and compare zero latent variable and zero covariate models ---------

null_lv_model <- gllvm(diet_data_original_filtered[,21:37], diet_data_original_filtered[, 1:20] %>% select(SpeciesCode,FieldSeason,HabitatType), studyDesign = sDesign, 
                       family = "ZIP", row.eff = ~(1|StreamNumber), num.lv = 0, 
                       formula = ~ SpeciesCode + FieldSeason + HabitatType,
                       seed = 1234)

coefplot(null_lv_model, cex.ylab = 0.7, mar = c(4, 9, 2, 1), mfrow=c(2,3), order = TRUE)

final_model <- gllvm(diet_data_original_filtered[,21:37], diet_data_original_filtered[, 1:20] %>% select(SpeciesCode,FieldSeason,HabitatType), studyDesign = sDesign, 
                       family = "ZIP", row.eff = ~(1|StreamNumber), num.lv = 3, 
                       formula = ~ SpeciesCode + FieldSeason + HabitatType,
                       seed = 1234)

coefplot(final_model, cex.ylab = 0.7, mar = c(4, 9, 2, 1), mfrow=c(2,3), order = TRUE)
