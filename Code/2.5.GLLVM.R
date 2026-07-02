# last updated: 06/23/26
# Results:
  # ZIP family distribution with 3 latent variables performs the best in model comparison using AIC
    # Explanation - ZIP is expected when count data displays zero-inflation with equidispersion (exc. to 0s)

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(mvabund)
library(grDevices)
library(gllvm)

# Calling files -----------------------------------------------------------

diet_data_original <- read_csv("Data/Created_Data/Diet_Data_Original.csv")
diet_data_original_traits <- read_csv("Data/Created_Data/Diet_Taxa_Original_Traits.csv")

# Modifying dataframe -----------------------------------------------------

# remove empty stomach  (zero rows)
diet_data_original_filtered <- diet_data_original[rowSums(diet_data_original[,21:37]) > 0,] %>% filter(LifeStage == "YoY") %>% na.omit()

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

Model_Testing_df <- data.frame(Latent_Variables = c(1,2,3,4,5),
           Normal = gaussian_AIC_comparison,
           Poisson = poisson_AIC_comparison,
           Negative_Binomial = negative_binomial_AIC_comparison,
           Zero_Inflated_Negative_Binomial = ZINB_AIC_comparison,
           Zero_Inflated_Poisson = ZIP_AIC_comparison)

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

par(mfrow = c(1,1))
ordiplot(simple_model, biplot = TRUE,
         main = "Ordination of ", symbols = TRUE, s.cex = 0.6, pch = pchSC, s.colors = ColorsSC)

legend("topleft", legend = c("CH", "CO", "SH"), pch = c(1, 2, 3), col = c('red','green','purple'), bty = "n")

sDesign_raw <- data.frame(StreamNumber = diet_data_original_filtered$StreamNumber)
sDesign <- data.frame(StreamNumber = as.factor(diet_data_original_filtered$StreamNumber))

site_model <- gllvm(diet_data_original_filtered[,21:37], studyDesign = sDesign_raw, family = "ZIP", row.eff = ~(1|StreamNumber),
                    num.lv = 3, seed = 1234)

## plot by species
png("Figures/New_Figures/LV_Biplot.png", width = 7, height = 7, units = "in", res = 300)
ordiplot(site_model, biplot = TRUE,
         main = "Latent Variable Biplot by Species",
         symbols = TRUE, s.cex = 0.6, pch = pchSC, s.colors = ColorsSC, jitter = TRUE)
mtext("58.5% var. explained", side = 1, line = 2.1)
mtext("23.9% var. explained", side = 2, line = 2.1)
legend("topleft", legend = c("CH", "CO", "SH"), pch = c(1, 2, 3), col = c('red','green','purple'), bty = "n")
dev.off()

# assigning color values to fieldseason code
ColorsFS <- NULL
ColorsFS[diet_data_original_filtered$FieldSeason == 2020] = 'red'
ColorsFS[diet_data_original_filtered$FieldSeason == 2022] = 'green'

## plot by season
png("Figures/New_Figures/LV_Biplot_Field_Season.png", width = 7, height = 7, units = "in", res = 300)
ordiplot(site_model, biplot = TRUE,
         main = "Latent Variable Biplot by Field Season",
         symbols = TRUE, s.cex = 0.6, pch = pchSC, s.colors = ColorsFS, jitter = TRUE)
mtext("58.5% var. explained", side = 1, line = 2.1)
mtext("23.9% var. explained", side = 2, line = 2.1)
legend("topleft", legend = c("2020","2022"), pch = 1, col = c('red','green'), bty = "n")
dev.off()

# assigning color values to fieldseason code
ColorsHT <- NULL
ColorsHT[diet_data_original_filtered$HabitatType == "Mid-Channel Pool"] = 'red'
ColorsHT[diet_data_original_filtered$HabitatType == "Scour Pool"] = 'green'
ColorsHT[diet_data_original_filtered$HabitatType == "Flatwater"] = 'purple'

## plot by habitat type
png("Figures/New_Figures/LV_Biplot_Habitat_Type.png", width = 7, height = 7, units = "in", res = 300)
ordiplot(site_model, biplot = TRUE,
         main = "Latent Variable Biplot by Habitat Type",
         symbols = TRUE, s.cex = 0.6, s.colors = ColorsHT, pch = pchSC, jitter = TRUE)
mtext("58.5% var. explained", side = 1, line = 2.1)
mtext("23.9% var. explained", side = 2, line = 2.1)
legend("topleft", legend = c("Mid-Channel Pool","Scour Pool", "Flatwater"), pch = 1, col = c('red','green','purple'), bty = "n")
dev.off()

# assign color values to fork length
FL <- DietData_traits$ForkLength
rbPal <- colorRampPalette(c('red', 'green'))
ColorsFL <- rbPal(20)[as.numeric(cut(FL, breaks = 20))]

## plot by fork length
png("Figures/New_Figures/LV_Biplot_Fork_Length.png", width = 7, height = 7, units = "in", res = 300)
ordiplot(site_model, biplot = TRUE,
         main = "Latent Variable Biplot by Fork Length",
         symbols = TRUE, s.cex = 0.6, pch = pchSC, s.colors = ColorsFL,  jitter = TRUE)
mtext("58.5% var. explained", side = 1, line = 2.1)
mtext("23.9% var. explained", side = 2, line = 2.1)
legend("topleft", legend = c("Small", "Large"), pch = 1, col = c('red','green'), bty = "n")
dev.off()

# Test and compare zero latent variable and zero covariate models ---------

diet_data_original_filtered <- diet_data_original_filtered %>%
  mutate(ForkLength_scaled = as.numeric(scale(ForkLength)))

null_lv_model <- gllvm(diet_data_original_filtered[,21:37], diet_data_original_filtered %>% select(SpeciesCode,FieldSeason, HabitatType, ForkLength), studyDesign = sDesign, 
                       family = "ZIP",  num.lv = 0, row.eff = ~ (1|StreamNumber),
                       formula = ~ SpeciesCode + ForkLength + FieldSeason,
                       seed = 1234)

png("Figures/New_Figures/GLLVM_Coef_Plot.png", width = 9, height = 6, units = "in", res = 300)
coefplot(null_lv_model, cex.ylab = 0.7, mar = c(4, 9, 2, 1), mfrow=c(2,3), order = TRUE)
dev.off()

final_model <- gllvm((diet_data_original_filtered %>% filter(FieldSeason == 2022))[,21:37], diet_data_original_filtered %>% filter(FieldSeason == 2022) %>% select(SpeciesCode,FieldSeason,HabitatType,ForkLength), studyDesign = sDesign, 
                       family = "ZIP", row.eff = ~(1|StreamNumber), num.lv = 3, 
                       formula = ~ SpeciesCode + HabitatType + ForkLength,
                       seed = 1234)

coefplot(final_model, cex.ylab = 0.7, mar = c(4, 9, 2, 1), mfrow=c(2,3), order = TRUE)

ZIP_cov_AIC_comparison <- NULL
for(i in 1:5){
  fiti <- gllvm(diet_data_original_filtered[,21:37], diet_data_original_filtered[, 1:20] %>% select(SpeciesCode,FieldSeason,HabitatType), studyDesign = sDesign, 
                family = "ZIP", row.eff = ~(1|StreamNumber), num.lv = i, 
                formula = ~ SpeciesCode + FieldSeason + HabitatType,
                seed = 1234)
  ZIP_cov_AIC_comparison[i] <- summary(fiti)$AICc
  names(ZIP_cov_AIC_comparison)[i] = i
}
ZIP_cov_AIC_comparison

# Variance explained ------------------------------------------------------

VP(fl_model)
VP(final_model)

# Exporting figures -------------------------------------------------------

write.csv(Model_Testing_df,"Figures/New_Figures/Model_Testing.csv", row.names = FALSE)