# last updated: 06/23/26
# Results:
  # ZIP family distribution with 3 latent variables performs the best in model comparison using AIC
    # Explanation - ZIP is expected when count data displays zero-inflation with equidispersion (exc. to 0s)

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(mvabund)
library(grDevices)
library(gllvm)
library(forcats)

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
gllvm::ordiplot(simple_model, biplot = TRUE,
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

### ZINB

simple_model_2 <- gllvm(diet_data_original_filtered[,21:37],studyDesign = sDesign_raw,row.eff = ~(1|StreamNumber), 
                        family = "ZINB", num.lv = 2, sd.errors = FALSE, seed = 1234)

ordiplot(simple_model_2, biplot = TRUE,
         main = "Latent Variable Biplot by Species",
         symbols = TRUE, s.cex = 1, pch = pchSC, s.colors = ColorsSC)
legend("topleft", legend = c("CH", "CO", "SH"), pch = c(1, 2, 3), col = c('red','green','purple'), bty = "n")

ordiplot(simple_model_2, biplot = FALSE,
         main = "Latent Variable Biplot by Habitat Type",
         symbols = TRUE, s.cex = 1, pch = pchSC, s.colors = ColorsHT)
legend("topleft", legend = c("Mid-Channel Pool","Scour Pool", "Flatwater"), pch = 1, col = c('red','green','purple'), bty = "n")

ordiplot(simple_model_2, biplot = FALSE,
         main = "Latent Variable Biplot by Field Season",
         symbols = TRUE, s.cex = 1, pch = pchSC, s.colors = ColorsFS)
legend("topleft", legend = c("2020","2022"), pch = 1, col = c('red','green'), bty = "n")

ordiplot(simple_model_2, biplot = FALSE,
         main = "Latent Variable Biplot by Fork Length",
         symbols = TRUE, s.cex = 0.6, pch = pchSC, s.colors = ColorsFL)
legend("topleft", legend = c("Small", "Large"), pch = 1, col = c('red','green'), bty = "n")

# Test and compare zero latent variable and zero covariate models ---------

diet_data_original_filtered <- diet_data_original_filtered %>%
  mutate(ForkLength_scaled = as.numeric(scale(ForkLength)))

null_lv_model <- gllvm(diet_data_original_filtered[,21:37], diet_data_original_filtered %>% select(SpeciesCode,FieldSeason, HabitatType, ForkLength), studyDesign = sDesign, 
                       family = "ZIP",  num.lv = 0, row.eff = ~ (1|StreamNumber),
                       formula = ~ SpeciesCode + ForkLength + FieldSeason,
                       seed = 1234)

test_model <- gllvm(diet_data_original_filtered[,21:37], diet_data_original_filtered %>% mutate(FieldSeason = as.factor(FieldSeason)) %>% select(SpeciesCode,FieldSeason, HabitatType, ForkLength), studyDesign = sDesign, 
                       family = "ZINB",  num.lv = 0, row.eff = ~ (1|StreamNumber),
                       formula = ~ SpeciesCode +  HabitatType + ForkLength + FieldSeason,
                       seed = 1234)

null_lv_HT_model <- gllvm(diet_data_original_filtered[,21:37], diet_data_original_filtered %>% select(SpeciesCode,FieldSeason, HabitatType, ForkLength), studyDesign = sDesign, 
                       family = "ZIP",  num.lv = 0, row.eff = ~ (1|StreamNumber),
                       formula = ~ SpeciesCode + ForkLength + HabitatType,
                       seed = 1234)

null_lv_HT_FS_model <- gllvm(diet_data_original_filtered[,21:37], diet_data_original_filtered %>% select(SpeciesCode,FieldSeason, HabitatType, ForkLength), studyDesign = sDesign, 
                          family = "ZIP",  num.lv = 0, row.eff = ~ (1|StreamNumber),
                          formula = ~ SpeciesCode + FieldSeason + HabitatType,
                          seed = 1234)

png("Figures/New_Figures/GLLVM_Coef_Plot.png", width = 9, height = 6, units = "in", res = 300)
coefplot(test_model, cex.ylab = 0.7, mar = c(4, 9, 2, 1), mfrow=c(2,3), order = TRUE)
dev.off()

final_model <- gllvm((diet_data_original_filtered %>% filter(FieldSeason == 2022))[,21:37], diet_data_original_filtered %>% filter(FieldSeason == 2022) %>% select(SpeciesCode,FieldSeason,HabitatType,ForkLength), studyDesign = sDesign_2022, 
                       family = "ZIP", row.eff = ~(1|StreamNumber), num.lv = 0, 
                       formula = ~ SpeciesCode + HabitatType + ForkLength,
                       seed = 1234)

coefplot(final_model, cex.ylab = 0.7, mar = c(4, 9, 2, 1), mfrow=c(2,3), order = TRUE)


sDesign_2022 <- data.frame(StreamNumber = as.factor((diet_data_original_filtered %>% filter(FieldSeason == 2022))$StreamNumber))

final_model_V2 <- gllvm((diet_data_original_filtered %>% filter(FieldSeason == 2022))[,21:37], diet_data_original_filtered %>% filter(FieldSeason == 2022) %>% select(SpeciesCode,FieldSeason,HabitatType,ForkLength), studyDesign = sDesign_2022, 
                     family = "ZINB", row.eff = ~(1|StreamNumber), num.lv = 0, 
                     formula = ~ SpeciesCode + HabitatType + ForkLength,
                     seed = 1234)

coefplot(final_model_V2, cex.ylab = 0.7, mar = c(4, 9, 2, 1), mfrow=c(2,3), order = TRUE)

final_model_all <- gllvm(diet_data_original_filtered[,21:37], diet_data_original_filtered %>% select(SpeciesCode,FieldSeason,HabitatType,ForkLength), studyDesign = sDesign, 
                     family = "ZIP", row.eff = ~(1|StreamNumber), num.lv = 0, 
                     formula = ~ SpeciesCode + HabitatType + ForkLength,
                     seed = 1234)

coefplot(final_model_all, cex.ylab = 0.7, mar = c(4, 9, 2, 1), mfrow=c(2,3), order = TRUE)

final_model_all_V2 <- gllvm(diet_data_original_filtered[,21:37], diet_data_original_filtered %>% select(SpeciesCode,HabitatType,ForkLength, FieldSeason) %>% mutate(FieldSeason = as.factor(FieldSeason)), studyDesign = sDesign, 
                        family = "ZINB", row.eff = ~(1|StreamNumber), num.lv = 0, 
                        formula = ~ SpeciesCode + HabitatType + ForkLength + FieldSeason,
                        seed = 1234)

coefplot(final_model_all_V2, cex.ylab = 0.7, mar = c(4, 9, 2, 1), mfrow=c(2,3), order = TRUE)

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
VP(final_model_all_V2)

# Plot reworking ----------------------------------------------------------

model_params <- rownames_to_column(data.frame(final_model_V2$params$Xcoef), "Taxa") %>%
  pivot_longer(cols = 2:6, names_to = "Variable", values_to = "Coefficient_Estimate") %>%
  left_join(diet_data_original_filtered[,21:37] %>% 
              pivot_longer(cols = 1:17, names_to = "Taxa", values_to = "Count") %>%
              group_by(Taxa) %>%
              summarize(Count = sum(Count)),
            by = "Taxa")

ggplot(model_params %>% filter(Variable %in% c("SpeciesCodeCO", "SpeciesCodeSH")), aes(x = Coefficient_Estimate, y = Taxa, fill = Variable)) +
  geom_col()

ggplot(model_params %>% filter(Variable %in% c("HabitatTypeMid.Channel.Pool", "HabitatTypeScour.Pool")), aes(x = Coefficient_Estimate, y = Taxa, fill = Variable)) +
  geom_col()

count_labels <- model_params %>%
  filter(Variable %in% c("SpeciesCodeCO", "SpeciesCodeSH")) %>%
  group_by(Taxa) %>%
  summarize(Count = first(Count), .groups = "drop")

model_params %>%
  group_by(Taxa) %>%
  mutate(Taxa_Count = sum(Count)) %>%
  filter(Variable %in% c("SpeciesCodeCO", "SpeciesCodeSH")) %>%
  ggplot(aes(x = Coefficient_Estimate, y = forcats::fct_reorder(Taxa, Taxa_Count), fill = Variable)) +
  geom_col(position = "identity", alpha = 0.7) +
  geom_text(data = count_labels,
            aes(x = 0, y = Taxa, label = Count),
            inherit.aes = FALSE)
# Exporting figures -------------------------------------------------------

write.csv(Model_Testing_df,"Figures/New_Figures/Model_Testing.csv", row.names = FALSE)