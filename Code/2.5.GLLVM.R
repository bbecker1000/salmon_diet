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
library(DHARMa)
library(patchwork)

# Calling files -----------------------------------------------------------

diet_data_original <- read_csv("Data/Created_Data/Diet_Data_Original.csv")
diet_data_original_traits <- read_csv("Data/Created_Data/Diet_Taxa_Original_Traits.csv")

# Modifying dataframe -----------------------------------------------------

# remove empty stomach  (zero rows)
diet_data_original_filtered <- diet_data_original[rowSums(diet_data_original[,21:37]) > 0,] %>% 
  filter(LifeStage == "YoY") %>% 
  mutate(FieldSeason = as.character(FieldSeason),
         StreamNumber = as.character(StreamNumber)) %>%
  na.omit()

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


# dispersion check --------------------------------------------------------

dispersion_index <- diet_data_original %>%
  pivot_longer(cols = 21:37, names_to = "Taxa", values_to = "Count") %>%
  group_by(Taxa) %>%
  summarize(var = var(Count), mean = mean(Count), Dispersion_Index = var/mean)

plot(log(dispersion_index$mean), log(dispersion_index$var))
abline(a = 2.17, b = 1.5, lty = 2)

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

final_model_V0 <- gllvm((diet_data_original_filtered %>% filter(FieldSeason == 2022))[,21:37], diet_data_original_filtered %>% filter(FieldSeason == 2022) %>% select(SpeciesCode,FieldSeason,HabitatType,ForkLength), studyDesign = sDesign_2022, 
                     family = "poisson", row.eff = ~(1|StreamNumber), num.lv = 0, 
                     formula = ~ SpeciesCode + HabitatType + ForkLength,
                     seed = 1234)

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

final_model_V3 <- gllvm((diet_data_original_filtered %>% filter(FieldSeason == 2022))[,21:37], diet_data_original_filtered %>% filter(FieldSeason == 2022) %>% select(SpeciesCode,FieldSeason,HabitatType,ForkLength), studyDesign = sDesign_2022, 
                        family = "negative.binomial", row.eff = ~(1|StreamNumber), num.lv = 0, 
                        formula = ~ SpeciesCode + HabitatType + ForkLength,
                        seed = 1234)

final_model_V4 <- gllvm((diet_data_original_filtered %>% filter(FieldSeason == 2022))[,21:37], diet_data_original_filtered %>% filter(FieldSeason == 2022) %>% select(SpeciesCode,FieldSeason,HabitatType,ForkLength), studyDesign = sDesign_2022, 
                        family = "negative.binomial1", row.eff = ~(1|StreamNumber), num.lv = 0, 
                        formula = ~ SpeciesCode + HabitatType + ForkLength,
                        seed = 1234)

coefplot(final_model_V3, cex.ylab = 0.7, mar = c(4, 9, 2, 1), mfrow=c(2,3), order = TRUE)

final_model_all_V0 <- gllvm(diet_data_original_filtered[,21:37], diet_data_original_filtered %>% select(SpeciesCode,HabitatType,ForkLength, FieldSeason) %>% mutate(FieldSeason = as.factor(FieldSeason)), studyDesign = sDesign,
                        family = "poisson", row.eff = ~(1|StreamNumber), num.lv = 0, 
                        formula = ~ SpeciesCode + HabitatType + ForkLength + FieldSeason,
                        seed = 1234)

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

final_model_all_V3 <- gllvm(diet_data_original_filtered[,21:37], diet_data_original_filtered %>% select(SpeciesCode,HabitatType,ForkLength, FieldSeason) %>% mutate(FieldSeason = as.factor(FieldSeason)), studyDesign = sDesign, 
                            family = "negative.binomial", row.eff = ~(1|StreamNumber), num.lv = 0, 
                            formula = ~ SpeciesCode + HabitatType + ForkLength + FieldSeason,
                            seed = 1234)

final_model_all_V4 <- gllvm(diet_data_original_filtered[,21:37], diet_data_original_filtered %>% select(SpeciesCode,HabitatType,ForkLength, FieldSeason) %>% mutate(FieldSeason = as.factor(FieldSeason)), studyDesign = sDesign, 
                            family = "negative.binomial1", row.eff = ~(1|StreamNumber), num.lv = 0, 
                            formula = ~ SpeciesCode + HabitatType + ForkLength + FieldSeason,
                            seed = 1234)

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

# primary coefficients
coef_plot_df <- rownames_to_column(data.frame(final_model_all_V3$params$Xcoef), var = "Taxa") %>%
  select(Taxa, SpeciesCodeCO, SpeciesCodeSH) %>%
  rename(CO = SpeciesCodeCO, SH = SpeciesCodeSH) %>%
  pivot_longer(cols = 2:3, names_to = "SpeciesCode", values_to = "Coefficient_Estimate") %>%
  # add sd
  left_join(rownames_to_column(data.frame(final_model_V3$sd$Xcoef), var = "Taxa") %>%
              select(Taxa, SpeciesCodeCO, SpeciesCodeSH) %>%
              rename(CO = SpeciesCodeCO, SH = SpeciesCodeSH) %>%
              pivot_longer(cols = 2:3,
                           names_to = "SpeciesCode",
                           values_to = "SE"),
    by = c("Taxa", "SpeciesCode")) %>%
  # Wald test: z_value = coef/SE, p-value = 2 * pnorm w/ alpha = 0.05
  mutate(Significant = if_else(2 * pnorm(abs(Coefficient_Estimate / SE), 
                                         lower.tail = FALSE) < 0.05, 
                               "Yes", 
                               "No")) %>%
  left_join(diet_data_original_filtered[,21:37] %>% 
              pivot_longer(cols = 1:17, names_to = "Taxa", values_to = "Count") %>%
      group_by(Taxa) %>%
      summarize(Count = sum(Count)), by = "Taxa") %>%
  mutate(Lower = Coefficient_Estimate - 1.96 * SE,
         Upper = Coefficient_Estimate + 1.96 * SE)

##

count_labels <- coef_plot_df %>%
  group_by(Taxa) %>%
  summarize(Count = first(Count), .groups = "drop")

##

model_count_pred <- diet_data_original_filtered[,1:20] %>%
  mutate(RowID = row_number()) %>%
  select(RowID, SpeciesCode) %>%
  left_join(data.frame(final_model_all_V3$y) %>%
              mutate(RowID = row_number()) %>%
              pivot_longer(cols = 1:17, names_to = "Taxa", values_to = "Obs") %>%
              left_join('colnames<-'((as.data.frame(predict(final_model_all_V3, type = "response"))), dimnames(obs)[[2]]) %>%
                          mutate(RowID = row_number()) %>%
                          pivot_longer(cols = 1:17, names_to = "Taxa", values_to = "Pred"), 
                        by = c("RowID", "Taxa")), 
            by = "RowID") %>%
  group_by(Taxa, SpeciesCode) %>%
  summarize(Obs = mean(Obs), Pred = mean(Pred))

##

p1 <- ggplot(coef_plot_df, aes(x = exp(Coefficient_Estimate), y = forcats::fct_reorder(Taxa, Count), fill = SpeciesCode)) +
  geom_col(aes(alpha = Significant), position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(xmin = exp(Lower), xmax = exp(Upper), group = SpeciesCode, alpha = Significant), 
                width = 0.2,
                position = position_dodge(width = 0.9)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "black") +
  scale_alpha_manual(values = c("Yes" = 1, "No" = 0.3), guide = "none") +
  coord_cartesian(xlim = c(0,5)) + # scale_x_log10()
  geom_text(data = count_labels, aes(x = -0.12, y = Taxa, label = paste0("n=", Count)), 
            inherit.aes = FALSE,
            fontface = "bold") +
  geom_tile(data = model_count_pred, aes(x = Pred, y = Taxa),
            color = "white", fill = NA, linewidth = 2.5, inherit.aes = FALSE) +
  geom_tile(data = model_count_pred, aes(x = Pred, y = Taxa, color = SpeciesCode),
            fill = NA, linewidth = 1.5, inherit.aes = FALSE) +
  scale_color_manual(labels = c("CH", "CO", "SH"),
                     values = c("CH" = "#2E9B57",  # medium green
                                "CO" = "#D84A4A",  # medium red
                                "SH" = "#3F7FD3")) +
  labs(y = "Taxa", x = "Coefficient Estimate / Average Abundance Predictions") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position = "none"); p1

p2 <- ggplot(coef_plot_df, aes(x = exp(Coefficient_Estimate), y = forcats::fct_reorder(Taxa, Count), fill = SpeciesCode)) +
  geom_col(aes(alpha = Significant), position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(xmin = exp(Lower), xmax = exp(Upper), group = SpeciesCode, alpha = Significant), 
                width = 0.2,
                position = position_dodge(width = 0.9)) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  scale_alpha_manual(values = c("Yes" = 1, "No" = 0.3), guide = "none") +
  scale_x_log10(breaks = c(10, 10**5, 10**10, 10**15, 10**20),
                labels = c("1e+1", "1e+5", "1e+10", "1e+15", "1e+20")) +
  coord_cartesian(xlim = c(50,10**20)) +
  geom_text(data = count_labels, aes(x = -0.12, y = Taxa, label = paste0("n=", Count)), 
            inherit.aes = FALSE,
            fontface = "bold") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank());p2
  
p1 + p2 +
  plot_layout(widths = c(4, 1))
ggsave("Figures/New_Figures/Coefficient_Plot.png", width = 10, height = 8, units = "in")

###

ggplot(model_params %>% filter(Variable %in% c("SpeciesCodeCO", "SpeciesCodeSH")), aes(x = Coefficient_Estimate, y = Taxa, fill = Variable)) +
  geom_col()

ggplot(model_params %>% filter(Variable %in% c("HabitatTypeMid.Channel.Pool", "HabitatTypeScour.Pool")), aes(x = Coefficient_Estimate, y = Taxa, fill = Variable)) +
  geom_col()

count_labels <- model_params %>%
  filter(Variable %in% c("SpeciesCodeCO", "SpeciesCodeSH")) %>%
  group_by(Taxa) %>%
  summarize(Count = first(Count), .groups = "drop")
# Exporting figures -------------------------------------------------------

write.csv(Model_Testing_df,"Figures/New_Figures/Model_Testing.csv", row.names = FALSE)