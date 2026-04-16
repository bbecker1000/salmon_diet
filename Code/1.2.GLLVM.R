library(mvabund)
library(gllvm)
library(readxl)
library(tidyverse)

DietData <- read_excel("Data/DietData.xlsx")
DietData_env <- read_excel("Data/DietData_env.xlsx")

lowcounttaxa <- (DietData %>%
              pivot_longer(cols = 1:37, names_to = "Taxa", values_to = "Count") %>%
              group_by(Taxa) %>%
              summarize(Total_Count = sum(Count)) %>%
              filter(Total_Count < 10))$Taxa
nontaxafood <- c("detritus_total", "unk_invert", "Unknown", "sand_gravel_total", "unk_plant_material")
DietData <- DietData %>%
  select(-all_of(lowcounttaxa), - all_of(nontaxafood)) # remove junk and < 10 counts

DietDataWhole <- cbind(DietData_env, DietData)
DietDataWhole <- DietDataWhole[rowSums(DietDataWhole[,27:43]) != 0,] # remove zeros

DietData <- DietDataWhole[,27:43]
DietData_traits <- DietDataWhole %>%
  select(SpeciesCode, LifeStage, ForkLength, FishWeight, FultonConditionFactor)
DietData_habitat <- DietDataWhole %>%
  select(Creek, FieldSeason, BasinWideUnit, HabitatType, ComplexPool, Length_m, EstWidth_m, EstSurfaceArea_msq, MaxDepth_m, CrestDepth_m, ResidualPoolDepth_m, PercentCover)
DietData_env <- cbind(DietData_traits,DietData_habitat)

### Identifying best count distribution model: ZIP

fitg <- gllvm(DietData, family = "gaussian")
par(mfrow = c(3, 2), mar = c(4, 4, 2, 1))
plot(fitg, var.colors = 1)

fitp <- gllvm(DietData, family = "poisson")
par(mfrow = c(3, 2), mar = c(4, 4, 2, 1))
plot(fitp, var.colors = 1)

fitnb <- gllvm(DietData, family = "negative.binomial")
par(mfrow = c(3, 2), mar = c(4, 4, 2, 1))
plot(fitnb, var.colors = 1)

fitzip <- gllvm(DietData, family = "ZIP") # zero-inflated poisson
par(mfrow = c(3, 2), mar = c(4, 4, 2, 1))
plot(fitzip, var.colors = 1)

fitzinb <- gllvm(DietData, family = "ZINB") # zero-inflated negative binomial
par(mfrow = c(3, 2), mar = c(4, 4, 2, 1))
plot(fitzinb, var.colors = 1)

AIC(fitg, fitp, fitnb, fitzip, fitzinb)

### Dispersion test

meanY <- apply(DietData,1, mean)
varY <- apply(DietData,1, var)
par(mfrow = c(1,1))
plot(log(meanY),varY, log = "y")
points(log(sort(meanY)), sort(meanY), type = "l")
points(log(sort(meanY)), sort(meanY+ 1*meanY^2), type = "l", col=2, lty=2)
# Clear overdispersion

datadispersion <- DietData %>%
  pivot_longer(cols = 1:17, names_to = "Taxa", values_to = "Count") %>%
  group_by(Taxa) %>%
  summarize(var = var(Count), mean = mean(Count), ratio = var/mean);view(datadispersion)
# if ratio = 1, data is equidispersed & poisson is appropriate
# if r > 1, data is overdisperesed & negative binomial is appropriate
# Most underdispersion is sand or detritus... data is mostly fairly equidispersed with some extreme overdispersion

# ZIP performs the best by AIC... ZINB is only appropriate if overdispersion is caused by anything other than excess zeros
# This reasoning follow given the visual count histograms
ggplot(DietData %>% 
         pivot_longer(cols = 1:37, names_to = "Taxa", values_to = "Count"), 
       aes(x = Count)) + 
  geom_histogram(binwidth = 1) + 
  facet_wrap(~Taxa)

### Ordination plot

par(mfrow = c(1, 1))

# biplot can be used to identify indicator species that correspond to specific observations
# terms: observation = site, diet = species
ordiplot(fitzip, biplot = TRUE, ind.spp = 37, xlim = c(-3, 3), ylim = c(-3, 3), 
         main = "Biplot", s.cex = 0.01)

### Deciding which variables to include
DietData_traits %>%
  group_by(SpeciesCode) %>%
  summarize(mean(FultonConditionFactor)) # Maybe not include?

DietData_traits %>%
  filter(LifeStage == "YoY") %>%
  group_by(SpeciesCode) %>%
  summarize(mean(ForkLength)) # Maybe not include?

DietData_traits %>%
  filter(LifeStage == "YoY") %>%
  group_by(SpeciesCode) %>%
  summarize(mean(FishWeight)) ### Maybe include???

### Include environmental variables
# Iterate number of latent variables, compare using AICc to determine best number of latent variables
criteria <- NULL
for(i in 1:5){
  fiti <- gllvm(DietData, DietData_traits, family = "ZIP", num.lv = i, sd.errors = FALSE,
                formula = ~ SpeciesCode + FishWeight, seed = 1234)
  criteria[i] <- summary(fiti)$AICc
  names(criteria)[i] = i
}
criteria

M1 <- gllvm(DietData, DietData_traits, family = "ZIP", num.lv = 2,
      formula = ~ SpeciesCode + FishWeight + 0, seed = 1234)

M2 <- gllvm(DietData, DietData_traits, family = "ZIP", num.lv = 5,
            formula = ~ SpeciesCode + FishWeight + 0, seed = 1234)

M3 <- gllvm(DietData, DietData_traits, family = "ZIP", num.lv = 2,
            formula = ~ SpeciesCode * FishWeight + 0, seed = 1234)

coefplot(M1, cex.ylab = 0.7, mar = c(4, 9, 2, 1), mfrow=c(2,2), order = FALSE, 
         xlim.list = list(c(-15,15),c(-15,15),c(-15,15), c(-0.2,0.2)))
coefplot(M2, cex.ylab = 0.7, mar = c(4, 9, 2, 1), mfrow=c(2,2), order = FALSE, 
         xlim.list = list(c(-15,15),c(-15,15),c(-15,15), c(-0.2,0.2)))
coefplot(M3, cex.ylab = 0.7, mar = c(4, 9, 2, 1), mfrow=c(2,2), order = TRUE, 
         xlim.list = list(c(-15,15),c(-15,15),c(-15,100), c(-5,15)))

# Define symbols for different sampling locations:
pchSC = NULL
pchSC[DietData_traits$SpeciesCode == "CH"] = 1
pchSC[DietData_traits$SpeciesCode == "CO"] = 2
pchSC[DietData_traits$SpeciesCode == "SH"] = 3

library(grDevices)
FW <- DietData_traits$FishWeight
rbPal <- colorRampPalette(c('green', 'red'))
ColorsFW <- rbPal(20)[as.numeric(cut(FW, breaks = 20))]

par(mfrow = c(1,1))
ordiplot(M3, biplot = TRUE, ind.spp = 37, xlim = c(-3, 3), ylim = c(-3, 3), 
         main = "Biplot", symbols = TRUE, pch = pchSC, s.colors = ColorsFW)

M4 <- gllvm(DietData, DietData_traits, family = "ZIP", num.lv = 2,
            formula = ~ SpeciesCode * FishWeight + LifeStage + 0, seed = 1234)
coefplot(M4, cex.ylab = 0.7, mar = c(4, 9, 2, 1), mfrow=c(2,3), order = TRUE, 
         xlim.list = list(c(-15,100),c(-15,100),c(-15,100), c(-40,10),c(-10,10)))
par(mfrow = c(1,1))
ordiplot(M4, biplot = TRUE, ind.spp = 37, xlim = c(-3, 3), ylim = c(-3, 3), 
         main = "Biplot", symbols = TRUE, pch = pchSC, s.colors = ColorsFW)

M5 <- gllvm(DietData, DietData_env, family = "ZIP", num.lv = 2,
            formula = ~ SpeciesCode * FishWeight + LifeStage + HabitatType + FieldSeason + MaxDepth_m + 0, 
            seed = 1234)
coefplot(M5, cex.ylab = 0.7, mar = c(4, 9, 2, 1), mfrow=c(2,3), order = TRUE)
par(mfrow = c(1,1))
ordiplot(M5, biplot = TRUE, ind.spp = 37, xlim = c(-3, 3), ylim = c(-3, 3), 
         main = "Biplot", symbols = TRUE, pch = pchSC, s.colors = ColorsFW)
