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
              filter(Total_Count <= 10))$Taxa
nontaxafood <- c("detritus_total", "unk_invert", "Unknown", "sand_gravel_total", "unk_plant_material")
DietData <- DietData %>%
  select(-all_of(lowcounttaxa), - all_of(nontaxafood))

DietData_traits <- DietData_env %>%
  select(SpeciesCode, LifeStage, ForkLength, FishWeight, FultonConditionFactor)
DietData_habitat <- DietData_env %>%
  select(Creek, FieldSeason, BasinWideUnit, HabitatType, ComplexPool, Length_m, EstWidth_m, EstSurfaceArea_msq, MaxDepth_m, CrestDepth_m, ResidualPoolDepth_m, PercentCover)

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

datadispersion <- DietData %>%
  pivot_longer(cols = 1:37, names_to = "Taxa", values_to = "Count") %>%
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

### Include environmental variables
SC_model <- gllvm(DietData, DietData_traits, family = "ZIP",
                formula = ~SpeciesCode)

SC_NBmodel <- gllvm(DietData, DietData_traits, family = "negative.binomial",
                  formula = ~SpeciesCode)

SC_ZINBmodel <- gllvm(DietData, DietData_traits, family = "ZINB",
                      formula = ~SpeciesCode)

criteria <- NULL
for(i in 1:5){
  fiti <- gllvm(DietData, DietData_habitat, family = "ZIP", num.lv = i, sd.errors = FALSE,
                formula = ~ EstSurfaceArea_msq + MaxDepth_m, seed = 1234)
  criteria[i] <- summary(fiti)$AICc
  names(criteria)[i] = i
}
criteria

t2 <- gllvm(DietData, DietData_habitat, family = "negative.binomial", num.lv = 2,
              formula = ~ EstSurfaceArea_msq + MaxDepth_m, seed = 1234)

t3 <- gllvm(DietData, DietData_habitat, family = "ZIP", num.lv = 2,
            formula = ~ EstSurfaceArea_msq + MaxDepth_m, seed = 1234)

t1 <- gllvm(DietData, DietData_traits, family = "negative.binomial", num.lv = 1,
      formula = ~ ForkLength)

coefplot(t1, cex.ylab = 0.7, mar = c(4, 9, 2, 1), mfrow=c(1,1))

coefplot(t2, cex.ylab = 0.7, mar = c(4, 9, 2, 1), mfrow=c(1,1), order = TRUE)

coefplot(t3, cex.ylab = 0.7, mar = c(4, 9, 2, 1), mfrow=c(1,1), order = TRUE)

