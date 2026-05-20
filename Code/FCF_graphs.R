library(ggplot2)
library(readxl)
library(dplyr)
library(broom)

DietData <- read_excel("Data/DietData.xlsx")
DietData_env <- read_excel("Data/DietData_env.xlsx")

# graphing weight v. length (which is FCF) to understand FCF trends between groups 
ggplot(DietData_env %>% filter(LifeStage == "YoY"), aes(x = ForkLength, y =FishWeight, color= SpeciesCode)) +
  geom_point(color = "black", size = 2, alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE) + 
  facet_wrap(~ SpeciesCode, scales = "free")

ggplot(DietData_env %>% filter(LifeStage == "YoY"), aes(x = ForkLength, y =FishWeight, color= SpeciesCode)) +
  geom_point(aes(color = SpeciesCode), size = 2, alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE)

#getting slopes (slope= approximate for FCF) ** all slopes are 0.351? 
DietData_env %>% 
  group_by(SpeciesCode) %>%
  do(tidy(lm(FishWeight ~ ForkLength, data = DietData_env %>% filter(LifeStage== "YoY")))) %>% 
  select(SpeciesCode, term, estimate)

#
for (species in unique(DietData_env$SpeciesCode)) {
  DietData_temp <- DietData_env %>%
    filter(LifeStage == "YoY",
           SpeciesCode == species)
  print(species)
  print(summary(lm(FishWeight ~ ForkLength, data = DietData_temp)))
}
summary(lm(FishWeight ~ ForkLength, data = DietData_env %>% filter(LifeStage== "YoY")))

#pulling info from our data sets to compare to literature lmao 
#FCF for CH: 1.000656, CO: 1.106940, SH: 1.101246
FCF_average <- aggregate(FultonConditionFactor ~ SpeciesCode, data= DietData_env, FUN = mean)
ForkLength_average <- aggregate(ForkLength~ SpeciesCode, data= DietData_env, FUN = mean)