library(ggplot2)
library(readxl)

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

#getting slopes (slope= approximate for FCF)
library(dplyr)
library(broom)
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


##comparing FCF to salmonid densities by year
# preparing data sets
library(readr)

Salmonid_SummerMeasurements <- read_csv("Data/NPS_IMD_SFAN_Salmonids_SummerMeasurementsDataset.csv")
Juvinile_survey <- Salmonid_SummerMeasurements%>% 
  filter(FieldSeason %in% c(2020, 2022), 
         Watershed %in% c("Redwood Creek"), 
         SpeciesCode%in% c("SH", "CO", "CH")) %>%
  mutate(FCF= (FishWeight_g /(ForkLength_mm^3))*100000)
Salmonids_SnorkelCounts <- read.csv("Data/NPS_IMD_SFAN_Salmonids_SnorkelCountsDataset.csv")
Spawner_survey <- Salmonids_SnorkelCounts%>%
  filter(FieldSeason %in% c(2020, 2022), 
         Watershed %in% c("Redwood Creek"))

View(Juvinile_survey)
View(Spawner_survey)

## amount of each species per year 
ggplot(Juvinile_survey, aes(x= SpeciesCode, fill =factor(FieldSeason)))+
  geom_bar(position = "dodge", width = 0.7) +
  labs(title = "", 
       x= "Species",
       y= "Count", 
       fill= "FieldSeason")+
  theme_minimal()

#FCF per year
ggplot(Juvinile_survey, aes(x=SpeciesCode, y=FCF, fill = factor(FieldSeason)))+
  geom_boxplot(outlier.shape = 16, outlier.alpha = 0.5) +
  geom_jitter(width = 0.1, alpha = 0.1, color = "grey") +
  labs(title= "", 
       x= "Species",
       y= "Fulton's Condition Factor", 
       fill= "Feild Season")+
  theme_minimal()

ggplot(Juvinile_survey, aes(x = SpeciesCode, y = FCF, fill = as.factor(FieldSeason))) +
  stat_summary(fun = mean, geom = "bar", position = position_dodge(width = 0.9))
  labs(
    x = "Species",
    y = "Average Fulton's Condition Factor (FCF)",
    fill = "Field Season"
  ) +
  theme_minimal()











