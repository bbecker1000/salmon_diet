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
#Juveniles df
Salmonid_SummerMeasurements <- read_csv("Data/NPS_IMD_SFAN_Salmonids_SummerMeasurementsDataset.csv")
Juvenile_survey <- Salmonid_SummerMeasurements%>% 
  filter(FieldSeason %in% c(2020, 2022), 
         Watershed %in% c("Redwood Creek"), 
         SpeciesCode%in% c("SH", "CO", "CH"))
Juvenile_survey_FCF <- Juvenile_survey %>%
  mutate(FCF= (FishWeight_g /(ForkLength_mm^3))*100000)
# Spawning adults df
Salmonids_SnorkelCounts <- read.csv("Data/NPS_IMD_SFAN_Salmonids_SnorkelCountsDataset.csv")
Spawner_survey <- Salmonids_SnorkelCounts%>%
  filter(FieldSeason %in% c(2020, 2022), 
         Watershed %in% c("Redwood Creek"))+
#View
View(Juvenile_survey)
View(Juvenile_survey_FCF)
View(Spawner_survey)


## amount of juveniles each species per year 
Juvenile_survey %>%
  filter(round(FieldSeason) %in% c(2020, 2022)) %>%
ggplot(aes(x= as.factor(FieldSeason), fill =SpeciesCode))+
  geom_bar(position = "dodge", width = 0.7) +
  labs(title = "Juvenile Presence by Year", 
       x= "Feild Season",
       y= "Count", 
       fill= "Species")+
  theme_minimal()

#FCF juveniles per year
ggplot(Juvenile_survey_FCF, aes(x=factor(FieldSeason), y=FCF, fill = SpeciesCode))+
  geom_boxplot(outlier.shape = 16, outlier.alpha = 0.5,position = position_dodge( preserve = "single")) +
  geom_jitter(position = position_dodge(width = 0.75), alpha = 0.1, color = "grey") +
  labs(title= "Juvenile FCF by Year", 
       x= "Field Season",
       y= "Fulton's Condition Factor", 
       fill= "Species")+
  theme_minimal()

#Juvenile fish presence by latitude and longitude each year 
#2020
Juvenile_survey %>%
  filter(FieldSeason== 2020) %>%
  ggplot(aes(x= Longitude, y= Latitude, color = SpeciesCode, alpha = NumberOfFish))+
  geom_jitter(size = 3, width = 0.01, height = 0.01) +
  scale_alpha_continuous(range = c(0.4, 1.0)) +
  scale_color_manual(values = c(
    "SH" = "darkcyan",
    "CO" = "goldenrod"
  ))+
  theme_minimal()+
  labs(title = "Juvenile Fish Survey 2020", 
       x = "Longitude", 
       y = "Latitude", 
       color = "Species")

#2022
Juvenile_survey %>%
  filter(FieldSeason== 2022) %>%
  ggplot(aes(x= Longitude, y= Latitude, color = SpeciesCode, alpha = NumberOfFish))+
  geom_jitter(size = 1, width = 0.001, height = 0.001) +
  scale_alpha_continuous(range = c(0.3, 1.0)) +
  scale_color_manual(values = c(
    "SH" = "darkcyan",
    "CO" = "goldenrod",
    "CH" = "mediumvioletred"
  ))+
  theme_minimal()+
  labs(title = "Juvenile Fish Survey 2022", 
       x = "Longitude", 
       y = "Latitude", 
       color = "Species")

#Juvenile fish FCF by long/ lat by each year 
#2020
Juvenile_survey_FCF %>%
  filter(FieldSeason== 2020) %>%
  drop_na(FCF, NumberOfFish, Longitude, Latitude)%>%
  ggplot(aes(x= Longitude, y= Latitude, color= FCF))+
  geom_jitter(size = 1, width = 0.001, height = 0.001, alpha= 1) +
  scale_alpha_continuous(range = c(0.1, 1.0)) +
  scale_color_gradient(low = "cornsilk", high= "deeppink4")+
  theme_minimal()+
  labs(title = "Juvenile Fish Survey 2020", 
       x = "Longitude", 
       y = "Latitude", 
       color = "FCF")

#2022
Juvenile_survey_FCF %>%
  filter(FieldSeason== 2022) %>%
  drop_na(FCF, NumberOfFish, Longitude, Latitude)%>%
  ggplot(aes(x= Longitude, y= Latitude, color= FCF))+
  geom_jitter(size = 3, width = 0.01, height = 0.01, alpha= 0.4) +
  scale_alpha_continuous(range = c(0.1, 1.0)) +
  scale_color_gradient(low = "cornsilk", high= "deeppink4")+
  theme_minimal()+
  labs(title = "Juvenile Fish Survey 2022", 
       x = "Longitude", 
       y = "Latitude", 
       color = "FCF")










