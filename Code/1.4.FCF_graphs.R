library(ggplot2)
library(readxl)

#preparing data sets
#Gut lavages 
DietData <- read_excel("Data/DietData.xlsx")
DietData_env <- read_excel("Data/DietData_env.xlsx")

#Snorkel Surveys 
  library(readr)
#Juveniles df
  Salmonid_SummerMeasurements <- read_csv("Data/NPS_IMD_SFAN_Salmonids_SummerMeasurementsDataset.csv")
  Juvenile_survey <- Salmonid_SummerMeasurements%>% 
    filter(FieldSeason %in% c(2020, 2022), 
         Watershed %in% c("Redwood Creek"), 
         SpeciesCode%in% c("SH", "CO", "CH"))
  
  #Both years with FCF calculated 
  Juvenile_survey_FCF <- Juvenile_survey %>%
    mutate(FCF= (FishWeight_g /(ForkLength_mm^3))*100000)
  #2020 only with FCF calculated 
  Juvenile_survey_2020 <- Juvenile_survey_FCF %>% filter(FieldSeason== 2020) 
  #2022 only with FCF calculated 
  Juvenile_survey_2022 <- Juvenile_survey_FCF %>% filter(FieldSeason== 2022) 
  
# Spawning adults df
Salmonids_SnorkelCounts <- read.csv("Data/NPS_IMD_SFAN_Salmonids_SnorkelCountsDataset.csv")
Spawner_survey <- Salmonids_SnorkelCounts%>%
  filter(FieldSeason %in% c(2020, 2022), 
         Watershed %in% c("Redwood Creek"))
#_______________________________________________________________________________________________________________
  
# graphing weight v. length (which is FCF kinda) to understand FCF trends between groups 
#Gut lavage data set 
ggplot(DietData_env %>% filter(LifeStage == "YoY"), aes(x = ForkLength, y =FishWeight, color= SpeciesCode)) +
  geom_point(color = "black", size = 2, alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE) + 
  facet_wrap(~ SpeciesCode, scales = "free")
#still gut lavage, all on same graph 
ggplot(DietData_env %>% filter(LifeStage == "YoY"), aes(x = ForkLength, y =FishWeight, color= SpeciesCode)) +
  geom_point(aes(color = SpeciesCode), size = 2, alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE)

#Snorkel survey (juveniles) data set 
ggplot(Juvenile_survey %>% filter(LifeStage == "YoY"), aes(x = ForkLength_mm, y =FishWeight_g, color= SpeciesCode)) +
  geom_point(color = "black", size = 2, alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE) + 
  facet_wrap(~ SpeciesCode, scales = "free")
#snorkel survey all on same graph 
ggplot(Juvenile_survey %>% filter(LifeStage == "YoY"), aes(x = ForkLength_mm, y =FishWeight_g, color= SpeciesCode)) +
  geom_point(aes(color = SpeciesCode), size = 2, alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE)

#________________________________________________________________________________________________________________

#getting slopes (slope= approximate for FCF) ## all these can be broken down bby year as well, within both data sets 
library(dplyr)
library(broom)

#gut lavage slopes
for (species in unique(DietData_env$SpeciesCode)) {
  DietData_temp <- DietData_env %>%
    filter(LifeStage == "YoY",
           SpeciesCode == species)
  print(species)
  print(summary(lm(FishWeight ~ ForkLength, data = DietData_temp)))
}
summary(lm(FishWeight ~ ForkLength, data = DietData_env %>% filter(LifeStage== "YoY")))

#snorkel survey slopes
for (species in unique(Juvenile_survey_FCF$SpeciesCode)) {
  Juvenile_survey_temp <- Juvenile_survey_FCF %>%
    filter(LifeStage == "YoY",
           SpeciesCode == species)
  print(species)
  print(summary(lm(FishWeight_g ~ ForkLength_mm, data = Juvenile_survey_temp)))
}
summary(lm(FishWeight_g ~ ForkLength_mm, data = Juvenile_survey_FCF %>% filter(LifeStage== "YoY")))

#gut lavage stats 
#FCF for CH: 1.000656, CO: 1.106940, SH: 1.101246
FCF_average <- aggregate(FultonConditionFactor ~ SpeciesCode, data= DietData_env, FUN = mean)
print(FCF_average)
ForkLength_average <- aggregate(ForkLength~ SpeciesCode, data= DietData_env, FUN = mean)
print(ForkLength_average)

#snorkel survey stats - similar values to gut lavage yay
# FCF for CH: 0.9909656, CO: 1.1193549, SH: 1.1015537
Juvenile_FCF_average <- aggregate(FCF ~ SpeciesCode, data= Juvenile_survey_FCF, FUN = mean)
print(Juvenile_FCF_average)
Juvenile_ForkLength_average <- aggregate(ForkLength_mm~ SpeciesCode, data= Juvenile_survey_FCF, FUN = mean)
print(Juvenile_ForkLength_average)

#_______________________________________________________________________________________________________________
## Graphs comparing FCF to salmonid densities by year- only snorkel survey. due diligence that density doesn't effect FCF 
## Snorkel survey amount of juveniles each species per year 
Juvenile_survey %>%
  filter(round(FieldSeason) %in% c(2020, 2022)) %>%
ggplot(aes(x= as.factor(FieldSeason), fill =SpeciesCode))+
  geom_bar(position = "dodge", width = 0.7) +
  labs(title = "Juvenile Presence by Year", 
       x= "Feild Season",
       y= "Count", 
       fill= "Species")+
  theme_minimal()

#FCF juveniles per year (snorkel survey)
ggplot(Juvenile_survey_FCF, aes(x=factor(FieldSeason), y=FCF, fill = SpeciesCode))+
  geom_boxplot(outlier.shape = 16, outlier.alpha = 0.5,position = position_dodge( preserve = "single")) +
  geom_jitter(position = position_dodge(width = 0.75), alpha = 0.1, color = "grey") +
  labs(title= "Juvenile FCF by Year", 
       x= "Field Season",
       y= "Fulton's Condition Factor", 
       fill= "Species")+
  theme_minimal()

#_______________________________________________________________________________________________________________
#Maps for juvenile fish presence by latitude and longitude each year (snorkel survey)- due dilligence for FCF and denisty. 
#2020
Juvenile_survey %>%
  filter(FieldSeason== 2020) %>%
  ggplot(aes(x= Longitude, y= Latitude, color = SpeciesCode, size = NumberOfFish))+
  geom_jitter(width = 0.001, height = 0.001, alpha=0.5) +
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

#Juvenile fish FCF by long/ lat by each year (snorkel survey FCFs)
#2020
Juvenile_survey_FCF %>%
  filter(FieldSeason== 2020) %>%
  drop_na(FCF, NumberOfFish, Longitude, Latitude)%>%
  ggplot(aes(x= Longitude, y= Latitude, color= FCF))+
  geom_jitter(size = 1, width = 0.001, height = 0.001, alpha= 0.7) +
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
  geom_jitter(size = 1, width = 0.001, height = 0.001, alpha= 0.4) +
  scale_alpha_continuous(range = c(0.1, 1.0)) +
  scale_color_gradient(low = "cornsilk", high= "deeppink4")+
  theme_minimal()+
  labs(title = "Juvenile Fish Survey 2022", 
       x = "Longitude", 
       y = "Latitude", 
       color = "FCF")








