# last updated: 6/23/26
# Results: 2022 sig higher FCFs with SH being sig higher than CO 
# Notes: Does it make sense to only include YoY in this analysis ? i think so 

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(readxl)
library(vegan)
library(ggrepel)
library(dplyr)
library(lme4)
library(car)

# Calling files -----------------------------------------------------------

diet_data_original <- read_csv("Data/Created_Data/Diet_Data_Original.csv")
fish_measurement <- read.csv("Data/Created_Data/Fish_Measurement.csv")

diet_data_original_traits <- read_csv("Data/Created_Data/Diet_Taxa_Original_Traits.csv")

# Modifying data sets -----------------------------------------------------

#only include YoY in FCF analysis  

diet_data_year_filtered <- diet_data_original %>%
  filter(LifeStage == "YoY")

fish_measurement_year_filtered <- fish_measurement %>%
  filter(LifeStage == "YoY")

#exclude CH for statistics 

diet_data_sans_CH <- subset(diet_data_year_filtered, SpeciesCode %in% c("CO", "SH"))

fish_measurement_sans_CH <- subset(fish_measurement_year_filtered, SpeciesCode %in% c("CO", "SH"))

# Plotting FCF for each salmon species between Field Season ---------------

#gut lavage data set 

ggplot(data= diet_data_year_filtered, 
       aes(x= as.factor(FieldSeason), y= FultonConditionFactor, fill= SpeciesCode)) + 
  geom_boxplot() +
  labs(title= "Fulton Condition Factor by Feild Season, gut lavage", 
       x= "Field Season",
       y= "Fulton Condition Factor")

# seining/ e fishing data set 

ggplot(fish_measurement_year_filtered, 
       aes(x= as.factor(FieldSeason), y= FultonConditionFactor, fill= SpeciesCode)) + 
  geom_boxplot() +
  labs(title= "Fulton Condition Factor by Feild Season, e fish",
       x= "Field Season",
       y= "Fulton Condition Factor")

# Significance testing ----------------------------------------------------

# glm for CH presence effect on FCF s, using only sub-yearlings and e fish data set (bigger)

summary(FCF_glm <- glm(FultonConditionFactor~ FieldSeason + SpeciesCode, 
               data = fish_measurement_sans_CH))

anova(FCF_glm)

#comparing to gut lavage data set 

summary(FCF_glm_diet <- glm(FultonConditionFactor ~ FieldSeason + SpeciesCode, 
                       data = diet_data_sans_CH))

anova(FCF_glm_diet)


