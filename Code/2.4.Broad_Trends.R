# Last updated 06/03/2026
# Results:
  # fulton condition factor
    # a) fish measurements - sig. b/n CH[0.997], SH[1.108], CO[1.119] & FieldSeason 2022 > 2020 [0.054] dif.
    # b) diet data original - sig. b/n CH[1.015], SH[1.147], CO[1.126] & FieldSeason 2022 > 2020 [0.056] dif.
    # c) by river reach - sig. dif. b/n river reach FCF in survey but not gut lavage data...
  # counts
    # proportions from seining + efishing seems to match diet data better (makes sense given they used similar methods to catch fish for gut lavage)
    # snorkel may be most representative but have the highest error...
# NOTE: almost all graphed data is based on YoY lifestage... may want to make adjustments for final report
  # might want to do stat. analysis by interaction

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(patchwork)

# Calling files -----------------------------------------------------------

fish_snorkel <- read.csv("Data/Created_Data/Fish_Snorkel.csv")
fish_measurement <- read.csv("Data/Created_Data/Fish_Measurement.csv")
diet_data_original <- read.csv("Data/Created_Data/Diet_Data_Original.csv")

# Raw plotting ------------------------------------------------------------

### fulton condition factor

## fish measurement

# fish measurement weight vs length

# limit life stage and dimensions to diet data set
survey_length_weight_plot <- ggplot(fish_measurement %>% filter(LifeStage == "YoY", between(ForkLength, 30, 100)), 
       aes(x = ForkLength, y = FishWeight, color = SpeciesCode)) +
  geom_point() +
  facet_wrap(~FieldSeason) +
  stat_smooth(formula = y ~ splines::ns(x, 3),
              method = "glm")

# fish measurement fulton condition factor

survey_FCF_plot <- ggplot(fish_measurement %>% filter(LifeStage == "YoY", between(ForkLength, 30, 100)), 
       aes(x = SpeciesCode, y = FultonConditionFactor)) +
  geom_boxplot() +
  facet_wrap(~FieldSeason)

## diet data original

# diet data original weight vs length

diet_length_weight_plot <- ggplot(diet_data_original %>% filter(LifeStage == "YoY"), 
       aes(x = ForkLength, y = FishWeight, color = SpeciesCode)) +
  geom_point() +
  facet_wrap(~FieldSeason) +
  stat_smooth(formula = y ~ splines::ns(x, 3),
              method = "glm")

# diet data original fulton condition factor

diet_FCF_plot <- ggplot(diet_data_original %>% filter(LifeStage == "YoY"),
                        aes(x = SpeciesCode, y = FultonConditionFactor)) +
  geom_boxplot() +
  facet_wrap(~FieldSeason)

## by river reach

ggplot(fish_measurement %>% filter(LifeStage == "YoY", between(ForkLength, 30, 100), RiverReach != "Foot4"), 
       aes(x = RiverReach, y = FultonConditionFactor, color = SpeciesCode)) +
  geom_boxplot() +
  facet_wrap(~FieldSeason)

ggplot(fish_measurement %>% filter(LifeStage == "YoY", between(ForkLength, 30, 100), RiverReach != "Foot4"), 
       aes(x = RiverReach, y = FultonConditionFactor)) +
  geom_boxplot() +
  facet_wrap(~FieldSeason)

### rough counts

# snorkel 

snorkel_count_plot <- ggplot(fish_snorkel, 
                             aes(x = SpeciesCode, y = Count, fill = LifeStage)) +
  geom_col() +
  facet_wrap(~FieldSeason) +
  labs(title = "snorkel")

# seining + efishing

measurement_count_plot <- ggplot(fish_measurement, 
                                 aes(x = SpeciesCode, y = NumberOfFish, fill = LifeStage)) +
  geom_col() +
  facet_wrap(~FieldSeason) +
  labs(title = "seining + efish")

# diet 

diet_count_plot <- ggplot(diet_data_original %>% 
                            group_by(SpeciesCode, FieldSeason, LifeStage) %>% 
                            summarize(Count = n()),
                          aes(x = SpeciesCode, y = Count, fill = LifeStage)) +
  geom_col() +
  facet_wrap(~FieldSeason) +
  labs(title = "diet")

### river mapping

river_map <- rbind(unique(diet_data_original %>% select(Latitude, Longitude, RiverReach, StreamNumber)) %>% mutate(Data = "Diet"),
      unique(fish_measurement %>% select(Latitude, Longitude, RiverReach, StreamNumber)) %>% mutate(Data = "Seining + efish"),
      unique(fish_snorkel %>% select(Latitude, Longitude, RiverReach, StreamNumber)) %>% mutate(Data = "Snorkel"))

ggplot(river_map %>% arrange(RiverReach, StreamNumber), aes(x = Longitude, y = Latitude)) +
  geom_path(aes(group = RiverReach, linetype = RiverReach), linewidth = 1) + 
  geom_jitter(aes(shape = Data, color = Data), size = 2, width = 0.0005, height = 0.0005)

# Combining plots ---------------------------------------------------------

### fulton condition factor

(survey_length_weight_plot + survey_FCF_plot) / (diet_length_weight_plot + diet_FCF_plot)
ggsave("Figures/New_Figures/FCF_Comparison.png", width = 10, height = 8)

### counts

snorkel_count_plot + measurement_count_plot + diet_count_plot
ggsave("Figures/New_Figures/Count_Comparison.png", width = 15, height = 10)

# Significance testing ----------------------------------------------------

### fulton condition factor

## fish measurement

# by species

summary(glm(FultonConditionFactor ~ SpeciesCode + RiverReach + FieldSeason,
            fish_measurement %>% filter(LifeStage == "YoY", between(ForkLength, 30, 100)),
            family = gaussian(link = "identity")))

# diet data original

summary(glm(FultonConditionFactor ~ SpeciesCode + RiverReach + FieldSeason,
            diet_data_original %>% filter(LifeStage == "YoY"),
            family = gaussian(link = "identity")))
