# Last updated 07/21/2026
# Results:
  # fulton condition factor
    # a) fish measurements - sig. b/n CH[0.997], SH[1.108], CO[1.119] & FieldSeason 2022 > 2020 [0.054] dif.
    # b) diet data original - sig. b/n CH[1.015], SH[1.147], CO[1.126] & FieldSeason 2022 > 2020 [0.056] dif.
    # c) by river reach - sig. dif. b/n river reach FCF in survey but not gut lavage data...
    # d) NZMS - no sig. dif. across those that consume or don't consume NZMS
  # counts
    # proportions from seining + efishing seems to match diet data better (makes sense given they used similar methods to catch fish for gut lavage)
    # snorkel may be most representative but have the highest error...
# NOTE: almost all graphed data is based on YoY lifestage... may want to make adjustments for final report
  # might want to do stat. analysis by interaction

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(patchwork)
library(glmmTMB)
library(DHARMa)

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
                                    aes(x = ForkLength, y = FishWeight, color = SpeciesCode, shape = SpeciesCode)) +
  geom_point(alpha = 0.5) +
  facet_wrap(~FieldSeason) +
  stat_smooth(formula = y ~ x,
              method = "glm",
              method.args = list(family = gaussian(link = "log"))) +
  scale_color_discrete(labels = c("CH" = "Chinook", "CO" = "Coho", "SH" = "Steelhead")) +
  scale_shape_discrete(labels = c("CH" = "Chinook", "CO" = "Coho", "SH" = "Steelhead")) +
  labs(x = "Fork Length (mm)",
       y = "Fish Weight (g)",
       color = "Species",
       shape = "Species")

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
  stat_smooth(formula = y ~ x,
              method = "glm",
              method.args = list(family = gaussian(link = "log")))

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

## new zealand mud snail

diet_data_original %>%
  filter(StreamNumber %in% nzms_stream_number, SpeciesCode == "SH") %>%
  mutate(NZMS = case_when(Littorinimorpha == 0 ~ "Empty",
                          Littorinimorpha > 0 ~ "Eaten")) %>%
  ggplot(aes(x = NZMS, y = FultonConditionFactor, color = LifeStage)) +
  geom_boxplot()

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
                            filter(LifeStage != "NA") %>%
                            group_by(SpeciesCode, FieldSeason, LifeStage) %>% 
                            summarize(Count = n()),
                          aes(x = SpeciesCode, y = Count, fill = LifeStage)) +
  geom_col() +
  facet_wrap(~FieldSeason) +
  labs(title = "diet")

### river mapping

river_map <- rbind(diet_data_original %>%
                     filter(LifeStage == "YoY") %>%
                     select(Latitude, Longitude, RiverReach, StreamNumber, SpeciesCode, FieldSeason) %>% 
                     mutate(Data = "Gut Lavage",
                            Count = 1),
                   fish_measurement %>% 
                     filter(LifeStage == "YoY") %>%
                     select(Latitude, Longitude, RiverReach, StreamNumber, SpeciesCode, NumberOfFish, FieldSeason) %>% 
                     mutate(Data = "E-fishing") %>%
                     rename("Count" = "NumberOfFish"),
                   fish_snorkel %>% 
                     filter(LifeStage == "YoY") %>%
                     select(Latitude, Longitude, RiverReach, StreamNumber, SpeciesCode, Count, FieldSeason) %>% 
                     mutate(Data = "Snorkel"))

ggplot(river_map, aes(x = Data, y = Count, fill = SpeciesCode)) +
  stat_summary(fun = sum,
               geom = "col",
               position = "fill") +
  stat_summary(aes(label = after_stat(y)),
               fun = sum,
               geom = "text",
               position = position_fill(vjust = 0.5)) +
  facet_wrap(~RiverReach) 
ggsave("Figures/New_Figures/River_Reach_Species_Distribution.png", width = 10, height = 7, units = "in")

ggplot(river_map %>% arrange(RiverReach, StreamNumber), aes(x = Longitude, y = Latitude)) +
  geom_path(aes(group = RiverReach, linetype = RiverReach), linewidth = 1) + 
  geom_jitter(aes(shape = Data, color = Data), size = 2, width = 0.0005, height = 0.0005) +
  facet_wrap(~Data)

ggplot(river_map %>% arrange(RiverReach, StreamNumber), aes(x = Longitude, y = Latitude)) +
  geom_path(aes(group = RiverReach, linetype = RiverReach, color = RiverReach), linewidth = 1)

combined_prop_df <- rbind(fish_snorkel %>%
                            filter(LifeStage == "YoY") %>%
                            group_by(SpeciesCode, FieldSeason) %>%
                            summarise(Count = sum(Count, na.rm = TRUE),
                                      .groups = "drop") %>%
                            group_by(FieldSeason) %>%
                            mutate(Proportion = Count / sum(Count),
                                   Data = "Snorkel") %>%
                            ungroup(),
                          fish_measurement %>%
                            filter(LifeStage == "YoY") %>%
                            group_by(SpeciesCode, FieldSeason) %>%
                            summarise(Count = sum(NumberOfFish, na.rm = TRUE),
                                      .groups = "drop") %>%
                            group_by(FieldSeason) %>%
                            mutate(Proportion = Count / sum(Count),
                                   Data = "E-fishing") %>%
                            ungroup())

### snorkel survey

combined_count_df <- rbind(fish_snorkel %>%
        select(SpeciesCode, Count, LifeStage, FieldSeason) %>%
        mutate(Data = "Snorkel"),
      fish_measurement %>%
        select(SpeciesCode, NumberOfFish, LifeStage, FieldSeason) %>%
        rename(Count = NumberOfFish) %>%
        mutate(Data = "E-fishing"))

ggplot(combined_count_df, aes(x = SpeciesCode, y = Count, fill = LifeStage)) +
  geom_col() +
  facet_wrap(Data ~ FieldSeason)
ggsave("Figures/New_Figures/Raw_Counts_Surveys.png", width = 10, height = 8, unit = "in")

combined_prop_df <- rbind(fish_snorkel %>%
        filter(LifeStage == "YoY") %>%
        group_by(SpeciesCode, FieldSeason) %>%
        summarise(Count = sum(Count, na.rm = TRUE),
                  .groups = "drop") %>%
        group_by(FieldSeason) %>%
        mutate(Proportion = Count / sum(Count),
               Data = "Snorkel") %>%
        ungroup(),
      fish_measurement %>%
        filter(LifeStage == "YoY") %>%
        group_by(SpeciesCode, FieldSeason) %>%
        summarise(Count = sum(NumberOfFish, na.rm = TRUE),
                  .groups = "drop") %>%
        group_by(FieldSeason) %>%
        mutate(Proportion = Count / sum(Count),
               Data = "E-fishing") %>%
        ungroup())

ggplot(combined_prop_df, aes(x = as.factor(FieldSeason), y = Proportion, fill = SpeciesCode)) +
  geom_col() +
  facet_wrap(~Data)
ggsave("Figures/New_Figures/Proportion_Surveys.png", width = 8, height = 6, units = "in")

ggplot(snorkel_survey_data %>%
         filter(SpeciesCode %in% c("CH", "CO", "SH"),
                LifeStage == "YoY") %>%
         group_by(FieldSeason, SpeciesCode) %>%
         summarize(Total = sum(Count), .groups = "drop") %>%
         group_by(FieldSeason) %>%
         mutate(YearTotal = sum(Total)) %>%
         ungroup() %>%
         mutate(Highlight = case_when(FieldSeason %in% c(2022,2025) ~ "Yes",
                                      .default = "No")), 
       aes(x = FieldSeason, y = Total, fill = SpeciesCode)) +
  geom_col() +
  scale_color_manual(values = c("White","Black")) +
  geom_text(data = plot_data %>% filter(SpeciesCode == "CH", Total > 0),
            aes(x = FieldSeason, y = YearTotal,
                label = paste0("Chinook: ", Total)),
            inherit.aes = FALSE,
            vjust = -0.5) + 
  labs(y = "Count",
       x = "Year") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

plot_data <- snorkel_survey_data %>%
  filter(SpeciesCode %in% c("CH", "CO", "SH"),
         LifeStage == "YoY") %>%
  group_by(FieldSeason, SpeciesCode) %>%
  summarize(Total = sum(Count), .groups = "drop") %>%
  group_by(FieldSeason) %>%
  mutate(YearTotal = sum(Total)) %>%
  ungroup()

ggplot(plot_data,
       aes(x = FieldSeason, y = Total, fill = SpeciesCode)) +
  geom_area() +
  geom_point(data = plot_data %>% filter(SpeciesCode == "CH", Total > 0),
             aes(x = FieldSeason, y = YearTotal),
             inherit.aes = FALSE,
             size = 4,
             shape = 21,
             fill = "#F8766D",
             stroke = 1) +
  geom_text(data = plot_data %>% filter(SpeciesCode == "CH", Total > 0),
            aes(x = FieldSeason, y = YearTotal,
                label = paste0("Chinook: ", Total)),
            inherit.aes = FALSE,
            vjust = -1) +
  labs(x = "Year") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

### habitat type

ggplot(diet_data_original %>% filter(LifeStage == "YoY"), aes(x = HabitatType, fill = SpeciesCode)) +
  geom_bar(position = "fill") +
  geom_text(aes(label = after_stat(count)),
            stat = "count",
            position = position_fill(vjust = 0.5)) +
  scale_fill_discrete(labels = c("CH" = "Chinook", "CO" = "Coho", "SH" = "Steelhead")) +
  labs(x = "Habitat Type",
       y = "Percentage",
       fill = "Species")
ggsave("Figures/New_Figures/Species_Habitat_Distribution.png", height = 6, width = 8, units = "in")

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

summary(glm(FultonConditionFactor ~ SpeciesCode + LifeStage + FieldSeason,
            diet_data_original,
            family = gaussian(link = "identity")))

## new zealand mud snail testing

# choose only the stream sites where the NZMS was found for comparisons across SH in the same pools
nzms_stream_number <- unique((diet_data_original %>% filter(Littorinimorpha > 0, SpeciesCode == "SH"))$StreamNumber)

# FCF as a function of NZMS found in gut controlled for size and age
summary(glm(FultonConditionFactor ~ Littorinimorpha + ForkLength + LifeStage,
            diet_data_original %>% filter(StreamNumber %in% nzms_stream_number, SpeciesCode == "SH"),
            family = "gaussian"))

# direct numbers
diet_data_original %>%
  filter(StreamNumber %in% nzms_stream_number, SpeciesCode == "SH") %>%
  mutate(NZMS = case_when(Littorinimorpha == 0 ~ "Empty",
                          Littorinimorpha > 0 ~ "Eaten")) %>%
  group_by(NZMS, StreamNumber, LifeStage) %>%
  summarize(mean(FultonConditionFactor), mean(ForkLength))

### fork length

summary(glm(ForkLength ~ SpeciesCode + LifeStage + FieldSeason,
            diet_data_original,
            family = gaussian(link = "identity")))

### stat testing across sample and population data
combined_morphometric_df <- rbind(diet_data_original %>% 
        select(FultonConditionFactor, ForkLength, SpeciesCode, FieldSeason, LifeStage, FishWeight) %>%
        mutate(Data = "Gut Lavage"),
      fish_measurement %>% 
        filter(between(ForkLength, 30, 100)) %>%
        select(FultonConditionFactor, ForkLength, SpeciesCode, FieldSeason, LifeStage, FishWeight) %>%
        mutate(Data = "E-fishing"))

fish_measurement %>% 
  group_by(SpeciesCode, FieldSeason, LifeStage) %>%
  drop_na() %>%
  summarize(mean(FultonConditionFactor), sd(FultonConditionFactor), mean(ForkLength), sd(ForkLength))

diet_data_original %>% 
  group_by(SpeciesCode, FieldSeason, LifeStage) %>%
  drop_na() %>%
  summarize(mean(FultonConditionFactor), sd(FultonConditionFactor), mean(ForkLength), sd(ForkLength))

combined_morphometric_df %>% 
  group_by(SpeciesCode, Data) %>%
  drop_na() %>%
  summarize(mean(FultonConditionFactor), sd(FultonConditionFactor))summary(glm(FultonConditionFactor ~ FieldSeason + LifeStage + SpeciesCode + Data,
            combined_morphometric_df,
            family = gaussian(link = "identity")))

qq <- glmmTMB(FultonConditionFactor ~ FieldSeason + LifeStage + SpeciesCode + Data,
       combined_morphometric_df,
       family = t_family(link = "identity"))

sim_res <- simulateResiduals(fittedModel = qq, n = 250)
plot(sim_res)

# Finalized plots ---------------------------------------------------------

FCF_summary <- combined_morphometric_df %>% 
  group_by(SpeciesCode, Data) %>% 
  summarize(FCF = format(round(mean(FultonConditionFactor, na.rm = TRUE), 2), nsmall = 2),
            sd = format(round(sd(FultonConditionFactor, na.rm = TRUE), 2), nsmall = 2),
            .groups = 'drop')

# fork length vs. fish weight colored by data, faceted by species w/ FCF text
ggplot(combined_morphometric_df %>% filter(LifeStage == "YoY"), 
       aes(x = ForkLength, y = FishWeight, color = Data, shape = Data)) +
  geom_point(alpha = 0.5) +
  facet_wrap(~SpeciesCode, 
             labeller = labeller(SpeciesCode = c("CH" = "Chinook", 
                                                 "CO" = "Coho",
                                                 "SH" = "Steelhead"))) +
  stat_smooth(formula = y ~ x,
              method = "glm",
              method.args = list(family = gaussian(link = "log")),
              color = "black",
              linewidth = 1.5) +
  stat_smooth(formula = y ~ x,
              method = "glm",
              method.args = list(family = gaussian(link = "log"))) +
  geom_text(data = FCF_summary %>% filter(Data == "E-fishing"),
            aes(x = -Inf,
                y = Inf,
                color = Data,
                label = paste0("FCF = ", FCF, " ± ", sd)),
            hjust = -0.05, vjust = 1.3, inherit.aes = FALSE, show.legend = FALSE, size = 5) +
  geom_text(data = FCF_summary %>% filter(Data == "Gut Lavage"),
            aes(x = -Inf,
                y = Inf,
                color = Data,
                label = paste0("FCF = ", FCF, " ± ", sd)),
            hjust = -0.05, vjust = 3, inherit.aes = FALSE, show.legend = FALSE, size = 5) +
  labs(x = "Fork Length (mm)",
       y = "Fish Weight (g)",
       color = "Data Type",
       shape = "Data Type") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size = 10,
                                  face = "bold"))
ggsave("Figures/New_Figures/FCF_Comparison_V2.png", width = 10, height = 6, units = "in")

# fork length faceted by data, colored by year, boxplot by species
ggplot(combined_morphometric_df %>% filter(LifeStage == "YoY"), aes(x = SpeciesCode, y = ForkLength)) +
  geom_boxplot(aes(color = as.factor(FieldSeason))) +
  scale_x_discrete(labels = c("CH" = "Chinook", "CO" = "Coho", "SH" = "Steelhead")) +
  facet_wrap(~Data) +
  labs(x = "Species",
       y = "Fork Length (mm)",
       color = "Year")
ggsave("Figures/New_Figures/FCF_Comp_Sample.png", width = 10, height = 8, units = "in")

ggplot(combined_morphometric_df %>% filter(LifeStage == "YoY"), aes(x = SpeciesCode, y = FultonConditionFactor)) +
  geom_boxplot(aes(color = as.factor(FieldSeason))) +
  facet_wrap(~Data)
