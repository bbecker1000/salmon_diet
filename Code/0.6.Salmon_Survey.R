library(tidyverse)

### Download and clean up salmon count data

salmon_counts <- read.csv("Data/NPS_IMD_SFAN_Salmonids_SnorkelCountsDataset.csv")
salmon_counts <- salmon_counts %>%
  mutate(Date = as.Date(StartDate)) %>%
  filter(FieldSeason %in% c(2017, 2018, 2019,2020,2021,2022,2023,2024,2025), # limit to years within BMI & gut content datasets
         Watershed == "Redwood Creek", # study system is only redwood creek
         month(Date) == 7 | month(Date) == 8) %>%
         #UnitCode == "MUWO") # Study site
  select(SpeciesCode, LifeStage, Count,  # sample-specific data
         EventID, LocationID, SnorkelSurveysID, BasinWideUnitSnorkel, StartDate, FieldSeason,Latitude, Longitude,  # unit data
         Watershed, StreamName, HabitatDescriptionSnorkel, # habitat data
         Pass, Visibility, TimeElapsed_s, # sampling informaiton
         )

salmon_counts$LifeStage[salmon_counts$LifeStage == "1"] <- "1+"
salmon_counts$LifeStage[salmon_counts$LifeStage == "yoy"] <- "YoY"

ggplot(salmon_counts, aes(x = SpeciesCode, y = Count, fill = LifeStage)) +
  geom_col() +
  facet_wrap(~FieldSeason)

ggsave("Figures/Salmon_Counts_by_Year.png")

