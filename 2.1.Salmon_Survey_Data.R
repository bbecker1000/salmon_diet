# last updated: 05/28/26
# 


# Libraries -----------------------------------------------------------------

library(tidyverse)

# Calling files -----------------------------------------------------------

snorkel_survey_data <- read.csv("Data/NPS_IMD_SFAN_Salmonids_SnorkelCountsDataset.csv")
measurement_survey_data <- read_csv("Data/NPS_IMD_SFAN_Salmonids_SummerMeasurementsDataset.csv")

# Processing data and creating dataframes ---------------------------------

fish_snorkel <- snorkel_survey_data %>%
  # pull stream number from location code
  # correct life stage yoy syntax
  mutate(Date = as.Date(StartDate),
         StreamNumber = sub("RW-01-", "", LocationCode),
         LifeStage = case_match(LifeStage,
                                "yoy" ~ "YoY",
                                .default = LifeStage)) %>%
  # limit to years gut content data set
  # study system is only redwood creek, exclude fern creek
  # limit survey dates to within the gut contents data set
  filter(FieldSeason %in% c(2020, 2022), 
         Watershed == "Redwood Creek", 
         StreamName != "Fern Creek",
         month(Date) == 7 | month(Date) == 8) %>%
  # salmon and count data
  # location data
  # habitat data
  # quality data
  select(SpeciesCode, LifeStage, Count,
         EventID, LocationID, SnorkelSurveysID, BasinWideUnitSnorkel, StartDate, FieldSeason, Latitude, Longitude, StreamNumber,
         Watershed, StreamName, HabitatDescriptionSnorkel, 
         Pass, Visibility, TimeElapsed_s)


fish_measurement <- measurement_survey_data %>% 
  mutate(Date = as.Date(StartDate),
         StreamNumber = sub("RW-01-", "", LocationCode)) %>%
  filter(FieldSeason %in% c(2020, 2022),
         Watershed == "Redwood Creek",
         StreamName != "Fern Creek",
         month(Date) == 7 | month(Date) == 8,
         LifeStage %in% c("YoY", "1+"))
