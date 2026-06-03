# last updated: 06/03/26
# creating salmon snorkel survey and seining + electrofishing measurement survey data for general discussion
# revised syntax for certain variables, filtered dates, location, and species life stage to within study range and selected relevant variables

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
  # limit to years in gut content data set
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
  # pull stream number from location code
  mutate(Date = as.Date(StartDate),
         StreamNumber = sub("RW-01-", "", LocationCode)) %>%
  # limit to years in gut content data set
  # study system is only redwood creek, exclude fern creek
  # limit survey dates to within the gut contents data set
  # limit life stage to within the gut contents data set
  filter(FieldSeason %in% c(2020, 2022),
         Watershed == "Redwood Creek",
         StreamName != "Fern Creek",
         month(Date) == 7 | month(Date) == 8,
         LifeStage %in% c("YoY", "1+")) %>%
  # salmon and count data
  # location data
  # habitat data
  # morphometric data
  select(SpeciesCode, LifeStage, NumberOfFish,
         ID, EventID, LocationID, StartDate, FieldSeason, Latitude, Longitude, StreamNumber,
         Watershed, StreamName,
         ForkLength_mm, FishWeight_g) %>%
  mutate(FultonConditionFactor = (FishWeight_g /(ForkLength_mm^3))*100000)

# Export ------------------------------------------------------------------

write.csv(fish_snorkel,"Data/Created_Data/Fish_Snorkel.csv", row.names = FALSE)
write.csv(fish_measurement, "Data/Created_Data/Fish_Measurement.csv", row.names = FALSE)
