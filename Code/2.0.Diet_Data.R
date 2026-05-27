# creating the diet data, environmental data, and trait data for gllvm

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(readxl)

# Calling files -----------------------------------------------------------

# download raw data
gut_contents_22 <- read_xlsx("Data/2022_GutContents.xlsx")
gut_contents_20 <- read_xlsx("Data/2020_GutContents_terr_aqua_corrected.xlsx")
fish_data_22 <- read_xlsx("Data/RW_2022_FishData.xlsx")
fish_data_20 <- read_xlsx("Data/RW_2020_FishData.xlsx")
habitat_data <- read_xlsx("Data/R.Sainz_Redwood and Fern Habitat Data_2020&2022_poolcomplexity.xlsx")
location_data <- read_xlsx("Data/Redwood and Fern Habitat and Location Data_2020&2022.xlsx")

# Processing data and creating dataframes -----------------------------------------------------

### gut lavage diet data

# filter and combine data frames
gut_contents_combined <- rbind(
  gut_contents_20 %>% 
    select(Sample_ID, Order, Family, Terrestrial_or_Aquatic, Lifestage, Length_mm, Width_mm),
  gut_contents_22 %>% 
    select(SampleID_New, Order, Family, Terrestrial_or_Aquatic, Lifestage, Length_mm, Width_mm) %>%
    rename(Sample_ID = SampleID_New)
) %>%
  # replace NA with unknown in "Terrestrial_or_Aquatic" column
  # Create new category that differentiates between habitat preference within taxa
  mutate(Terrestrial_or_Aquatic = case_match(Terrestrial_or_Aquatic, 
                                             NA ~ "unknown", 
                                             .default = Terrestrial_or_Aquatic),
         Taxa_Habitat = paste0(Order, "_", Terrestrial_or_Aquatic))

# create lists of taxa columns to remove
low_count_taxa_original <- (gut_contents_combined %>%
                              group_by(Order) %>%
                              summarize(Count = n()) %>%
                              filter(Count < 10))$Order
non_food_taxa_original <- c("detritus_total", "unk_invert", "Unknown", "sand_gravel_total", "unk_plant_material")
low_count_taxa_terrestrial_or_aquatic <- (gut_contents_combined %>%
                      group_by(Taxa_Habitat) %>%
                      summarize(count = n()) %>%
                      filter(count < 10))$Taxa_Habitat
non_food_taxa_terrestrial_or_aquatic <- c("unk_invert_unknown", "Unknown_unknown", "unk_invert_terrestrial", "detritus_total_unknown", 
                    "trash_unknown", "unk_plant_material_unknown", "unk_plant_material_aquatic", 
                    "sand_gravel_total_unknown", "unk_invert_aquatic", "seed_unknown", "unknown_fish_aquatic")

### gut lavage fish data

fish_data_combined <- rbind(
  fish_data_20 %>%
    # create basin wide unit column from sampleID
    mutate(BasinWideUnit = floor(Sample_ID)) %>% 
    select(Sample_ID, BasinWideUnit, Creek, SpeciesCode, FieldSeason, LifeStage, ForkLength, FishWeight, FultonConditionFactor) %>%
    mutate(Creek = recode(Creek,
                          "RWD" = "Redwood Creek Mainstem",
                          "Fern" = "Fern Creek"),
           # correct case-sensitive first basin wide unit floor rounding from 0 to 1 (due to decimal values)
           BasinWideUnit = case_match(BasinWideUnit,
                                      0 ~ 1, 
                                      .default = BasinWideUnit)), 
  fish_data_22 %>%
    select(SampleID_New, BasinWideUnit, StreamID, SpeciesCode, FieldSeason, LifeStage, ForkLength, FishWeight, FultonConditionFactor) %>%
    rename(Sample_ID = SampleID_New,
           Creek = StreamID) %>%
    mutate(Creek = recode(Creek,
                          "34" = "Redwood Creek Mainstem",
                          "37" = "Fern Creek")))

### environmental data

environmental_data <- habitat_data %>%
  select(FieldSeason, StreamName, BasinWideUnit, Latitude, Longitude, HabitatType,
         Length_m, EstWidth_m, EstSurfaceArea_msq, MaxDepth_m, CrestDepth_m, ResidualPoolDepth_m) %>%
  rename(Creek = StreamName) %>%
  left_join(location_data %>% 
              mutate(FieldSeason = as.character(FieldSeason)) %>%
              select(FieldSeason, BasinWideUnit, SectionNum), by = c("FieldSeason", "BasinWideUnit"))

### combine everything

# create diet data frame with low count and trash taxa removed, including empty stomachs
diet_data_original <- fish_data_combined %>%
  left_join(environmental_data, by = c("FieldSeason", "Creek", "BasinWideUnit")) %>%
  left_join(gut_contents_combined %>%
              group_by(Sample_ID, Order) %>%
              summarize(Count =  n()) %>%
              pivot_wider(names_from = Order, values_from = Count) %>%
              # replace NAs with 0s
              mutate(across(everything(), ~ coalesce(., 0))) %>%
              select(-all_of(low_count_taxa_original), -all_of(non_food_taxa_original)),
            by = "Sample_ID") %>%
  mutate(across(all_of(20:36), ~ coalesce(.,0)))

# create diet data frame for terrestrial/aquatic categorizations with low count and trash taxa removed, including empty stomachs
diet_data_terrestrial_or_aquatic <- fish_data_combined %>%
  left_join(environmental_data, by = c("FieldSeason", "Creek", "BasinWideUnit")) %>%
  left_join(gut_contents_combined %>%
              group_by(Sample_ID, Taxa_Habitat) %>%
              summarize(Count = n()) %>%
              pivot_wider(names_from = Taxa_Habitat, values_from = Count) %>%
              # replace NAs with 0s
              mutate(across(everything(), ~ coalesce(., 0))) %>%
              select(-all_of(low_count_taxa_terrestrial_or_aquatic), -all_of(non_food_taxa_terrestrial_or_aquatic)),
            by = "Sample_ID") %>%
  mutate(across(all_of(20:46), ~ coalesce(.,0)))

### create taxa trait data frame

diet_taxa_original_traits <- gut_contents_combined %>%
  select(Order, Length_mm) %>%
  filter(Order %in% names(diet_data_original[,20:36])) %>%
  group_by(Order) %>%
  drop_na(Length_mm) %>%
  summarize(Avg_Length_mm = mean(as.numeric(Length_mm))) %>%
  column_to_rownames(var = "Order")

diet_taxa_terrestrial_or_aquatic_traits <- gut_contents_combined %>%
  select(Taxa_Habitat, Terrestrial_or_Aquatic, Length_mm) %>%
  filter(Taxa_Habitat %in% names(diet_data_terrestrial_or_aquatic[,20:46])) %>%
  group_by(Taxa_Habitat, Terrestrial_or_Aquatic) %>%
  drop_na(Length_mm) %>%
  summarize(Avg_Length_mm = mean(as.numeric(Length_mm))) %>%
  column_to_rownames(var = "Taxa_Habitat")

# Export data -------------------------------------------------------------

write.csv(diet_data_original, "Data/Created_Data/Diet_Data_Original.csv", row.names = FALSE)
write.csv(diet_data_terrestrial_or_aquatic, "Data/Created_Data/Diet_Data_Terrestrial_Or_Aquatic.csv", row.names = FALSE)
write.csv(diet_taxa_original_traits, "Data/Created_Data/Diet_Taxa_Original_Traits.csv", row.names = FALSE)
write.csv(diet_taxa_terrestrial_or_aquatic_traits, "Data/Created_Data/Diet_Taxa_Terrestrial_Or_Aquatic_Traits.csv", row.names = FALSE)
