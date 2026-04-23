library(readxl)
library(tidyverse)

### Making new diet data set

GutContents22 <- read_xlsx("Data/2022_GutContents.xlsx") #2022 data (current 2022 Lavage Access db, Ruby added Order_terr_aqua column to excel file and adjusted "unk_plant" name)
GutContents20 <- read_xlsx("Data/2020_GutContents_terr_aqua_corrected.xlsx") #2020 data (current 2020 Lavage Access db, Ruby added Order_terr_aqua column to excel file and adjusted "unk_plant" name)

# Select relevant columns

GutContent20_clean <- GutContents20 %>% 
  select(Sample_ID, Order, Family, Terrestrial_or_Aquatic, Lifestage, Length_mm, Width_mm)

GutContent22_clean <- GutContents22 %>% 
  select(SampleID_New, Order, Family, Terrestrial_or_Aquatic, Lifestage, Length_mm, Width_mm) %>%
  rename(Sample_ID = SampleID_New)

# Combined dataframes from different years
GutContents_Comb <- rbind(GutContent20_clean, GutContent22_clean)

# create lists of orders to remove due to either low count or non-food items
lowcounttaxa <- (GutContents_Comb %>%
                   group_by(Order) %>%
                   summarize(Count = n()) %>%
                   filter(Count < 10))$Order
nonfoodtaxa <- c("detritus_total", "unk_invert", "Unknown", "sand_gravel_total", "unk_plant_material")

# replace NA for habitat specification with unknown
GutContents_Comb[is.na(GutContents_Comb$Terrestrial_or_Aquatic),]$Terrestrial_or_Aquatic <- "unknown"

# Create new diet item name 
GutContents_Comb <- GutContents_Comb %>%
  mutate(Taxa_Habitat = paste0(Order, "_", Terrestrial_or_Aquatic))

DietData_ta <- GutContents_Comb %>%
  group_by(Sample_ID, Taxa_Habitat) %>%
  summarize(Count = n()) %>%
  pivot_wider(names_from = Taxa_Habitat, values_from = Count)

DietData_ta[is.na(DietData_ta)] <- 0 # replace NAs with 0s

### Grab fish data

FishData22 <- read_xlsx("Data/RW_2022_FishData.xlsx") #2022 data (current 2022 Lavage Access db)
FishData20 <- read_xlsx("Data/RW_2020_FishData.xlsx") #2020 data (current 2020 Lavage Access db)

FishData20_Clean <- FishData20 %>%
  mutate(BasinWideUnit = floor(Sample_ID)) %>% # create basinwideunit column from sampleID
  select(Sample_ID, BasinWideUnit, Creek, SpeciesCode, FieldSeason, LifeStage, ForkLength, FishWeight, FultonConditionFactor) %>%
  mutate(Creek = recode(Creek,
                        "RWD" = "Redwood Creek Mainstem",
                        "Fern" = "Fern Creek"))

FishData20_Clean$BasinWideUnit[FishData20_Clean$BasinWideUnit == 0] <- 1 # correct for just the first sampleid basinwide units

FishData22_Clean <- FishData22 %>%
  select(SampleID_New, BasinWideUnit, StreamID, SpeciesCode, FieldSeason, LifeStage, ForkLength, FishWeight, FultonConditionFactor) %>%
  rename(Sample_ID = SampleID_New,
         Creek = StreamID) %>%
  mutate(Creek = recode(Creek,
                        "34" = "Redwood Creek Mainstem",
                        "37" = "Fern Creek"))

FishData_Comb <- rbind(FishData20_Clean, FishData22_Clean)

### Grab environmental data

# Should not be using canopy cover data since it only comes from 2022
# CanopyCover22 <- read_xlsx("Data/Redwood_CanopyCover_DietSites_2022.xlsx") #There is only canopy cover data for 2022 (Redwood_CanopyCover_DietSites_2022)

# Location data is irrelevant since fish data lacks section number specificity which would give more specific habitat type
# LocationDataRWD <- read_xlsx("Data/Redwood and Fern Habitat and Location Data_2020&2022.xlsx") #location/section number data (Redwood sheet)
# LocationDataFern <- read_xlsx("Data/Redwood and Fern Habitat and Location Data_2020&2022.xlsx", sheet = 2) #Fern sheet

HabitatData <- read_xlsx("Data/R.Sainz_Redwood and Fern Habitat Data_2020&2022_poolcomplexity.xlsx") #2020 and 2022

HabitatData_Clean <- HabitatData %>%
  select(FieldSeason, StreamName, BasinWideUnit, Latitude, Longitude, HabitatType, 
         Length_m, EstWidth_m, EstSurfaceArea_msq, MaxDepth_m, CrestDepth_m, ResidualPoolDepth_m) %>%
  rename(Creek = StreamName)

### Merge environmental df
DietData_env <- FishData_Comb %>%
  left_join(HabitatData_Clean, by = c("FieldSeason", "Creek", "BasinWideUnit"))

### Merge DietData_env and DietData
DietDataComb_ta <- DietData_env %>% # Choose to join w/ dietdata_env so that only sampleIDs from diets are lost if no matching
  left_join(DietData_ta, by = "Sample_ID")

DietDataComb_ta[,19:79][is.na(DietDataComb_ta[,19:79])] <- 0

### Clean out 
lowcounttaxa_ta <- (GutContents_Comb %>%
  group_by(Taxa_Habitat) %>%
  summarize(count = n()) %>%
  filter(count < 10))$Taxa_Habitat
nonfoodtaxa_ta <- c("unk_invert_unknown", "Unknown_unknown", "unk_invert_terrestrial", "detritus_total_unknown", 
                    "trash_unknown", "unk_plant_material_unknown", "unk_plant_material_aquatic", 
                    "sand_gravel_total_unknown", "unk_invert_aquatic", "seed_unknown", "unknown_fish_aquatic")

DietDataComb_ta <- DietDataComb_ta %>%
  select(-all_of(lowcounttaxa_ta), -all_of(nonfoodtaxa_ta))

### Create taxa trait dataframe

truetaxa_ta <- names(DietDataComb_ta[,19:45])

DietTaxa_Traits <- GutContents_Comb %>%
  select(Taxa_Habitat, Terrestrial_or_Aquatic, Length_mm) %>%
  filter(Taxa_Habitat %in% truetaxa_ta) %>%
  group_by(Taxa_Habitat, Terrestrial_or_Aquatic) %>%
  drop_na(Length_mm) %>%
  summarize(Avg_Length_mm = mean(as.numeric(Length_mm)))
