
## 2024/2025
# Ruby Sainz Armendariz
# Aquatic Ecology Intern, GOGA

# Salmonid Diet Study in Redwood Creek, Golden Gate National Recreation Area, CA


## 2026-02-19
## Maggie and Ellington - please proof this code for any potential errors.


rm(list=ls()) #clears the environment


#Load in libraries
library(tidyverse)
library(readxl)
library(magrittr) 
library(hunspell) 
library(dplyr) 


#Load in data
#setwd("C:/Users/rsainzarmendariz/OneDrive - DOI/Documents/SalmonidDietAnalysis")
setwd("M:/Divisions/Natural Resources/Wildlife/Fish/Salmon/Redwood/Salmonid_diet/Data/RData")
FishData22 <- read_xlsx("RW_2022_FishData.xlsx") #2022 data (current 2022 Lavage Access db)
FishData20 <- read_xlsx("RW_2020_FishData.xlsx") #2020 data (current 2020 Lavage Access db)
GutContents22 <- read_xlsx("2022_GutContents.xlsx") #2022 data (current 2022 Lavage Access db, I added Order_terr_aqua column to excel file and adjusted "unk_plant" name)
GutContents20 <- read_xlsx("2020_GutContents_terr_aqua_corrected.xlsx") #2020 data (current 2020 Lavage Access db, I added Order_terr_aqua column to excel file and adjusted "unk_plant" name)
HabitatData <- read_xlsx("R.Sainz_Redwood and Fern Habitat Data_2020&2022_poolcomplexity.xlsx") #2020 and 2022
CanopyCover22 <- read_xlsx("Redwood_CanopyCover_DietSites_2022.xlsx") #There is only canopy cover data for 2022 (Redwood_CanopyCover_DietSites_2022)
LocationDataRWD <- read_xlsx("Redwood and Fern Habitat and Location Data_2020&2022.xlsx") #location/section number data (Redwood sheet)
LocationDataFern <- read_xlsx("Redwood and Fern Habitat and Location Data_2020&2022.xlsx", sheet = 2) #Fern sheet


###   Data Transformation ###

## Create 1 dataframe for the fish and invertebrate data

#Remove columns in FishData
FishData20.2 <- subset(FishData20, select = -c(Comments)) 
FishData22.2 <- subset(FishData22, select = -c(Comments)) 

#Fix YoY
FishData20.2 <- FishData20.2%>%
  mutate(LifeStage=if_else(LifeStage=="YOY","YoY", as.character(LifeStage)))

#Remove columns in GutContents
GutContents20.2 <- subset(GutContents20, select = -c(Notes))
GutContents22.2 <- subset(GutContents22, select = -c(Notes))

#Clean up column names
colnames(GutContents20.2)[2] <- "SampleID"
colnames(GutContents22.2)[3] <- "SampleID"
colnames(FishData20.2)[4] <- "SampleID"
colnames(FishData22.2)[13] <- "SampleID"

#Combine FishData with GutContents by SampleID
FishandInvert20 <- merge(GutContents20.2, FishData20.2, by = "SampleID")
#Checking why the number of rows does not add up to 1,011
length(unique(FishData20.2$SampleID)) 
length(unique(GutContents20.2$SampleID)) #there are not as many unique SampleID values 
#here compared to FishData20.2. That means there are fish that had no gut contents:
#(SampleID's 0.5,37.1-37.4, 389.5, 431.3, 431.7, 431.9,431.11, 560.6,593.6, 663.2, 
#678.2, 683.2, 788.3, 788.5)
#although the GutContents has 50.1, 81.13, 385.1-5, 393.2, 570.2, 789.3, 811.8
#These fish doesn't exist.

FishandInvert22 <- merge(GutContents22.2, FishData22.2, by = "SampleID")
#double check my work
length(unique(FishData22.2$SampleID)) 
length(unique(GutContents22.2$SampleID))
#Fish that don't exist: 268.92. 


#Add BasinWideUnit to 2020 data
FishandInvert20$BasinWideUnit = FishandInvert20$SampleID #Duplicate SampleID column and naming it BasinWideUnit
#Make each sample ID equal to a BW unit
FishandInvert20 <- FishandInvert20%>%
  mutate(BasinWideUnit=if_else(BasinWideUnit=="0.1","1", as.character(BasinWideUnit)))%>%
  mutate(BasinWideUnit=if_else(BasinWideUnit=="0.2","1", as.character(BasinWideUnit)))%>%
  mutate(BasinWideUnit=if_else(BasinWideUnit=="0.3","1", as.character(BasinWideUnit)))%>%
  mutate(BasinWideUnit=if_else(BasinWideUnit=="0.4","1", as.character(BasinWideUnit)))%>%
  mutate(BasinWideUnit=if_else(BasinWideUnit=="0.6","1", as.character(BasinWideUnit)))%>%
  mutate(BasinWideUnit=if_else(BasinWideUnit=="0.7","1", as.character(BasinWideUnit)))%>%
  mutate(BasinWideUnit=if_else(BasinWideUnit=="57.1","57", as.character(BasinWideUnit)))%>%
  mutate(BasinWideUnit=if_else(BasinWideUnit=="57.2","57", as.character(BasinWideUnit)))%>%
  mutate(BasinWideUnit=if_else(BasinWideUnit=="57.3","57", as.character(BasinWideUnit)))%>%
  mutate(BasinWideUnit=if_else(BasinWideUnit=="57.4","57", as.character(BasinWideUnit)))%>%
  mutate(BasinWideUnit=if_else(BasinWideUnit=="57.5","57", as.character(BasinWideUnit)))%>%
  mutate(BasinWideUnit=if_else(BasinWideUnit=="57.6","57", as.character(BasinWideUnit)))%>%
  mutate(BasinWideUnit=if_else(BasinWideUnit=="57.7","57", as.character(BasinWideUnit)))%>%
  mutate(BasinWideUnit=if_else(BasinWideUnit=="57.8","57", as.character(BasinWideUnit)))%>%
  mutate(BasinWideUnit=if_else(BasinWideUnit=="57.9","57", as.character(BasinWideUnit)))%>%
  mutate(BasinWideUnit=if_else(BasinWideUnit=="57.91","57", as.character(BasinWideUnit)))%>%
  mutate(BasinWideUnit=if_else(BasinWideUnit=="106.1","106", as.character(BasinWideUnit)))%>%
  mutate(BasinWideUnit=if_else(BasinWideUnit=="109.1","109", as.character(BasinWideUnit)))%>%
  mutate(BasinWideUnit=if_else(BasinWideUnit=="109.2","109", as.character(BasinWideUnit)))%>%
  mutate(BasinWideUnit=if_else(BasinWideUnit=="109.3","109", as.character(BasinWideUnit)))%>%
  mutate(BasinWideUnit=if_else(BasinWideUnit=="109.4","109", as.character(BasinWideUnit)))%>%
  mutate(BasinWideUnit=if_else(BasinWideUnit=="148.1","148", as.character(BasinWideUnit)))%>%
  mutate(BasinWideUnit=if_else(BasinWideUnit=="148.2","148", as.character(BasinWideUnit)))%>%
  mutate(BasinWideUnit=if_else(BasinWideUnit=="148.3","148", as.character(BasinWideUnit)))%>%
  mutate(BasinWideUnit=if_else(BasinWideUnit=="148.4","148", as.character(BasinWideUnit)))%>%
  mutate(BasinWideUnit=if_else(BasinWideUnit=="196.1","196", as.character(BasinWideUnit)))%>%
  mutate(BasinWideUnit=if_else(BasinWideUnit=="196.2","196", as.character(BasinWideUnit)))%>%
  mutate(BasinWideUnit=if_else(BasinWideUnit=="196.3","196", as.character(BasinWideUnit)))%>%
  mutate(BasinWideUnit=if_else(BasinWideUnit=="196.4","196", as.character(BasinWideUnit)))%>%
  mutate(BasinWideUnit=if_else(BasinWideUnit=="196.5","196", as.character(BasinWideUnit)))%>%
  mutate(BasinWideUnit=if_else(BasinWideUnit=="196.6","196", as.character(BasinWideUnit)))%>%
  mutate(BasinWideUnit=if_else(BasinWideUnit=="196.7","196", as.character(BasinWideUnit)))%>%
  mutate(BasinWideUnit=if_else(BasinWideUnit=="240.1","240", as.character(BasinWideUnit)))%>%
  mutate(BasinWideUnit=if_else(BasinWideUnit=="240.2","240", as.character(BasinWideUnit)))%>%
  mutate(BasinWideUnit=if_else(BasinWideUnit=="240.3","240", as.character(BasinWideUnit)))%>%
  mutate(BasinWideUnit=if_else(BasinWideUnit=="240.4","240", as.character(BasinWideUnit)))%>%
  mutate(BasinWideUnit=if_else(BasinWideUnit=="240.5","240", as.character(BasinWideUnit)))%>%
  mutate(BasinWideUnit=if_else(BasinWideUnit=="240.6","240", as.character(BasinWideUnit)))%>%
  mutate(BasinWideUnit=if_else(BasinWideUnit=="305.1","305", as.character(BasinWideUnit)))%>%
  mutate(BasinWideUnit=if_else(BasinWideUnit=="305.2","305", as.character(BasinWideUnit)))%>%
  mutate(BasinWideUnit=if_else(BasinWideUnit=="322.1","322", as.character(BasinWideUnit)))%>%
  mutate(BasinWideUnit=if_else(BasinWideUnit=="322.2","322", as.character(BasinWideUnit)))%>%
  mutate(BasinWideUnit=if_else(BasinWideUnit=="322.3","322", as.character(BasinWideUnit)))%>%
  mutate(BasinWideUnit=if_else(BasinWideUnit=="386.1","386", as.character(BasinWideUnit)))%>%
  mutate(BasinWideUnit=if_else(BasinWideUnit=="387.1","387", as.character(BasinWideUnit)))%>%
  mutate(BasinWideUnit=if_else(BasinWideUnit=="387.2","387", as.character(BasinWideUnit)))%>%
  mutate(BasinWideUnit=if_else(BasinWideUnit=="387.3","387", as.character(BasinWideUnit)))%>%
  mutate(BasinWideUnit=if_else(BasinWideUnit=="387.4","387", as.character(BasinWideUnit)))%>%
  mutate(BasinWideUnit=if_else(BasinWideUnit=="387.5","387", as.character(BasinWideUnit)))%>%
  mutate(BasinWideUnit=if_else(BasinWideUnit=="387.6","387", as.character(BasinWideUnit)))%>%
  mutate(BasinWideUnit=if_else(BasinWideUnit=="387.7","387", as.character(BasinWideUnit)))%>%
  mutate(BasinWideUnit=if_else(BasinWideUnit=="387.8","387", as.character(BasinWideUnit)))%>%
  mutate(BasinWideUnit=if_else(BasinWideUnit=="389.1","389", as.character(BasinWideUnit)))%>%
  mutate(BasinWideUnit=if_else(BasinWideUnit=="389.2","389", as.character(BasinWideUnit)))%>%
  mutate(BasinWideUnit=if_else(BasinWideUnit=="389.3","389", as.character(BasinWideUnit)))%>%
  mutate(BasinWideUnit=if_else(BasinWideUnit=="389.4","389", as.character(BasinWideUnit)))%>%
  mutate(BasinWideUnit=if_else(BasinWideUnit=="389.6","389", as.character(BasinWideUnit)))%>%
  mutate(BasinWideUnit=if_else(BasinWideUnit=="389.7","389", as.character(BasinWideUnit)))%>%
  mutate(BasinWideUnit=if_else(BasinWideUnit=="431.1","431", as.character(BasinWideUnit)))%>%
  mutate(BasinWideUnit=if_else(BasinWideUnit=="431.2","431", as.character(BasinWideUnit)))%>%
  mutate(BasinWideUnit=if_else(BasinWideUnit=="431.4","431", as.character(BasinWideUnit)))%>%
  mutate(BasinWideUnit=if_else(BasinWideUnit=="431.5","431", as.character(BasinWideUnit)))%>%
  mutate(BasinWideUnit=if_else(BasinWideUnit=="431.6","431", as.character(BasinWideUnit)))%>%
  mutate(BasinWideUnit=if_else(BasinWideUnit=="431.8","431", as.character(BasinWideUnit)))%>%
  mutate(BasinWideUnit=if_else(BasinWideUnit=="431.11","431", as.character(BasinWideUnit)))%>%
  mutate(BasinWideUnit=if_else(BasinWideUnit=="468.1","468", as.character(BasinWideUnit)))%>%
  mutate(BasinWideUnit=if_else(BasinWideUnit=="560.1","560", as.character(BasinWideUnit)))%>%
  mutate(BasinWideUnit=if_else(BasinWideUnit=="560.2","560", as.character(BasinWideUnit)))%>%
  mutate(BasinWideUnit=if_else(BasinWideUnit=="560.3","560", as.character(BasinWideUnit)))%>%
  mutate(BasinWideUnit=if_else(BasinWideUnit=="560.4","560", as.character(BasinWideUnit)))%>%
  mutate(BasinWideUnit=if_else(BasinWideUnit=="560.5","560", as.character(BasinWideUnit)))%>%
  mutate(BasinWideUnit=if_else(BasinWideUnit=="560.7","560", as.character(BasinWideUnit)))%>%
  mutate(BasinWideUnit=if_else(BasinWideUnit=="560.8","560", as.character(BasinWideUnit)))%>%
  mutate(BasinWideUnit=if_else(BasinWideUnit=="560.9","560", as.character(BasinWideUnit)))%>%
  mutate(BasinWideUnit=if_else(BasinWideUnit=="570.1","570", as.character(BasinWideUnit)))%>%
  mutate(BasinWideUnit=if_else(BasinWideUnit=="593.1","593", as.character(BasinWideUnit)))%>%
  mutate(BasinWideUnit=if_else(BasinWideUnit=="593.2","593", as.character(BasinWideUnit)))%>%
  mutate(BasinWideUnit=if_else(BasinWideUnit=="593.3","593", as.character(BasinWideUnit)))%>%
  mutate(BasinWideUnit=if_else(BasinWideUnit=="593.4","593", as.character(BasinWideUnit)))%>%
  mutate(BasinWideUnit=if_else(BasinWideUnit=="593.5","593", as.character(BasinWideUnit)))%>%
  mutate(BasinWideUnit=if_else(BasinWideUnit=="645.1","645", as.character(BasinWideUnit)))%>%
  mutate(BasinWideUnit=if_else(BasinWideUnit=="645.2","645", as.character(BasinWideUnit)))%>%
  mutate(BasinWideUnit=if_else(BasinWideUnit=="645.3","645", as.character(BasinWideUnit)))%>%
  mutate(BasinWideUnit=if_else(BasinWideUnit=="645.4","645", as.character(BasinWideUnit)))%>%
  mutate(BasinWideUnit=if_else(BasinWideUnit=="645.5","645", as.character(BasinWideUnit)))%>%
  mutate(BasinWideUnit=if_else(BasinWideUnit=="645.6","645", as.character(BasinWideUnit)))%>%
  mutate(BasinWideUnit=if_else(BasinWideUnit=="645.7","645", as.character(BasinWideUnit)))%>%
  mutate(BasinWideUnit=if_else(BasinWideUnit=="663.1","663", as.character(BasinWideUnit)))%>%
  mutate(BasinWideUnit=if_else(BasinWideUnit=="678.1","678", as.character(BasinWideUnit)))%>%
  mutate(BasinWideUnit=if_else(BasinWideUnit=="680.1","680", as.character(BasinWideUnit)))%>%
  mutate(BasinWideUnit=if_else(BasinWideUnit=="683.1","683", as.character(BasinWideUnit)))%>%
  mutate(BasinWideUnit=if_else(BasinWideUnit=="788.1","788", as.character(BasinWideUnit)))%>%
  mutate(BasinWideUnit=if_else(BasinWideUnit=="788.2","788", as.character(BasinWideUnit)))%>%
  mutate(BasinWideUnit=if_else(BasinWideUnit=="788.4","788", as.character(BasinWideUnit)))%>%
  mutate(BasinWideUnit=if_else(BasinWideUnit=="788.6","788", as.character(BasinWideUnit)))%>%
  mutate(BasinWideUnit=if_else(BasinWideUnit=="788.7","788", as.character(BasinWideUnit)))

#Double check work
length(unique(FishandInvert20$BasinWideUnit))

#Remove more columns in FishandInvert20 and 22 
FishandInvertMain20 <- subset(FishandInvert20, select = c(Creek,FieldSeason,BasinWideUnit,SampleID,SpeciesCode,LifeStage,ForkLength,FishWeight,FultonConditionFactor,Order,Lifestage,Terrestrial_or_Aquatic)) 
#In the 2022 dataframe, create Creek column so it matches with the 2020 data
FishandInvert22$Creek = FishandInvert22$StreamID
FishandInvert22 <-FishandInvert22%>%
  mutate(Creek = if_else(Creek=="34","RWD", as.character(Creek)))%>%
  mutate(Creek = if_else(Creek=="37","Fern", as.character(Creek)))
FishandInvertMain22 <- subset(FishandInvert22,select = c(Creek,FieldSeason,BasinWideUnit,SampleID,SpeciesCode,LifeStage,ForkLength,FishWeight,FultonConditionFactor,Order,Lifestage,Terrestrial_or_Aquatic)) 

#Combine 2020 and 2022 dataframes
FishandInvertCombined <- merge(FishandInvertMain20,FishandInvertMain22, all = TRUE)

#Categorize orders into "benthic" and "pelagic"
#categorize_orders <- function(FishandInvertCombined) {
#  FishandInvertCombined <- FishandInvertCombined %>%
#    mutate(InvertLocation = case_when(
#      # Category 1: Amphipoda, Hemiptera, Hymenoptera, Araneae, Psocodea, Diplopoda
#      Order %in% c("Amphipoda", "Hemiptera", "Hymenoptera", "Araneae", "Psocodea", "Diplopoda") ~ "pelagic",
#      
#      # Category 2: Terrestrial organisms
#      Terrestrial_or_Aquatic == "terrestrial" ~ "pelagic",
#      
#      # Category 3: Ephemeroptera, Plecoptera, Trichoptera, Lepidoptera, Megaloptera, Diptera, Neuroptera
#      # and Terrestrial_or_Aquatic is aquatic"
#     Order %in% c("Ephemeroptera", "Plecoptera", "Trichoptera", "Lepidoptera", "Megaloptera", "Diptera", "Neuroptera") & 
#        Terrestrial_or_Aquatic == "aquatic" ~ "benthic",
#      
#      # Category 4: Isopoda and terrestrial
#      Order == "Isopoda" & Terrestrial_or_Aquatic == "terrestrial" ~ "pelagic",
#      
#      # Category 5: Isopoda and aquatic
#      Order == "Isopoda" & Terrestrial_or_Aquatic == "aquatic" ~ "benthic",
#      
#      #Category 6: Bivalvia, Mollusca, Acari, Littorinimorpha, Gastropoda, Decapoda, Oligochaeta 
#      Order %in% c("Bivalvia", "Mollusca", "Acari", "Littorinimorpha", "Gastropoda_snail", "Decapoda", "Oligochaeta") ~ "benthic",
#      
#      # Category 7: Coleoptera and adults
#      Order == "Coleoptera" & Lifestage == "adult" ~ "pelagic",
#      
#      # Category 8: Coleoptera, larvae and aquatic
#      Order =="Coleoptera" & Terrestrial_or_Aquatic == "aquatic" & Lifestage == "larva" ~ "benthic",
#      
#      # Category 9: Coleoptera, larvae and terrestrial
#      Order =="Coleoptera" & Terrestrial_or_Aquatic == "terrestrial" & Lifestage == "larva" ~ "benthic",
#      
#      # Default: Set to NA (or another category if needed)
#      TRUE ~ NA_character_
#    ))%>%
#  #Create a new column that merges Order and InvertLocation
#  mutate(Order_InvertLocation = paste(Order, InvertLocation, sep = "_"))
#  
#  return(FishandInvertCombined)
#}
#
#FishandInvertCombined <- categorize_orders(FishandInvertCombined)



## Create 1 dataframe for environmental data

#Remove columns in HabitatData
HabitatData.1 <- subset(HabitatData,select = c(FieldSeason,StreamName,BasinWideUnit,HabitatType,HabitatTypeSecondary,ComplexPool,PoolComplexity,SideChannel,Length_m,EstWidth_m,NumberOfWidths,EstSurfaceArea_msq,MaxDepth_m,CrestDepth_m,ResidualPoolDepth_m,RootWad,LWDJam,SWDJam)) 

#Fix some of the columns
colnames(HabitatData.1)[2] <- "Creek" #Rename column so it matches with FishandInvertCombined data

#Fix names
HabitatData.1 <- HabitatData.1%>%
  mutate(Creek=if_else(Creek=="Redwood Creek Mainstem","RWD", as.character(Creek)))%>%
  mutate(Creek=if_else(Creek=="Fern Creek","Fern", as.character(Creek)))%>%
  mutate(HabitatType=if_else(HabitatType=="Scour Pool","ScourPool", as.character(HabitatType)))%>%
  mutate(HabitatType=if_else(HabitatType=="Backwater Pool","BackwaterPool", as.character(HabitatType)))%>%
  mutate(HabitatType=if_else(HabitatType=="Scour Pool","ScourPool", as.character(HabitatType)))%>%
  mutate(HabitatType=if_else(HabitatType=="Mid-Channel Pool","MidChannelPool", as.character(HabitatType)))%>%
  mutate(HabitatType=if_else(HabitatType=="Plunge Pool","PlungePool", as.character(HabitatType)))
  
#Remove columns in CanopyCover22
CanopyCover22.2 <- subset(CanopyCover22, select = c(UnitNumber,PercentCover)) 

#Adjust some of the BW units
CanopyCover22.2 <- CanopyCover22.2%>%
  mutate(UnitNumber=if_else(UnitNumber=="318","319", as.character(UnitNumber)))%>%
  mutate(UnitNumber=if_else(UnitNumber=="364","365", as.character(UnitNumber)))%>%
  mutate(UnitNumber=if_else(UnitNumber=="737","738", as.character(UnitNumber)))

colnames(CanopyCover22.2)[1] <- "BasinWideUnit" #rename column so it matches everything else

#Merge Fern and RWD location data
LocationData <- rbind(LocationDataFern, LocationDataRWD) 

colnames(LocationData)[2] <- "Creek"
LocationData <- LocationData %>%
  mutate(Creek=if_else(Creek=="34","RWD", as.character(Creek)))%>%
  mutate(Creek=if_else(Creek=="37","Fern", as.character(Creek)))

LocationData <- subset(LocationData, select = c(Creek,FieldSeason,BasinWideUnit,SectionNum)) 
colnames(LocationData)[4] <- "SectionNumber"
                                                                   
#Combine habitat, canopy cover and location data
HabitatandCanopyCover <- merge(HabitatData.1, CanopyCover22.2, all = T)
HabitatCanopyandLocation <- merge(LocationData,HabitatandCanopyCover, by = c("BasinWideUnit","FieldSeason","Creek"))
                                  


## Merge all data into 1 dataframe

#Change specific columns from numeric to factors
FishandInvertCombined <- FishandInvertCombined %>% mutate_at(vars(SampleID), as.factor)
HabitatCanopyandLocation <- HabitatCanopyandLocation %>% mutate_at(vars(BasinWideUnit,SectionNumber,FieldSeason,), as.factor)

#Add Count column to my data
Count <- data.frame(Count = rep(1,3028))
FishandInvertCombined_Counts <- cbind(FishandInvertCombined,Count)

#Pivot data so rows are fish and columns are invertebrate orders
FishandInvert_wide <- FishandInvertCombined_Counts %>%
  pivot_wider(
  names_from = Order, #names_from = Order_InvertLocation
  values_from = Count,
  values_fn = list(Count = sum), #sums the counts
  id_cols = c("Creek","FieldSeason","BasinWideUnit","SampleID","SpeciesCode","LifeStage","ForkLength","FishWeight","FultonConditionFactor")
)
FishandInvert_wide[is.na(FishandInvert_wide)] <- 0 #replaces NA's with zeros

DietData <- FishandInvert_wide %>%
  left_join(HabitatCanopyandLocation, by = c("BasinWideUnit", "FieldSeason", "Creek"))

# Replace NA values 
DietData$ComplexPool[is.na(DietData$ComplexPool)] <- "No"
DietData$HabitatTypeSecondary[is.na(DietData$HabitatTypeSecondary)] <- "NA"
DietData$SideChannel[is.na(DietData$SideChannel)] <- "No"
DietData <- DietData %>% mutate_at(vars(PoolComplexity), as.numeric)
DietData$PoolComplexity[is.na(DietData$PoolComplexity)] <- 0
DietData <- DietData %>% mutate_at(vars(PoolComplexity), as.factor)
DietData[is.na(DietData)] <- 0

#Remove sculpin and fish with NA life stage
DietData <- filter(DietData, SpeciesCode != "SCU")
DietData <- filter(DietData, LifeStage != "NA")

## Separate the covariates (fish and habitat data) and response variables (invert orders)
DietData_env <- subset(DietData, select = c(Creek,FieldSeason,BasinWideUnit,SectionNumber,SampleID,SpeciesCode,LifeStage,ForkLength,FishWeight,FultonConditionFactor,HabitatType,HabitatTypeSecondary,ComplexPool,PoolComplexity,SideChannel,Length_m,EstWidth_m,NumberOfWidths,EstSurfaceArea_msq,MaxDepth_m,CrestDepth_m,ResidualPoolDepth_m,RootWad,LWDJam,SWDJam,PercentCover))

DietData <- subset(DietData, select = -c(Creek,FieldSeason,BasinWideUnit,SectionNumber,SampleID,SpeciesCode,LifeStage,ForkLength,FishWeight,FultonConditionFactor,HabitatType,HabitatTypeSecondary,ComplexPool,PoolComplexity,SideChannel,Length_m,EstWidth_m,NumberOfWidths,EstSurfaceArea_msq,MaxDepth_m,CrestDepth_m,ResidualPoolDepth_m,RootWad,LWDJam,SWDJam,PercentCover))


#Export dataset to Excel
library(writexl)
write_xlsx(DietData, path = "DietData.xlsx")
write_xlsx(DietData_env, path = "DietData_env.xlsx")






##APPENDIX II DATA TABLE
#Table that Darren wants for the report. This is just for the report, not 
#the analysis
#Remove columns in FishandInvert20 and 22 to only show relevant fish/invert data
#AppendixIItable20<- subset(FishandInvert20,select = c(BasinWideUnit,SampleID,Order,Lifestage,Terrestrial_or_Aquatic,SpeciesCode,LifeStage,ForkLength,FishWeight)) 
#AppendixIItable22<- subset(FishandInvert22,select = c(BasinWideUnit,SampleID,Order,Lifestage,Terrestrial_or_Aquatic,SpeciesCode,LifeStage,ForkLength,FishWeight)) 
#tables are both way too long. Darren said to hold off on this for now
#Clean up column names and make table look more presentable
#colnames(AppendixIItable20)[2] <- "SampleID" #WHERE I LEFT OFF






### Data Analysis ###

DietData <- read_xlsx("DietData.xlsx") #Invertebrate orders
DietData_env <- read_xlsx("DietData_env.xlsx") #Fish and habitat


## Question 1: What is the diet overlap between the coho, steelhead and chinook salmon?
#A NMDS will be performed to find the best fit model, or the diet that overlaps the 
#species the best. Then a PERMANOVA will be run to test whether diets are statistically 
#different among fish species, and if so, how different.

#NMDS

library(readr)
library(tidyverse)  
library(vegan)
library(magrittr)
library(dplyr)
library(ggord)
library(tidyr)


library(readxl)
DietData <- read_excel("Data/DietData.xlsx")
DietData
DietData_env <- read_excel("Data/DietData_env.xlsx")
DietData_env


#Remove rare taxa from data (5 count and under) and "junk" orders (unknowns, detritus, seed, trash, NA's)
column_sums <- colSums(DietData)
print(column_sums)
hist(column_sums,
     breaks = pretty(column_sums, n = 50),
     xlab = "total",
     ylab = "Number of orders")
axis(1, at = seq(0,425, by = 25))

#DietData_condensed <- subset(DietData, select = -c(Decapoda_benthic,Odonata_pelagic,Bivalvia_benthic,trash_NA,Odonata_NA,Neuroptera_benthic,Megaloptera_pelagic,unknown_fish_NA,Orthoptera_NA,Lepidoptera_benthic,Ephemeroptera_pelagic,Acari_pelagic,seed_NA,Salmoniformes_NA,Collembola_NA,Oligochaeta_benthic,Plecoptera_pelagic,Annelida_NA,Siphonaptera_pelagic,Gastropoda_snail_pelagic,Slug_pelagic,Isopoda_pelagic,unk_invert_NA,Ephemeroptera_NA,Diptera_NA,Lepidoptera_NA,detritus_total_NA,Unknown_NA,unk_plant_material_NA,Trichoptera_NA,sand_gravel_total_NA,Isopoda_NA,Coleoptera_NA,Megaloptera_benthic))
DietData_condensed <- subset(DietData, select = -c(Decapoda,Oligochaeta,Collembola,detritus_total,Bivalvia,seed,Orthoptera,unk_plant_material,trash,Slug,unk_invert,Siphonaptera,Salmoniformes,Unknown,Odonata,Neuroptera,unknown_fish,Lepidoptera,sand_gravel_total,Megaloptera))

column_sums <- colSums(DietData_condensed)
print(column_sums)
#DietData_check <- subset(DietData, select = c(Decapoda_benthic,Odonata_pelagic,Bivalvia_benthic,trash_NA,Odonata_NA,Neuroptera_benthic,Megaloptera_pelagic,unknown_fish_NA,Orthoptera_NA,Lepidoptera_benthic,Ephemeroptera_pelagic,Acari_pelagic,seed_NA,Salmoniformes_NA,Collembola_NA,Oligochaeta_benthic,Plecoptera_pelagic,Annelida_NA,Siphonaptera_pelagic,Gastropoda_snail_pelagic,Slug_pelagic,Isopoda_pelagic,unk_invert_NA,Ephemeroptera_NA,Diptera_NA,Lepidoptera_NA,detritus_total_NA,Unknown_NA,unk_plant_material_NA,Trichoptera_NA,sand_gravel_total_NA,Isopoda_NA,Coleoptera_NA))
#column_sums_check <- colSums(DietData_check)
#print(column_sums_check)
hist(column_sums,
     breaks = pretty(column_sums, n = 30),
     xlab = "total",
     ylab = "Number of orders")
axis(1, at = seq(0,500, by = 50))

#Check for empty rows
DietData_condensed$ROWSUM <- rowSums(DietData_condensed)

t1 <- bind_cols(DietData_env, DietData_condensed)

t2 <- t1 %>% filter(ROWSUM != 0)

DietData_env <- t2[,c(1:26)]
DietData_condensed <- t2[,c(27:43)] #(removing rowsum!)

nms <- metaMDS(DietData_condensed, trymax = 25) 
nms
 

plot(nms)

#Plot by fish species
p.nmds <- ggord(nms, 
                grp_in = DietData_env$SpeciesCode, 
                pbslab = TRUE,
                arrow = NULL, 
                #size = 3,
                alpha_el = 0.2, 
                ptslab = TRUE,           # darkness of polygon
                poly = TRUE,
                labcol = "black",
                ellipse = TRUE,
                ellipse_pro = 0.80,      # confidence intervals for ellipse
                grp_title = "Fish Species",  
                repel = TRUE,            # make text not overlap
                txt = 4,                 # size of text
                #cols = c('purple', 'orange', 'blue', 'green', 'red', 'yellow'), # you want to select colors
                facet = FALSE, #make each grp_in on its own panel
                nfac = 6) +   # number of facet columns
  theme_gray(base_size = 14) # can add ggplot commands !

p.nmds + theme(legend.position = "right")
#save a copy of the plot
ggsave("p.NMS.jpg", width = 10, height = 8, units = "in", dpi = 300)

#Plot by age
p.nmds <- ggord(nms, 
                grp_in = DietData_env$LifeStage,
                pbslab = TRUE,
                arrow = NULL, 
                #size = 3,
                alpha_el = 0.2, 
                ptslab = TRUE,           # darkness of polygon
                poly = TRUE,
                labcol = "black",
                ellipse = TRUE,
                ellipse_pro = 0.80,      # confidence intervals for ellipse
                grp_title = "Fish Species",  
                repel = TRUE,            # make text not overlap
                txt = 4,                 # size of text
                #cols = c('purple', 'orange', 'blue', 'green', 'red', 'yellow'), # you want to select colors
                facet = FALSE, #make each grp_in on its own panel
                nfac = 6) +   # number of facet columns
  theme_gray(base_size = 14) # can add ggplot commands !

p.nmds + theme(legend.position = "right")


#Ben's code
library(gllvm)

#check data are ready
DietData_env
DietData_condensed

#reset graphics
par(mfrow = c(1, 1))

#neg binomial with covariates
fit_env.nb <- gllvm(DietData_condensed,  # [,c(23, 32, 11, 18, 28, 14)]
                    DietData_env, family = "negative.binomial", 
                    num.lv = 1,
                    formula = ~ SpeciesCode + LifeStage,
                    seed = 1234)
summary(fit_env.nb)
plot(fit_env.nb, mfrow=c(3,2))
coefplot(fit_env.nb, cex.ylab = 0.7, mar = c(4, 5, 2, 1), mfrow=c(1,5),
         order = FALSE )


#PERMANOVA
#Like an ANOVA for multivariate data
#one effect
m1.adonis <- adonis2(DietData_condensed ~ SampleID, data = DietData_env, permutations=1000) #99.9%
m1.adonis <- adonis2(DietData_condensed ~ FieldSeason, data = DietData_env, permutations=1000) #36.5%
m1.adonis <- adonis2(DietData_condensed ~ BasinWideUnit, data = DietData_env, permutations=1000) #27.7%
m1.adonis <- adonis2(DietData_condensed ~ SectionNumber, data = DietData_env, permutations=1000) #24.4%
m1.adonis <- adonis2(DietData_condensed ~ SpeciesCode, data = DietData_env, permutations=1000) #8%
m1.adonis <- adonis2(DietData_condensed ~ LifeStage, data = DietData_env, permutations=1000) #16%


## two effects
# Only looking at covariates with the highest R2 value
m2.adonis <- adonis2(DietData_condensed ~ FieldSeason * BasinWideUnit, data = DietData_env, permutations=1000)
m2.adonis <- adonis2(DietData_condensed ~ FieldSeason * SectionNumber, data = DietData_env, permutations=1000) 
m2.adonis <- adonis2(DietData_condensed ~ SpeciesCode * LifeStage, data = DietData_env, by = "terms", permutations=1000) 
m2.adonis <- adonis2(DietData_condensed ~ FieldSeason * SpeciesCode, data = DietData_env, permutations=1000) 


m1.adonis
m2.adonis


#For the results of the PERMANOVA, pay attention to the Rsquared and Pr(>F) columns. 
#The Rsquared is a percentage (0-100%). If the Rsquared value is really low, that means 
#that the env variable does not really play a role in the dependent variable. 
#The Residual is any variation that cannot be explained by the data.





#Question 2: Does this diet overlap vary spatially?
#Perform a PERMANOVA to see if its significance changes with different sites.

m3.adonis <- adonis2(DietData ~ BasinWideUnit * BasinWideUnit, data = DietData_env, permutations=1000) #

p.nmds <- ggord(nms, 
                grp_in = DietData_env$BasinWideUnit,
                pbslab = FALSE,
                arrow = NULL, 
                #size = 3,
                alpha_el = 0.2, 
                ptslab = TRUE,           # darkness of polygon
                poly = TRUE,
                labcol = "black",
                ellipse = TRUE,
                ellipse_pro = 0.80,      # confidence intervals for ellipse
                grp_title = "Fish Species",  
                repel = TRUE,            # make text not overlap
                txt = 2,                 # size of text
                #cols = c('purple', 'orange', 'blue', 'green', 'red', 'yellow'), # you want to select colors
                facet = TRUE, #make each grp_in on its own panel
                nfac = 6) +   # number of facet columns
  theme_gray(base_size = 14) # can add ggplot commands !

p.nmds + theme(legend.position = "none")



#Question 3: What fish species and life stage eat New Zealand mud snails?
#Line 101 in Ben's code
#plot by life stage and by species instead of year.

