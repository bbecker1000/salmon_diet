library(ggplot2)
library(tidyverse)
library(lme4)
library(readxl)
library(dplyr)

### Reformatting Data ###

# Download data
DietData <- read_excel("Data/DietData.xlsx")
DietData_env <- read_excel("Data/DietData_env.xlsx")

# Find total n count of prey items per observation (row) and create percentage data frame
TotalCountList <- rowSums(DietData)

DietDataPercent <- DietData/TotalCountList

# Remodel DietData and join with DietData_env

SampleID <- DietData_env$SampleID 
DietDataPercent <- cbind(DietDataPercent, SampleID)

DietData_env$SampleID <- as.character(DietData_env$SampleID) # Convert all sampleIDs from numeric to character data type
DietData_env$SampleID[103] <- "148.10" # Fix truncated sampleID (from 148.1 to 148.10)

# Combine environmental with diet datasets by sampleID
DietDataComb <- DietData_env %>%
  left_join(DietDataPercent, by = "SampleID")

# Convert dataframe into longer with individual rows for each prey observation per sampleID
DietDataCombLonger <- pivot_longer(DietDataComb, cols = 27:63, names_to = "PreyTaxa", values_to = "Percentage")

#Caluculate average diet composition by species 
AverageDiet <- DietDataCombLonger %>%
  group_by(SpeciesCode, PreyTaxa) %>%
  summarise(MeanPercentage = mean(Percentage, na.rm = TRUE),
            .groups = "drop")

##Graphing diet compotion per species ** SH should be split into two life stages, how to deal with so much unknown? 
library(ggplot2)
ggplot(AverageDiet, aes(x= SpeciesCode, y=MeanPercentage, fill= PreyTaxa))+
  geom_bar(stat = "identity")+
  labs(title = "",
       x = "Salmonid Species",
       y = "Percentage") +
  theme_minimal() 