# last updated: 6/23/26
# Results:

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(readxl)
library(vegan)
library(ggrepel)

# Calling files -----------------------------------------------------------

diet_data_original <- read_csv("Data/Created_Data/Diet_Data_Original.csv")
diet_data_original_traits <- read_csv("Data/Created_Data/Diet_Taxa_Original_Traits.csv")

# Modifying data frame -----------------------------------------------------

# remove empty stomach  (zero rows)
diet_data_original_filtered <- diet_data_original[rowSums(diet_data_original[,21:37]) > 0,] %>% filter(LifeStage == "YoY") %>% na.omit()

# Plotting FCF for each salmon species between Field Season 

ggplot(data= )