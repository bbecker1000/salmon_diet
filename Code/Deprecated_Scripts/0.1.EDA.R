library(ggplot2)

#importing datasets 
library(readxl)
DietData <- read_excel("Data/DietData.xlsx")
View(DietData)
DietData_env <- read_excel("Data/DietData_env.xlsx")
View(DietData_env)

#EDA 
#Species X Body characteristics Box plots 
library(ggplot2)
#fork length = SH have significantly longer and more variable Fork legths 
ggplot(DietData_env, aes(x= SpeciesCode, y= ForkLength, fill= SpeciesCode))+
  geom_boxplot(outlier.shape = 16, outlier.alpha = 0.5) +
  geom_jitter(width = 0.2, alpha = 0.6, color = "black") +
    theme_minimal()+
    labs(title= "", 
         x= "Species",
         y="Fork Length"
    )
# Fish weight = SH are bigger with more variability 
ggplot(DietData_env, aes(x= SpeciesCode, y= FishWeight, fill= SpeciesCode))+
  geom_boxplot(outlier.shape = 16, outlier.alpha = 0.5) +
  geom_jitter(width = 0.2, alpha = 0.6, color = "black") +
  scale_y_log10() +
  theme_minimal()+
  labs(title= "", 
       x= "Species",
       y="Fish Weight"
  )
#Fulton Condition Factor = CH less , similar distributions 
ggplot(DietData_env, aes(x= SpeciesCode, y= FultonConditionFactor, fill= SpeciesCode))+
  geom_boxplot(outlier.shape = 16, outlier.alpha = 0.5) +
  geom_jitter(width = 0.2, alpha = 0.6, color = "black") +
  scale_y_log10() +
  theme_minimal()+
  labs(title= "", 
       x= "Species",
       y="Fulton Condition Factor"
  )

#Species x Life Stage = only SH Show 1+ life stage.. girl they are dominating ** how to get amount in each categroy into these? 
ggplot(DietData_env, aes(x= SpeciesCode, y= LifeStage, fill= LifeStage))+
  geom_col(position = "dodge", width = 0.7) +
  labs(title = "", 
       x= "Species",
       y= "Life Stage",
       fill= "life Stage" )+
  theme_minimal()

#Species x Habitat type= SH in Plunge Pool and Riffle as well, where others are in same 3 ** how to get amount in each categroy into these? 
ggplot(DietData_env, aes(x= SpeciesCode, y= HabitatType, fill= HabitatType))+
  geom_col(position = "dodge", width = 0.7) +
  labs(title = "", 
       x= "Species",
       y= "Habitat Type",
       fill= "Habitat Type" )+
  theme_minimal()

#EDA within each species 
# life stage x fork legth = older are bigger 
glayer1 <- subset(DietData_env, SpeciesCode == "SH")
ggplot(data= glayer1, aes(x= LifeStage, y= ForkLength, fill= LifeStage))+
  geom_boxplot(outlier.shape = 16, outlier.alpha = 0.5) +
  geom_jitter(width = 0.2, alpha = 0.6, color = "black") +
  theme_minimal()+
  labs(title= "", 
       x= "LifeStage",
       y="Fork Length"
  )
#life stage x weight  
glayer1 <- subset(DietData_env, SpeciesCode == "SH")
ggplot(data= glayer1, aes(x= LifeStage, y= FishWeight, fill= LifeStage))+
  geom_boxplot(outlier.shape = 16, outlier.alpha = 0.5) +
  geom_jitter(width = 0.2, alpha = 0.6, color = "black") +
  theme_minimal()+
  labs(title= "", 
       x= "LifeStage",
       y="Weight"
  )
#life stage x fulton condition factor === SH fishes slim out with maturity 
glayer1 <- subset(DietData_env, SpeciesCode == "SH")
ggplot(data= glayer1, aes(x= LifeStage, y= FultonConditionFactor, fill= LifeStage))+
  geom_boxplot(outlier.shape = 16, outlier.alpha = 0.5) +
  geom_jitter(width = 0.2, alpha = 0.6, color = "black") +
  theme_minimal()+
  labs(title= "", 
       x= "LifeStage",
       y="FCF"
  )
#life stage x habitat type = all stages in all habitats for SH
glayer1 <- subset(DietData_env, SpeciesCode == "SH")
ggplot(data= glayer1, aes(x= LifeStage, y= HabitatType, fill = HabitatType)) + 
  geom_col(position = "dodge", width = 0.7) +
  labs(title = "", 
       x= "Species",
       y= "Life Stage",
       fill= "life Stage" )+
  theme_minimal()
#life stage x habitat type for CO 


#Species diet comparisons.. getting tricky 
#LETS GOOOO NMDS ** not quite there... is it that there is not enough defintition between diets? 
library(vegan)
library(readxl)
library(tidyverse)
library(ggrepel)
set.seed(451)
DietData_m <-as.matrix(DietData)
View(DietData_m)
NMDS2 <- metaMDS(DietData_m, distance= "bray", k=2, try=20)
stressplot(NMDS2)
NMDS_point <- as_tibble(NMDS2$points)
NMDS_full <- cbind(DietData_env, NMDS_point)
Prey_points <- NMDS2$species %>% as.data.frame() %>%
  rownames_to_column(var = 'Prey')
View(Prey_points)
#makes the plot 
p.NMS_plot <- ggplot()+
  geom_jitter(data= NMDS_full, aes(x= MDS1, y= MDS2,
                                   color= SpeciesCode, 
                                   fill= SpeciesCode),
              size= 3, width = 0.3, height = 0.3)+
  stat_ellipse(data = NMDS_full, aes(x= MDS1, y= MDS2,
                                     color= SpeciesCode,
                                     fill= SpeciesCode),
               geom= "polygon",
               level= 0.8, alpha= 0.3)+
  geom_text_repel(data= Prey_points, aes(x= MDS1, y= MDS2,
                                     label= Prey), color ="black", family= "Helvetica")+
  labs(title = "")+
  theme_bw()
p.NMS_plot

#------------------------------------------------------------------------------------------
# Diet Compositions between years and species

# Download data
DietData <- read_excel("Data/DietData.xlsx")
DietData_env <- read_excel("Data/DietData_env.xlsx")

# Filter out unwanted stomach materials
DietData_filtered <- DietData %>%
  select(Decapoda, Ephemeroptera, Diptera, Plecoptera, Psocodea, Hymenoptera, Trichoptera, Araneae, Coleoptera,
         Gastropoda_snail, Isopoda, Megaloptera, Littorinimorpha, Hemiptera, Lepidoptera, Odonata, Oligochaeta, Bivalvia)

# Find total count of prey items per observation and create percentage data frame
TotalCountList <- rowSums(DietData_filtered)
DietDataPercent <- DietData_filtered / TotalCountList

# Remodel DietData and join with DietData_env
SampleID <- DietData_env$SampleID
DietDataPercent <- cbind(DietDataPercent, SampleID)

DietData_env$SampleID <- as.character(DietData_env$SampleID)
DietData_env$SampleID[103] <- "148.10"

# Combine environmental with diet datasets by sampleID
DietDataComb <- DietData_env %>%
  left_join(DietDataPercent, by = "SampleID")

# Convert dataframe into longer with individual rows for each prey observation per sampleID
DietDataCombLonger <- pivot_longer(DietDataComb, cols = 27:44, names_to = "PreyTaxa", values_to = "Percentage")

# Make separate data set for each year
DietDataCombLonger_2020 <- DietDataCombLonger %>% filter(FieldSeason == 2020)
DietDataCombLonger_2022 <- DietDataCombLonger %>% filter(FieldSeason == 2022)

# Calculate average diet composition by species for each year
AverageDiet_2020 <- DietDataCombLonger_2020 %>%
  group_by(SpeciesCode, PreyTaxa) %>%
  summarise(MeanPercentage = mean(Percentage, na.rm = TRUE),
            .groups = "drop")

AverageDiet_2022 <- DietDataCombLonger_2022 %>%
  group_by(SpeciesCode, PreyTaxa) %>%
  summarise(MeanPercentage = mean(Percentage, na.rm = TRUE),
            .groups = "drop")

# Graph 2020
ggplot(AverageDiet_2020, aes(x = SpeciesCode, y = MeanPercentage, fill = PreyTaxa)) +
  geom_bar(stat = "identity") +
  labs(title = "Percent Diet Compositions 2020",
       x = "Salmonid Species",
       y = "Percentage") +
  theme_minimal()

# Graph 2022
ggplot(AverageDiet_2022, aes(x = SpeciesCode, y = MeanPercentage, fill = PreyTaxa)) +
  geom_bar(stat = "identity") +
  labs(title = "Percent Diet Compositions 2022",
       x = "Salmonid Species",
       y = "Percentage") +
  theme_minimal()
