# exploratory data analysis of original diet data
# notes:
  # morphometrics - CH fulton condition factor < CO, SH
  # environment - not too much difference in habitat type based on species or life stage
  # nmds - unsuccessful at parsing differences

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(readxl)
library(vegan)
library(ggrepel)

# Calling files -----------------------------------------------------------

diet_data_original <- read.csv("Data/Created_Data/Diet_Data_Original.csv")

# Raw plotting ----------------------------------------------------------------

### morphometrics comparisons

# forklength comparison
ggplot(diet_data_original %>%
         filter(LifeStage %in% c("YoY")),
       aes(x = SpeciesCode, y = ForkLength, fill = SpeciesCode)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.6, color = "black")

# fish weight comparison
ggplot(diet_data_original %>%
         filter(LifeStage %in% c("YoY")),
       aes(x= SpeciesCode, y= FishWeight, fill= SpeciesCode))+
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.6, color = "black")

# fulton condition factor comparison
ggplot(diet_data_original %>%
         filter(LifeStage %in% c("YoY")),
       aes(x= SpeciesCode, y= FultonConditionFactor, fill= SpeciesCode))+
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.6, color = "black")

# life stage comparison
ggplot(diet_data_original %>%
         filter(LifeStage != "NA") %>%
         group_by(SpeciesCode, LifeStage) %>%
         summarize(Count = n()), 
       aes(x = SpeciesCode, y = Count, fill = LifeStage))+
  geom_col(position = "dodge", width = 0.7)

## within SH species comparison

# SH life stage comparison
ggplot(diet_data_original %>%
         filter(SpeciesCode == "SH"), 
       aes(x = LifeStage, y = ForkLength, fill = LifeStage))+
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.6, color = "black")

# SH fish weight comparison
ggplot(diet_data_original %>%
         filter(SpeciesCode == "SH"), 
       aes(x = LifeStage, y = FishWeight, fill = LifeStage))+
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.6, color = "black")

# SH fulton condition factor comparison
ggplot(diet_data_original %>%
         filter(SpeciesCode == "SH"), 
       aes(x = LifeStage, y = FultonConditionFactor, fill = LifeStage))+
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.6, color = "black")

### environmental characteristics

# habitat type comparison 
ggplot(diet_data_original %>%
         filter(LifeStage %in% c("YoY")) %>%
         group_by(SpeciesCode, HabitatType) %>%
         summarize(Count = n()), 
       aes(x = SpeciesCode, y = Count, fill = HabitatType))+
  geom_col(position = "dodge", width = 0.7)

# SH habitat type comparison

ggplot(diet_data_original %>%
         filter(SpeciesCode == "SH") %>%
         group_by(LifeStage, HabitatType) %>%
         summarize(Count = n()), 
       aes(x = LifeStage, y = Count, fill = HabitatType)) + 
  geom_col(position = "dodge", width = 0.7)

# nmds --------------------------------------------------------------------

set.seed(451)
NMDS2 <- metaMDS(as.matrix(diet_data_original[,20:36] %>%
                             filter(rowSums(.) != 0)), 
                 distance= "bray", k=2, try=20)
stressplot(NMDS2)
NMDS_point <- as_tibble(NMDS2$points)
NMDS_full <- cbind(diet_data_original[,1:19], NMDS_point)
Prey_points <- NMDS2$species %>% as.data.frame() %>%
  rownames_to_column(var = 'Prey')
#makes the plot 
ggplot() +
  geom_jitter(data = NMDS_full, 
              aes(x= MDS1, y= MDS2, color= SpeciesCode, fill= SpeciesCode),
              size= 3, width = 0.3, height = 0.3)+
  stat_ellipse(data = NMDS_full, 
               aes(x= MDS1, y= MDS2, color= SpeciesCode, fill= SpeciesCode),
               geom= "polygon",
               level= 0.8, alpha= 0.3)+
  geom_text_repel(data = Prey_points,
                  aes(x= MDS1, y= MDS2, label= Prey), 
                  color ="black", family= "Helvetica")+
  labs(title = "")+
  theme_bw()
