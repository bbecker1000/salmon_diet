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