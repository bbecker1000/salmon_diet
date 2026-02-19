library(tidyverse)
library(lme4)

Aquatic_Taxa <- c("Diptera", "Trichoptera", "Plecoptera", "Ephemeroptera", "Gastropoda_snail", "Amphipoda", "Isopoda", "Littorinimorpha")
Terrestrial_Taxa <- c("Coleoptera", "Hymenoptera", "Lepidoptera", "Hemiptera", "Araneae", "Acari", "Psocodea", "Megaloptera", "Diplopoda")

# Megaloptera are aquatic as juveniles and terrestrial as adults!
# Isopoda are found as both aquatic and terrestrial!
# Need actual citations to support this classification^

SpatialDiet <- filtAverageDiet %>% 
  mutate(Habitat = ifelse(PreyTaxa %in% Aquatic_Taxa,"Aquatic", "Terrestrial"))

ggplot(SpatialDiet, aes(x = SpeciesCode, y = MeanPercentage, fill = Habitat)) +
  geom_bar(stat = "identity")

# Requires filt data from 0.3.PERMANOVAandGraph
Average_Diet_Habitat <- filtDietDataCombLonger %>%
  mutate(Habitat = ifelse(PreyTaxa %in% Aquatic_Taxa,"Aquatic", "Terrestrial")) %>%
  group_by(SampleID,Habitat) %>%
  summarize(MeanPercentage = sum(Percentage))

Average_Diet_Habitat_Longer <- Average_Diet_Habitat %>%
  pivot_wider(names_from = "Habitat", values_from = MeanPercentage) %>%
  left_join(DietData_env, by = "SampleID")

PreyType_glm<- glm(cbind(Aquatic,Terrestrial) ~ SpeciesCode + 0,
                      Average_Diet_Habitat_Longer,
                      family = "binomial")

summary(PreyType_glm)