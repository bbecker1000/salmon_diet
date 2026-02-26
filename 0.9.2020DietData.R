GC_original_20 <- read_excel("Data/2020_GutContents.xlsx")
GC_new_20 <- read_excel("Data/2020_GutContents_terr_aqua_corrected.xlsx")
FD_20 <- read_excel("Data/RW_2020_FishData.xlsx")


length(intersect(FD_20$Sample_ID, GC_original_20$Sample_ID))
length(intersect(FD_20$Sample_ID, GC_new_20$Sample_ID))

DietData_20 <- GC_new_20 %>%
  group_by(Sample_ID, Order) %>%
  summarize(Count = n()) %>%
  pivot_wider(names_from = "Order", values_from = "Count")

DietData_20[is.na(DietData_20)] <- 0

DietDataComb_20 <- FD_20 %>%
  inner_join(DietData_20, by = "Sample_ID")

### EDA

ggplot(DietDataComb_20, aes(x = SpeciesCode)) +
  geom_bar()

ggplot(DietDataComb_20, aes(x = LifeStage, fill = SpeciesCode)) +
  geom_bar()

ggplot(DietDataComb_20, aes(x = ForkLength)) +
  geom_density()

SH_Lifestage_Counts <- DietDataComb_20 %>%
  pivot_longer(cols = 15:36, names_to = "Taxa", values_to = "Count") %>%
  filter(SpeciesCode == "SH") %>%
  group_by(LifeStage) %>%
  summarize(Totalcounts = sum(Count))

DietDataPercent_20 <- DietDataComb_20 %>%
  pivot_longer(cols = 15:36, names_to = "Taxa", values_to = "Count") %>%
  filter(SpeciesCode == "SH") %>%
  left_join(SH_Lifestage_Counts, by = "LifeStage") %>%
  mutate(Percentage = Count/Totalcounts)

ggplot(DietDataPercent_20, aes(x = LifeStage, y = Percentage, fill = Taxa)) +
  geom_bar(stat = "identity")

### Separated data by SH and Lifestage

DietDataComb_20_longer <- DietDataComb_20 %>%
  pivot_longer(cols = 15:36, names_to = "Taxa", values_to = "Count") %>%
  filter(SpeciesCode == "SH")

DietDataComb_20_longer %>%
  group_by(Taxa) %>%
  summarize(sum(Count))

DietDataCombFiltered_20 <- DietDataComb_20 %>%
  filter(SpeciesCode == "SH") %>%
  select(-c(Araneae, Bivalvia, Decapoda, Odonata, Oligochaeta, Unknown, detritus_total, trash, unk_invert, unk_plant_material), # Left out in other model
         -c(Hemiptera, Hymenoptera, Isopoda, Lepidoptera, Littorinimorpha, Megaloptera, Plecoptera)) %>% # these were kept in the 2022 diets and made up to 5% of diet data
  mutate(Year = "2020")
