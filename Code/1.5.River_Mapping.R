library(paletteer)

### Habitat to river reach comparison

HabitatChar <- HabitatData_Clean %>%
  mutate(HabChar = case_when(
    SectionNum >= 0 & SectionNum <= 3.9 ~ "PacificWay",
    SectionNum >= 4 & SectionNum <= 17.9 ~ "Highway1", 
    SectionNum >= 18 & SectionNum <= 28.9 ~ "FrankValley",
    SectionNum >= 29 & SectionNum <= 49.9 ~ "KentCreekTrail",
    SectionNum >= 50 & SectionNum <= 68.9  ~ "Dipsea",
    SectionNum >= 69 & SectionNum <= 74 ~ "Foot4",
  ))

ggplot(HabitatChar %>% filter(Creek != "Fern Creek") %>% arrange(HabChar, SectionNum), aes(x = Longitude, y = Latitude)) +
  geom_jitter(aes(color = HabitatType),width = 0.001, height = 0.001, alpha = 0.6, size = 2) +
  geom_path(aes(group = HabChar, linetype = HabChar), linewidth = 2) +
  facet_wrap(~FieldSeason)
ggsave("Figures/River_Habitat.png", width = 6, height = 3.5, units = "in")

HabitatChar %>%
  filter(Creek != "Fern Creek", FieldSeason == "2022") %>%
  count(HabChar, HabitatType) %>%
  group_by(HabChar) %>%
  mutate(prop = n / sum(n)) %>%
  ggplot(aes(x = "", y = prop, fill = HabitatType)) +
  geom_col(width = 1) +
  facet_wrap(~HabChar) +
  coord_polar("y", start = 0)
ggsave("Figures/River_Habitat_Pie_2022.png", width = 6, height = 3.5, units = "in")

HabitatChar_props <- HabitatChar %>%
  filter(Creek != "Fern Creek") %>%
  count(HabChar, HabitatType, FieldSeason) %>%
  group_by(HabChar, FieldSeason) %>%
  mutate(prop = n / sum(n))

summary(glm(n ~ HabitatType * FieldSeason,
            HabitatChar_props,
            family = poisson))

# By area?

HabitatChar_Coverage <- HabitatChar %>% 
  filter(Creek != "Fern Creek") %>%
  group_by(FieldSeason, HabChar, HabitatType) %>%
  summarize(Area_Coverage = sum(EstSurfaceArea_msq, na.rm = TRUE))

HabitatChar_Coverage <- HabitatChar_Coverage %>%
  group_by(FieldSeason, HabChar) %>%
  summarize(Total_Coverage = sum(Area_Coverage)) %>%
  right_join(HabitatChar_Coverage, by = c("FieldSeason", "HabChar")) %>%
  mutate(prop = Area_Coverage/Total_Coverage)

ggplot(HabitatChar_Coverage, aes(x = "", y = prop, fill = HabitatType)) +
  geom_col(width = 1) +
  facet_wrap(~HabChar + FieldSeason) +
  coord_polar("y", start = 0)
ggsave("Figures/River_Area_Pie.png", width = 10, height = 10, units = "in")

# Coverage by dietdata

DietHabitatChar_2 <- DietData_env %>%
  mutate(HabChar = case_when(
    SectionNum >= 0 & SectionNum <= 3.9 ~ "PacificWay",
    SectionNum >= 4 & SectionNum <= 17.9 ~ "Highway1", 
    SectionNum >= 18 & SectionNum <= 28.9 ~ "FrankValley",
    SectionNum >= 29 & SectionNum <= 49.9 ~ "KentCreekTrail",
    SectionNum >= 50 & SectionNum <= 68.9  ~ "Dipsea",
    SectionNum >= 69 & SectionNum <= 74 ~ "Foot4",
  ))

ggplot(DietHabitatChar_2 %>% filter(Creek != "Fern Creek"), aes(x = Longitude, y = Latitude, color = HabChar, shape = HabChar)) +
  geom_point() +
  facet_wrap(~FieldSeason)
ggsave("Figures/Fish_River_Habitat_Map.png", width = 6, height = 3.5, units = "in")

DietHabitatChar_2 %>%
  filter(Creek != "Fern Creek") %>%
  count(HabChar, HabitatType, FieldSeason) %>%
  group_by(HabChar, FieldSeason) %>%
  mutate(prop = n / sum(n)) %>%
  ggplot(aes(x = "", y = prop, fill = HabitatType)) +
  geom_col(width = 1) +
  facet_wrap(~HabChar + FieldSeason) +
  coord_polar("y", start = 0)
ggsave("Figures/Fish_River_Habitat.png", width = 10, height = 10, units = "in")

### Diet by habitat

### Snorkel survey v. gut lavage samples 
#grab juvenile survey from 1.4 and HabitatData_clean
#2020
Juvenile_survey_2020_FCF <- Juvenile_survey %>% filter(FieldSeason== 2020) 
#graph 
ggplot(data= Juvenile_survey_2020, aes(x= Longitude, y= Latitude, color = SpeciesCode, size = NumberOfFish))+
  geom_jitter(width = 0.001, height = 0.001, alpha=0.5) +
  scale_alpha_continuous(range = c(0.4, 1.0)) +
  scale_color_manual(values = c(
    "SH" = "darkcyan",
    "CO" = "goldenrod"
  ))+
  geom_point(data= HabitatData_Clean, aes(x= Longitude, y= Latitude), 
             alpha= 0.5,  size= 2, shape= 23, color= "black")+
  theme_minimal()+
  labs(title = "Juvenile Fish Survey 2020", 
       x = "Longitude", 
       y = "Latitude", 
       color = "Species")

filtered_DietDataComb_HabChar <- DietDataComb_HabChar %>%
  filter(Creek != "Fern Creek", LifeStage == "YoY", FieldSeason == "2022")

filtered_DietDataComb_HabChar[,21:37] <- filtered_DietDataComb_HabChar[,21:37]/rowSums(filtered_DietDataComb_HabChar[,21:37])

filtered_DietDataComb_HabChar %>%
  group_by(HabChar) %>%
  summarize(Freq = n()) %>%
  ggplot(aes(x = HabChar, y = Freq)) +
  geom_col()

summarize_HabChar <- filtered_DietDataComb_HabChar %>%
  select(HabChar, 21:37) %>%
  pivot_longer(cols = 2:18, names_to = "Taxa", values_to = "Prop") %>%
  filter(Prop != "NaN") %>%
  group_by(HabChar,Taxa) %>%
  summarize(Prop = mean(Prop))

ggplot(summarize_HabChar, aes(x = "", y = Prop, fill = Taxa)) +
  geom_bar(stat = "Identity") +
  coord_polar("y", start = 0) + 
  facet_wrap(~HabChar)

view(cbind(summarize_HabChar %>% 
        filter(HabChar == "Dipsea") %>% 
        ungroup() %>%
        select(Taxa, Prop) %>% 
        arrange(desc(Prop)) %>% 
        rename(Dipsea = Prop),
      summarize_HabChar %>% 
        filter(HabChar == "FrankValley") %>% 
        ungroup() %>%
        select(Taxa, Prop) %>% 
        arrange(desc(Prop)) %>% 
        rename(FrankValley = Prop),
      summarize_HabChar %>% 
        filter(HabChar == "Highway1") %>% 
        ungroup() %>%
        select(Taxa, Prop) %>% 
        arrange(desc(Prop)) %>% 
        rename(DipHighway1sea = Prop),
      summarize_HabChar %>% 
        filter(HabChar == "KentCreekTrail") %>% 
        ungroup() %>%
        select(Taxa, Prop) %>% 
        arrange(desc(Prop)) %>% 
        rename(KentCreekTrail = Prop),
      summarize_HabChar %>% 
        filter(HabChar == "PacificWay") %>% 
        ungroup() %>%
        select(Taxa, Prop) %>% 
        arrange(desc(Prop)) %>% 
        rename(PacificWay = Prop)))

### Snorkel survey v. gut lavage samples 
#grab juvenile survey from 1.4 and HabitatData_clean
#2020
Juvenile_survey_2020_FCF <- Juvenile_survey %>% filter(FieldSeason== 2020) 
#graph 
ggplot(data= Juvenile_survey_2020, aes(x= Longitude, y= Latitude, color = SpeciesCode, size = NumberOfFish))+
  geom_jitter(width = 0.001, height = 0.001, alpha=0.5) +
  scale_alpha_continuous(range = c(0.4, 1.0)) +
  scale_color_manual(values = c(
    "SH" = "darkcyan",
    "CO" = "goldenrod"
  ))+
  geom_point(data= HabitatData_Clean, aes(x= Longitude, y= Latitude), 
             alpha= 0.5,  size= 2, shape= 23, color= "black")+
  theme_minimal()+
  labs(title = "Juvenile Fish Survey 2020", 
       x = "Longitude", 
       y = "Latitude", 
       color = "Species")

#2022
Juvenile_survey %>%
  filter(FieldSeason== 2022) %>%
  ggplot(aes(x= Longitude, y= Latitude, color = SpeciesCode, alpha = NumberOfFish))+
  geom_jitter(size = 1, width = 0.001, height = 0.001) +
  scale_alpha_continuous(range = c(0.3, 1.0)) +
  scale_color_manual(values = c(
    "SH" = "darkcyan",
    "CO" = "goldenrod",
    "CH" = "mediumvioletred"
  ))+
  geom_point(data= HabitatData_Clean, aes(x= Longitude, y= Latitude), 
             alpha= 0.5,  size= 2, shape= 23, color= "black")+
  theme_minimal()+
  labs(title = "Juvenile Fish Survey 2022", 
       x = "Longitude", 
       y = "Latitude", 
       color = "Species")