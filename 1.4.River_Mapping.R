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
  geom_path(aes(group = HabChar, linetype = HabChar), linewidth = 2)
ggsave("Figures/River_Habitat.png", width = 6, height = 3.5, units = "in")


HabitatChar %>%
  filter(Creek != "Fern Creek") %>%
  count(HabChar, HabitatType) %>%
  group_by(HabChar) %>%
  mutate(prop = n / sum(n)) %>%
  ggplot(aes(x = "", y = prop, fill = HabitatType)) +
  geom_col(width = 1) +
  facet_wrap(~HabChar) +
  coord_polar("y", start = 0)
ggsave("Figures/River_Habitat_Pie.png", width = 6, height = 3.5, units = "in")


view(DietHabitatChar)

DietHabitatChar_2 <- DietData_env %>%
  mutate(HabChar = case_when(
    SectionNum >= 0 & SectionNum <= 3.9 ~ "PacificWay",
    SectionNum >= 4 & SectionNum <= 17.9 ~ "Highway1", 
    SectionNum >= 18 & SectionNum <= 28.9 ~ "FrankValley",
    SectionNum >= 29 & SectionNum <= 49.9 ~ "KentCreekTrail",
    SectionNum >= 50 & SectionNum <= 68.9  ~ "Dipsea",
    SectionNum >= 69 & SectionNum <= 74 ~ "Foot4",
  ))

ggplot(DietHabitatChar_2 %>% filter(Creek != "Fern Creek"), aes(x = Longitude, y = Latitude, color = SectionNum, shape = HabChar)) +
  geom_point()

