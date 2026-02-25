library(tidyverse)

### Create combined df with both species and community shared invert data

# Pull Filtered_Redwood_BMI from 0.7
# Pull filtered_DietDataWhole from 0.2

#now makes df from only 2020 salmon diet data 
Invert_Comp_df <- filtered_DietDataWhole %>% 
  select(SpeciesCode, Acari, Amphipoda, Ephemeroptera, Plecoptera, Trichoptera, Coleoptera, Diptera) %>%
  pivot_longer(cols = 2:8, names_to = "Order", values_to = "Counts") %>%
  group_by(SpeciesCode, Order) %>%
  summarize(Totalcounts = sum(Counts)) %>%
  pivot_wider(names_from = SpeciesCode, values_from = Totalcounts) %>%
  left_join(Filtered_Redwood_BMI, by = "Order") %>%
  rename(Community = Totalcounts); Invert_Comp_df

# Convert counts to percentages
Invert_Comp_Percentage_df <- Invert_Comp_df %>% 
  pivot_longer(cols = 2:5, names_to = "Type", values_to = "Count") %>%
  group_by(Type) %>%
  summarize(Totalcounts = sum(Count)) %>%
  left_join(Invert_Comp_df %>% pivot_longer(cols = 2:5, names_to = "Type", values_to = "Count"), by = c("Type")) %>%
  mutate(Percentage = Count/Totalcounts) %>%
  select(Type, Order, Percentage) %>%
  pivot_wider(names_from = Type, values_from = Percentage); Invert_Comp_Percentage_df

# Chi-square test

Invert_Contingency_Table <- as.matrix(Invert_Comp_df[, c("CH", "CO", "SH", "Community")])
rownames(Invert_Contingency_Table) <- Invert_Comp_df$Order

chisq.test(Invert_Contingency_Table, simulate.p.value = TRUE)

chisq.test(x = Invert_Comp_df$CH, p = Invert_Comp_Percentage_df$Community, simulate.p.value = TRUE)
chisq.test(x = Invert_Comp_df$CO, p = Invert_Comp_Percentage_df$Community, simulate.p.value = TRUE)
chisq.test(x = Invert_Comp_df$SH, p = Invert_Comp_Percentage_df$Community, simulate.p.value = TRUE)


##Exact same analysis but for only 2020 diets (NO CHINOOK IN HERE)
DietData2020 <- filter(filtered_DietDataWhole, FieldSeason == "2020")

#now makes df from only 2020 salmon diet data 
Invert_Comp_df <- DietData2020 %>% 
  select(SpeciesCode, Acari, Amphipoda, Ephemeroptera, Plecoptera, Trichoptera, Coleoptera, Diptera) %>%
  pivot_longer(cols = 2:8, names_to = "Order", values_to = "Counts") %>%
  group_by(SpeciesCode, Order) %>%
  summarize(Totalcounts = sum(Counts)) %>%
  pivot_wider(names_from = SpeciesCode, values_from = Totalcounts) %>%
  left_join(Filtered_Redwood_BMI, by = "Order") %>%
  rename(Community = Totalcounts); Invert_Comp_df

# Convert counts to percentages
Invert_Comp_Percentage_df <- Invert_Comp_df %>% 
  pivot_longer(cols = 2:4, names_to = "Type", values_to = "Count") %>%
  group_by(Type) %>%
  summarize(Totalcounts = sum(Count)) %>%
  left_join(Invert_Comp_df %>% pivot_longer(cols = 2:4, names_to = "Type", values_to = "Count"), by = c("Type")) %>%
  mutate(Percentage = Count/Totalcounts) %>%
  select(Type, Order, Percentage) %>%
  pivot_wider(names_from = Type, values_from = Percentage); Invert_Comp_Percentage_df

# Chi-square test

Invert_Contingency_Table <- as.matrix(Invert_Comp_df[, c("CO", "SH", "Community")])
rownames(Invert_Contingency_Table) <- Invert_Comp_df$Order

chisq.test(Invert_Contingency_Table, simulate.p.value = TRUE)

chisq.test(x = Invert_Comp_df$CO, p = Invert_Comp_Percentage_df$Community, simulate.p.value = TRUE)
chisq.test(x = Invert_Comp_df$SH, p = Invert_Comp_Percentage_df$Community, simulate.p.value = TRUE)
