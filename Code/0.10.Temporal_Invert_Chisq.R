
## pull DietDataComb from 0.3,separate Diet Data into years , select only inverts to be able to compare to invert survery data 
DietData2020 <- filtered_DietDataWhole %>%
  filter(FieldSeason == "2020") %>%
  select(SpeciesCode, FieldSeason, Acari, Amphipoda, Ephemeroptera, Plecoptera, Trichoptera, Coleoptera, Diptera)
DietData2022 <- filtered_DietDataWhole %>%
  filter(FieldSeason == "2022")%>%
  select(SpeciesCode, FieldSeason, Acari, Amphipoda, Ephemeroptera, Plecoptera, Trichoptera, Coleoptera, Diptera)

#set up dfs for chi squared
#2020
Invert2020 <- DietData2020 %>% 
  pivot_longer(cols = 2:8, names_to = "Order", values_to = "Counts") %>%
  group_by(SpeciesCode, Order) %>%
  summarize(Totalcounts = sum(Counts)) %>%
  pivot_wider(names_from = SpeciesCode, values_from = Totalcounts) %>%
  left_join(Filtered_Redwood_BMI, by = "Order") %>%
  rename(Community = Totalcounts); Invert2020
#2022
Invert2022 <- DietData2022 %>% 
  pivot_longer(cols = 2:8, names_to = "Order", values_to = "Counts") %>%
  group_by(SpeciesCode, Order) %>%
  summarize(Totalcounts = sum(Counts)) %>%
  pivot_wider(names_from = SpeciesCode, values_from = Totalcounts) %>%
  left_join(Filtered_Redwood_BMI, by = "Order") %>%
  rename(Community = Totalcounts); Invert2022

#Set up for ch sq 
Invert2020_df <- as.matrix(Invert2020[, c("CO", "SH", "Community")])
rownames(Invert2020_df) <- Invert2020$Order
Invert2022_df <- as.matrix(Invert2022[, c("CO", "SH", "Community")])
rownames(Invert2022_df) <- Invert2022$Order

#run Chi-sq ------- p= 0.01599
chisq.test(x= Invert2020_df, p= Invert2022_df, simulate.p.value = TRUE)

#graphical comparision 
library(ggplot2)
##set up percenatge df for each year using year split dfs 
#2020
CountList2020 <- rowSums(DietData2020[3:9])
DietPercent2020 <- DietData2020[3:9]/CountList2020
DietPercent2020[is.na(DietPercent2020)]<- 0 
pivot_longer(DietPercent2020, cols= 1:7, names_to = "Order")
Diet2020 <- DietPercent2020 %>%
  group_by(Order) %>%
  summarise(total_value = sum(value))
#2022
CountList2022 <- rowSums(DietData2022[3:9])
DietPercent2022 <- DietData2022[3:9]/CountList2022
DietPercent2022[is.na(DietPercent2022)]<- 0 
#make data fram with percentages, fiels seasons, combined 
DietPercent2020$FieldSeason <- 2020
DietPercent2022$FieldSeason <- 2022
Temporal_invert_comp <- cbind(DietPercent2020, DietPercent2022)
#graph 
ggplot(Temporal_invert_comp, aes(x= FieldSeason, y= Order, fill= Order))+
  geom_col()+
  labs( title= "",
       x= "Field Season", 
       y= "Diet Composition")+
  theme_minimal()
