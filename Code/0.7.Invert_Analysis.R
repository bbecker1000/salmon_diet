library(readxl)
library(tidyverse)
library(ggplot2)


# Replace bad column names
Col_Names <- c("Class","Order","S1","S2","S3","S4","S5","S6","S7","S8","S9")
Redwood_BMI_2020 <- Redwood_BMI_2020[4:14,]
names(Redwood_BMI_2020) <- Col_Names

# Replace NAs with 0s in col 3:11 and convert to numeric
Redwood_BMI_2020[,3:11][is.na(Redwood_BMI_2020[,3:11])] <- "0"
Redwood_BMI_2020[,3:11] <- lapply(Redwood_BMI_2020[,3:11],as.numeric)

# Make date-sample ID df for merging
Sample_IDs <- c("S1","S2","S3","S4","S5","S6","S7","S8","S9")
Dates <- c("16/7/2020",	"16/7/2020", "16/7/2020", "16/7/2020",	"16/7/2020",	"16/7/2020",	"17/7/2020",	"17/7/2020",	"17/7/2020")
Dates <- as.POSIXct(Dates, format = "%d/%m/%Y", timezone = "PDT")
Sample_Dates_df <- data.frame(Sample_ID = as.character(Sample_IDs),
                              Date = Dates)

# Make Redwood_BMI_2020 df longer and merge with date-sample df
Redwood_BMI_2020_Longer <- Redwood_BMI_2020 %>%
  pivot_longer(cols = 3:11, names_to = "Sample_ID", values_to = "Count") %>%
  left_join(Sample_Dates_df, by = "Sample_ID") %>%
  mutate(Class_Order = paste0(Class, "_", Order))

# Redwood_BMI_2020 Percentage Conversions
Sampletotals <- Redwood_BMI_2020_Longer %>% 
  group_by(Sample_ID) %>%
  summarize(Totalcounts = sum(Count))

Redwood_BMI_2020_Longer <- Redwood_BMI_2020_Longer %>%
  left_join(Sampletotals, by = "Sample_ID") %>%
  mutate(Percentage = Count/Totalcounts)
 
# Sanity plots
ggplot(Redwood_BMI_2020_Longer, aes(x = Sample_ID, y = Percentage, fill = Class_Order)) +
  geom_bar(stat = "identity")

ggsave("Figures/Redwood_BMI_Averages.png")

### Create df averaged across samples

Taxatotals <- data.frame(Totalcounts = rowSums(Redwood_BMI_2020[,3:11]))
Filtered_Redwood_BMI <- Redwood_BMI_2020 %>%
  select(Order) %>%
  cbind(Taxatotals) %>%
  filter(Order %in% c("Subclass: Acari", "Amphipoda", "Ephemeroptera", "Plecoptera", "Trichoptera", "Coleoptera", "Diptera"))
Filtered_Redwood_BMI$Order[Filtered_Redwood_BMI$Order == "Subclass: Acari"] <- "Acari"
