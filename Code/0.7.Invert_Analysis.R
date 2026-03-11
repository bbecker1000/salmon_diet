library(readxl)
library(tidyverse)
library(ggplot2)

### Download and clean up 2019 - 2021 BMI data
# Note: all surveys take place in July

BMI_2019 <- read_excel("Data/GGNRA_Redwood_Ck 2019_BMI.xls", sheet = 4)[3:11,]
BMI_2019 <- BMI_2019 %>%
  rename(Class = ...1, Order = ...2) %>%
  mutate(Year = "2019") %>%
  select(Year, Class, Order, "Sample 7", "Sample 8", "Sample 9") # Only samples 7 - 9 are unaltered by extrinsic reconstruction activity
for (taxa in 1:nrow(BMI_2019)) {
  if(is.na(BMI_2019[taxa,3]) == TRUE){
    BMI_2019[taxa,3] <- BMI_2019[taxa,2]
  }
}
BMI_2019$Order[BMI_2019$Order == "Subclass: Acari"] <- "Acari"
BMI_2019[,4:6][is.na(BMI_2019[,4:6])] <- 0 # replace NAs with 0s

BMI_2020 <- read_excel("Data/GGNRA_Redwood_Creek_July2020_BMI.xls", sheet = 3)[3:13,]
BMI_2020 <- BMI_2020 %>%
  rename(Class = ...1, Order = ...2) %>%
  mutate(Year = "2020") %>%
  select(Year, Class, Order, "Sample 7", "Sample 8", "Sample 9") # Only samples 7 - 9 are unaltered by extrinsic reconstruction activity
# Replace missing order specificity
for (taxa in 1:nrow(BMI_2020)) {
  if(is.na(BMI_2020[taxa,3]) == TRUE){
    BMI_2020[taxa,3] <- BMI_2020[taxa,2]
  }
}
BMI_2020$Order[BMI_2020$Order == "Subclass: Acari"] <- "Acari"
BMI_2020[is.na(BMI_2020)] <- 0 # replace NAs with 0s

BMI_2021 <- read_excel("Data/Redwood_Ck_2021 BMI_level2_CSCI.xlsx", sheet = 5)[3:17,]
BMI_2021 <- BMI_2021 %>%
  rename(Class = ...1, Order = ...2) %>%
  mutate(Year = "2021") %>%
  select(Year, Class, Order, "Sample 7", "Sample 8", "Sample 9") # Only samples 7 - 9 are unaltered by extrinsic reconstruction activity
# Replace missing order specificity
for (taxa in 1:nrow(BMI_2021)) {
  if(is.na(BMI_2021[taxa,3]) == TRUE){
    BMI_2021[taxa,3] <- BMI_2021[taxa,2]
  }
}
BMI_2021$Order[BMI_2021$Order == "Subclass: Acari"] <- "Acari"
BMI_2021[,4:6][is.na(BMI_2021)[,4:6]] <- 0 # replace NAs with 0s

### Merge datasets by year

column_name <- names(BMI_2019[,4:6]) # pull sample names
df_list <- list(BMI_2019, BMI_2020, BMI_2021) # create list of BMI dfs to pull from
BMI_comb <- data.frame() # create empty df
# run for loop to convert samples into wider dfs and merge, retaining year and sample information
for (BMI_df in df_list){
  for (sample in column_name){
    sample_BMI <- BMI_df %>% 
      select(-Class) %>%
      rename(Taxa = Order) %>%
      mutate(Sample = sample) %>%
      dplyr::select(Year, Sample, Taxa, sample) %>%
      pivot_wider(names_from = Taxa, values_from = sample)
    BMI_comb <- bind_rows(BMI_comb, sample_BMI)
  }
  BMI_comb[is.na(BMI_comb)] <- 0
}




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

### Create df averaged across samples (not location-specific)

Taxatotals <- data.frame(Totalcounts = rowSums(Redwood_BMI_2020[,3:11]))
Filtered_Redwood_BMI <- Redwood_BMI_2020 %>%
  select(Order) %>%
  cbind(Taxatotals) %>%
  filter(Order %in% c("Subclass: Acari", "Amphipoda", "Ephemeroptera", "Plecoptera", "Trichoptera", "Coleoptera", "Diptera"))
Filtered_Redwood_BMI$Order[Filtered_Redwood_BMI$Order == "Subclass: Acari"] <- "Acari"

### Location-specific analysis

Redwood_BMI_2020.env <- read_excel("Data/RW_2019-21_StrmInvertData.xlsx", sheet = 3)
