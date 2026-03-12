library(readxl)
library(tidyverse)
library(ggplot2)
library(vegan)

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

write.csv(BMI_comb, "Temporary_Data_Frames/Combined_BMI.csv")

### Proportion comparison over by sample over time

BMI_summary_samples <- BMI_comb %>%
  mutate(Total = rowSums(across(3:17))) %>%
  pivot_longer(cols = 3:17, names_to = "Taxa", values_to = "Count") %>%
  mutate(Percentage = Count / Total)

ggplot(BMI_summary, aes(x = Year, y = Percentage, fill = Taxa)) +
  geom_bar(stat = "identity") +
  facet_wrap(~Sample)

ggsave("Figures/RW_BMI_by_Sample_Over_Time.png")

BMI_summary_year <- BMI_comb %>%
  pivot_longer(cols = 3:17, names_to = "Taxa", values_to = "Count") %>%
  group_by(Year, Taxa) %>%
  summarize(Count = sum(Count)) %>%
  pivot_wider(names_from = Taxa, values_from = Count) %>%
  mutate(Total = rowSums(across(Acari:Veneroida))) %>%
  pivot_longer(cols = 2:16, names_to = "Taxa", values_to = "Count") %>%
  mutate(Percentage = Count / Total)

ggplot(BMI_summary_year, aes(x = Year, y = Percentage, fill = Taxa)) +
  geom_bar(stat = "identity")

ggsave("Figures/RW_BMI_Over_Time.png")

### NMDS for BMI to see if there are significant changes in invert community across years

BMI_comb_filtered <- BMI_comb %>%
  select(-Odonata, -Turbellaria, -Amphipoda, -Hydroida, -Veneroida, -Basommatophora, -Megaloptera) #remove < 10 total observation taxa

set.seed(123)
BMI_matrix <- as.matrix(BMI_comb_filtered[,3:10])
BMI_nmds <- metaMDS(BMI_matrix, distance= "bray", k=2, try=20)
stressplot(BMI_nmds)
BMI_nmds_point <- as_tibble(BMI_nmds$points)
BMI_nmds_full <- cbind(BMI_comb_filtered[,1:2], BMI_nmds_point)
invert_points <- BMI_nmds$species %>% as.data.frame() %>%
  rownames_to_column(var = 'inverts')
#makes the plot 
ggplot()+
  geom_jitter(data = BMI_nmds_full, 
              aes(x = MDS1, y = MDS2, color = Year, fill = Year),
              size = 3, width = 0.3, height = 0.3)+
  stat_ellipse(data = BMI_nmds_full, 
               aes(x = MDS1, y = MDS2, color = Year, fill = Year),
               geom= "polygon",
               level= 0.8, alpha= 0.3)+
  geom_text_repel(data= invert_points, aes(x= MDS1, y= MDS2, label = inverts), 
                  color ="black", 
                  family= "Helvetica")+
  labs(title = "")+
  theme_bw()

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
