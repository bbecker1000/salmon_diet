
## 2024/2025
# Ruby Sainz Armendariz
# Aquatic Ecology Intern, GOGA

# Salmonid Diet Study in Redwood Creek, Golden Gate National Recreation Area, CA



rm(list=ls()) #clears the environment


#Load in libraries
library(readr)
library(readxl)
library(tidyverse)  
library(vegan)
library(magrittr)
library(dplyr)
library(ggord)
library(ggrepel)
library(tidyr)
library(scales)
library(ape)
library(patchwork)

#Load in data
#setwd("M:/Divisions/Natural Resources/Wildlife/Fish/Salmon/Redwood/Salmonid_diet/Data/RData")
#setwd("/Users/rubysainz/Desktop/NPS/Salmonid R analysis")
DietData <- read_xlsx("Data/DietData.xlsx") #Invertebrate orders
DietData_env <- read_xlsx("Data/DietData_env.xlsx") #Fish and habitat
DietData_bins <- read_xlsx("Data/DietData_bins.xlsx") #Invertebrate orders in benthic/surface categories
DietData_All <- read_xlsx("Data/DietData_All.xlsx") #Invertebrate orders and fish/habitat data (all data except benthic/surface categories). Used for Question 3.

## Question 1: What is the diet overlap between the coho, steelhead and chinook?
#A NMDS will be performed to find the best fit model, or the diet that overlaps the 
#species the best. Then a PERMANOVA will be run to test whether diets are statistically 
#different among fish species, and if so, how different.


#Remove rare taxa from data (under 10) and "junk" orders (unknowns, detritus, seed, trash, NA's)
column_sums <- colSums(DietData)
print(column_sums)
hist(column_sums,
     breaks = pretty(column_sums, n = 50),
     xlab = "total",
     ylab = "Number of orders")
axis(1, at = seq(0,425, by = 25))

column_sums2 <- colSums(DietData_bins)
print(column_sums2)
hist(column_sums2,
     breaks = pretty(column_sums2, n = 50),
     xlab = "total",
     ylab = "Number of orders")
axis(1, at = seq(0,425, by = 25))

DietData_condensed <- subset(DietData, select = -c(Decapoda,Oligochaeta,Collembola,detritus_total,Bivalvia,seed,Orthoptera,unk_plant_material,trash,Slug,unk_invert,Siphonaptera,Salmoniformes,Unknown,Odonata,Neuroptera,unknown_fish,Lepidoptera,sand_gravel_total,Megaloptera,Annelida,Thysanoptera))
DietData_bins_condensed <- subset(DietData_bins, select = -c(Decapoda_benthic,Odonata_surface,Bivalvia_benthic,trash_NA,Odonata_NA,Neuroptera_benthic,Megaloptera_surface,unknown_fish_NA,Orthoptera_NA,Lepidoptera_benthic,Ephemeroptera_surface,Acari_surface,seed_NA,Salmoniformes_NA,Collembola_NA,Oligochaeta_benthic,Plecoptera_surface,Annelida_NA,Siphonaptera_surface,Gastropoda_snail_surface,Slug_surface,Isopoda_surface,unk_invert_NA,Ephemeroptera_NA,Diptera_NA,Lepidoptera_NA,detritus_total_NA,Unknown_NA,unk_plant_material_NA,Trichoptera_NA,sand_gravel_total_NA,Isopoda_NA,Coleoptera_NA,Megaloptera_benthic, Lepidoptera_surface,Thysanoptera_surface))

column_sums <- colSums(DietData_condensed)
print(column_sums)
DietData_check <- subset(DietData, select = c(Decapoda,Oligochaeta,Collembola,detritus_total,Bivalvia,seed,Orthoptera,unk_plant_material,trash,Slug,unk_invert,Siphonaptera,Salmoniformes,Unknown,Odonata,Neuroptera,unknown_fish,Lepidoptera,sand_gravel_total,Megaloptera,Annelida,Thysanoptera))
column_sums_check <- colSums(DietData_check)
print(column_sums_check)

column_sums2 <- colSums(DietData_bins_condensed)
print(column_sums2)

#Check for empty rows
DietData_condensed$ROWSUM <- rowSums(DietData_condensed)

t1 <- bind_cols(DietData_env, DietData_condensed)

t2 <- t1 %>% filter(ROWSUM != 0)

DietData_env <- t2[,c(1:26)]
DietData_condensed <- t2[,c(27:41)] #(removing rowsum!)

DietData_bins_condensed$ROWSUM <- rowSums(DietData_bins_condensed)
DietData_env <- read_xlsx("DietData_env.xlsx") #Fish and habitat
t3 <- bind_cols(DietData_env, DietData_bins_condensed)

t4 <- t3 %>% filter(ROWSUM != 0)

DietData_env_bins <- t4[,c(1:26)]
DietData_bins_condensed <- t4[,c(27:45)] #(removing rowsum!)
DietData_env <- t2[,c(1:26)] #Need to repeat this due to line 81

#NMDS
nms <- metaMDS(DietData_condensed, trymax = 25) 
nms <- metaMDS(DietData_bins_condensed, trymax = 25) 
nms

plot(nms)


#Create a graph that shows the diet overlap between the three species with CH as the reference category
#For regular data (DietData_condensed and DietData_env)
library(gllvm)

#Convert SpeciesCode to a factor
DietData_env$SpeciesCode  <- as.factor(DietData_env$SpeciesCode)

#Making chinook the reference category
DietData_env$SpeciesCode  <- relevel(DietData_env$SpeciesCode , ref = "CH")

#reset graphics
par(mfrow = c(1, 1))

fit_env.nb <- gllvm(DietData_condensed,  # [,c(23, 32, 11, 18, 28, 14)]
                    DietData_env, family = "poisson", 
                    num.lv = 1,
                    formula = ~ SpeciesCode + LifeStage,
                    seed = 1234)
#summary(fit_env.nb)
#plot(fit_env.nb, mfrow=c(3,2))
coefplot(fit_env.nb, cex.ylab = 0.7, mar = c(4, 5, 2, 1), mfrow=c(1,5),
         order = TRUE, main="")

#For binned data (DietData_bins and DietData_env_bins)
DietData_env_bins$SpeciesCode  <- as.factor(DietData_env_bins$SpeciesCode)
DietData_env_bins$SpeciesCode  <- relevel(DietData_env_bins$SpeciesCode , ref = "CH")

par(mfrow = c(1, 1))

fit_env.nb <- gllvm(DietData_bins_condensed,  # [,c(23, 32, 11, 18, 28, 14)]
                    DietData_env_bins, family = "poisson", 
                    num.lv = 1,
                    formula = ~ SpeciesCode + LifeStage,
                    seed = 1234)
#summary(fit_env.nb)
#plot(fit_env.nb, mfrow=c(3,2))
coefplot(fit_env.nb, cex.ylab = 0.7, mar = c(4, 5, 2, 1), mfrow=c(1,3),
         order = TRUE )

library(stringr)

#Make the plots look nicer
#Use this code for regular and binned data. Only difference is presence/absence of plot legend. 
#All points for both sets of data were reliable, so reliability was omitted in the legend.

estimate <- data.frame(fit_env.nb[["params"]][["Xcoef"]])
estimate <- tibble::rownames_to_column(estimate, "Species")
estimate.sd <- data.frame(fit_env.nb[["sd"]][["Xcoef"]])
estimate.sd <- tibble::rownames_to_column(estimate.sd, "Species.sd")

#make df
estimate.tidy <- estimate %>% 
  pivot_longer(cols = c(-Species) , names_to = "Covariate", values_to = "Estimate")

estimate.sd.tidy <- estimate.sd %>% 
  pivot_longer(cols = c(-Species.sd) , names_to = "Covariate", values_to = "Estimate.sd")

glvvm.coef.plot <- tibble(cbind(estimate.tidy, estimate.sd.tidy[,-2]))
glvvm.coef.plot$Benthic_vs_surface <- str_sub(glvvm.coef.plot$Species,-7,-1)

## add covariate if large sd
#glvvm.coef.plot$reliability <- ifelse(glvvm.coef.plot$Estimate.sd > 5, "unreliable", "reliable")

#Plot
#For the regular data, comment out the color = Benthic_vs_surface, facet_wrap, Legend.position, change the title, etc?
plot.gllvm <- ggplot(glvvm.coef.plot, aes(reorder(Species, -Estimate), Estimate,
                                          color = Benthic_vs_surface,
                                          #shape = reliability
                                          )) + 
  geom_pointrange(aes(ymin = Estimate-2*Estimate.sd, ymax = Estimate+2*Estimate.sd)) +
  coord_flip() +
  geom_hline(yintercept = 0, linetype = 2) +
  ylim(-16,16) +
  facet_wrap(.~Covariate) + 
  labs(color = "Benthic vs surface") +  # Change legend title here
  theme_gray(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    axis.title = element_blank(),
    axis.text = element_text(size = 10),
    plot.margin = margin(10, 30, 10, 20),
    #legend.position = "none"
  )

print(plot.gllvm)
ggsave("p.gllvm.binned.jpg", width = 10, height = 8, units = "in", dpi = 300)

dev.off()



#For the results of the PERMANOVA, pay attention to the Rsquared and Pr(>F) columns. 
#The Rsquared is a percentage (0-100%). If the Rsquared value is really low, that means 
#that the env variable does not really play a role in the dependent variable. 
#The Residual is any variation that cannot be explained by the data.



#Question 2: Does this diet overlap vary spatially?
#Perform a PERMANOVA to see if location affects their diet and then visualize the data using an NMDS.

#Make sure variables are factors
DietData_env$BasinWideUnit <- factor(DietData_env$BasinWideUnit)
DietData_env$SectionNumber <- factor(DietData_env$SectionNumber)

#Run the models
m1.adonis <- adonis2(DietData_condensed ~ BasinWideUnit, data = DietData_env, permutations=1000) #27.7%
m1.adonis
m2.adonis <- adonis2(DietData_condensed ~ SectionNumber, data = DietData_env, permutations=1000) #24.4%
m2.adonis
#m3.adonis <- adonis2(DietData_condensed ~ BasinWideUnit * SectionNumber, data = DietData_env, permutations=1000)
#m3.adonis <- adonis2(DietData_condensed ~ BasinWideUnit + SectionNumber + BasinWideUnit:SectionNumber,
#                     data = DietData_env, permutations = 1000)
#m3.adonis
#table(DietData_env$BasinWideUnit, DietData_env$SectionNumber)
#the model with BasinWideUnit and SectionNumber did not run the interaction between them likely because they are too 
#closely related (SectionNumber is nested within BasinWideUnit), causing the interaction term to be non-significant and drop out. 
m4.adonis <- adonis2(DietData_condensed ~ newcategoryname + SpeciesCode, data =DietData_env, permutation = 1000) # run the model with a + or *

nmds <- metaMDS(DietData_condensed, distance = "bray", k = 2, trymax = 30)

# Extract NMDS coordinates
nmds_points <- as.data.frame(nmds$points)
nmds_points$SectionNumber <- DietData_env$SectionNumber  # Add grouping info

species <- data.frame(nmds[["species"]])
species <- tibble::rownames_to_column(species, "species")

ggplot()+
  geom_text_repel(data = species,aes(MDS1,MDS2,label=species))

nmds$stress
# 0.1024776 (good fit) 

p.nmds <-ggplot(nmds_points, aes(x = MDS1, y = MDS2, color = SectionNumber)) +
  geom_point(size = 3) +
  stat_ellipse(level = 0.95) +  # optional
  theme_minimal() +
  labs(title = "NMDS of Diet Data", x = "NMDS1", y = "NMDS2")

print(p.nmds)

#Although SectionNumber is being used which has less variables than BasinWideUnit, this plot still has so 
#much noise. Therefore, a different route will be taken to determine if diet overlap varies spatially.

#Create a bar plot with BasinWideUnit on the X axis and Littorinimorpha on the Y axis to determine where 
#new categories should be created.

ggplot(DietData_All %>% filter(Littorinimorpha > 0), aes(x = as.factor(BasinWideUnit))) +
  geom_bar() +
  labs(x = "Section Number", y = "Presence Counts of Littorinimorpha") +
  theme_minimal()
#This bar chart shows that there are only 5 BW units (1,2,53,148 and 587) that have fish with snails. 
#All of them are below the mini cascade. All of them were 2022 except for 1 fish in BW unit 1 which was 2020.

#Sort the location data into 2 groups - below the mini cascade and above. Cascade is at BW unit 596 
#for 2022. Not using the 2020 BW unit since it won't affect the results b/c the only fish with NZMS was at BW 1.

#For non-binned data (DietData_env)
DietData_env$Above_or_Below_Cascade <- as.factor(ifelse(as.numeric(as.character(DietData_env$BasinWideUnit)) < 597, 1, 2)) #Groups are BW unit 1-596 and 597+

DietData_env$Above_or_Below_Cascade <- as.factor(ifelse(as.numeric(as.character(DietData_env$BasinWideUnit)) < 597, "Below", "Above")) #Groups are BW unit 1-596 and 597+

m5.adonis <- adonis2(DietData_condensed ~ Above_or_Below_Cascade + SpeciesCode, data =DietData_env, permutation = 1000) # run the model with a + or *
m5.adonis

nmds <- metaMDS(DietData_condensed, distance = "bray", k = 2, trymax = 30)

# Extract NMDS coordinates
nmds_points <- as.data.frame(nmds$points)
nmds_points$Above_or_Below_Cascade <- DietData_env$Above_or_Below_Cascade  # Add grouping info

species <- data.frame(nmds[["species"]])
species <- tibble::rownames_to_column(species, "species")

ggplot()+
  geom_text_repel(data = species,aes(MDS1,MDS2,label=species)) 

nmds$stress
# 0.1957224

p.nmds1 <-ggplot(nmds_points, aes(x = MDS1, y = MDS2, color = Above_or_Below_Cascade)) +
  geom_point(size = 3) +
  stat_ellipse(level = 0.95) +  # optional
  theme_minimal() +
  labs(color = "Above or below NZMS boundary", title = "Uncategorized data", x = "NMDS1", y = "NMDS2") +
  theme(plot.title = element_text(size = 12),
        legend.position = "none") #comment out when NOT combining p.nmds1 and p.nmds2

print(p.nmds1)


#For binned data (DietData_env_bins)
DietData_env_bins$Above_or_Below_Cascade <- as.factor(ifelse(as.numeric(as.character(DietData_env_bins$BasinWideUnit)) < 597, "Below", "Above")) #Groups are BW unit 1-596 and 597+

m5.adonis <- adonis2(DietData_bins_condensed ~ Above_or_Below_Cascade + SpeciesCode, data =DietData_env_bins, permutation = 1000) # run the model with a + or *
m5.adonis

nmds <- metaMDS(DietData_bins_condensed, distance = "bray", k = 2, trymax = 30)

# Extract NMDS coordinates
nmds_points <- as.data.frame(nmds$points)
nmds_points$Above_or_Below_Cascade <- DietData_env_bins$Above_or_Below_Cascade  # Add grouping info

species <- data.frame(nmds[["species"]])
species <- tibble::rownames_to_column(species, "species")

ggplot()+
  geom_text_repel(data = species,aes(MDS1,MDS2,label=species)) +
  labs(title = "Average NMDS of Diet Data", x = "NMDS1", y = "NMDS2")

nmds$stress
# 0.1951975

p.nmds2 <-ggplot(nmds_points, aes(x = MDS1, y = MDS2, color = Above_or_Below_Cascade)) +
  geom_point(size = 3) +
  stat_ellipse(level = 0.95) +  # optional
  theme_minimal() +
  labs(color = "Above or below NZMS boundary", title = "Categorized data", x = "NMDS1", y = "NMDS2") +
  theme(axis.title.y = element_blank(), #comment out when NOT combining p.nmds1 and p.nmds2
  legend.text = element_text(size = 8),       # text size
  legend.title = element_text(size = 9),      # title size (optional)
  legend.key.size = unit(0.4, "cm"),
  plot.title = element_text(size = 12))           # key (icon) size

print(p.nmds2)

#Combine both ggplots so they can be compared side-by-side
p.nmds.combined <- p.nmds1 + p.nmds2
p.nmds.combined

ggsave("p.nms.combined.jpg", width = 10, height = 8, units = "in", dpi = 300)





# Compute Bray-Curtis dissimilarity
#dist_matrix <- vegdist(DietData_numeric, method = "bray")

# Run PCoA
#pcoa_res <- pcoa(dist_matrix)

# Create plot data
#pcoa_points <- as.data.frame(pcoa_res$vectors)
#pcoa_points$SectionNumber <- DietData_env$SectionNumber

# Plot
#ggplot(pcoa_points, aes(x = Axis.1, y = Axis.2, color = SectionNumber)) +
#  geom_point(size = 3) +
#  stat_ellipse(level = 0.95) +
#  labs(x = "PCoA1", y = "PCoA2", title = "PCoA (Bray-Curtis)") +
#  theme_minimal()




#PCA
#pca_result <- rda(DietData_condensed)  # or use prcomp(DietData_condensed) for standard PCA

# Plot PCA
#biplot(pca_result, scaling = 1)

#pca_df <- as.data.frame(scores(pca_result, display = "sites"))

#ggplot(pca_df, aes(x = PC1, y = PC2, color = DietData_env$SectionNumber)) +
#  geom_point() +
#  theme_minimal() +
#  labs(title = "PCA Plot", x = "Principal Component 1", y = "Principal Component 2")



#fit <- envfit(nmds, DietData_numeric, permutations = 999)
#plot(nmds)
#plot(fit, p.max = 0.05, col = "red")  # Adds vectors for significant diet variables
#ggsave("nms.jpg", width = 10, height = 8, units = "in", dpi = 300)





#Question 3: What fish species and life stage eat New Zealand mud snails? To answer this, I will 
#make two bar charts of % fish with NZMS in gut and SpeciesCode/LifeStage for 2020 and 2022.

#Create column for presence/absence of NZMS in each fish gut using DietData_All
DietData_All$NZMS_present = DietData_All$Littorinimorpha

DietData_All <- DietData_All %>%
  mutate(NZMS_present = if_else(NZMS_present != 0, 1, NZMS_present))

#Convert to numeric
DietData_All <- DietData_All %>%
  mutate(NZMS_present = as.numeric(NZMS_present))

#Summarize the data to calculate the percentage of fish that ate NZMS
df_summary <- DietData_All %>%
  group_by(FieldSeason, SpeciesCode, LifeStage) %>%
  summarise(
    total_fish = n(),
    fish_eaten_NZMS = sum(NZMS_present),
    percent_eaten = (fish_eaten_NZMS / total_fish) * 100)

#Create the plot
plot.percent.eaten <- ggplot(df_summary, aes(x = interaction(SpeciesCode, LifeStage), y = percent_eaten, fill = FieldSeason)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = paste0(round(percent_eaten, 1), "%")), #add percent labels to the bars
            position = position_dodge(width = 0.9),
            vjust = -0.5,
            size = 3) +
  #facet_wrap(~ FieldSeason) +  # This will create one plot per year
  scale_fill_grey(start = 0.8, end = 0.8)+  # light to dark
  theme_minimal() +
  labs(
    x = "Fish species and life stage",
    y = "% fish that consumed NZMS",
    fill = "Fish species") +
  theme(legend.position = "none", 
        axis.text.x = element_text(angle = 45, hjust = 1))  # To rotate x-axis labels if needed

print(plot.percent.eaten)
ggsave("p.percent.eaten.jpg", width = 10, height = 8, units = "in", dpi = 300)






