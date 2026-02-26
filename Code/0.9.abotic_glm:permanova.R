library(readxl)
library(tidyverse)

DietData <- read_excel("Data/DietData.xlsx")
DietData_env <- read_excel("Data/DietData_env.xlsx")

# Combine environmental with diet datasets by sampleID
CombindedDietData <- cbind(DietData_env, DietData)

##Glm for each species association with abiotic factors 

CombindedDietData$CO <- ifelse(CombindedDietData$SpeciesCode == "CO", 1, 0)
glm(CO ~ HabitatType + PoolComplexity +
      RootWad + PercentCover + SWDJam + LWDJam,
    data = CombindedDietData,
    family = binomial)

CombindedDietData$SH <- ifelse(CombindedDietData$SpeciesCode == "SH", 1, 0)
glm(SH ~ HabitatType + PoolComplexity +
      RootWad + PercentCover + SWDJam + LWDJam,
    data = CombindedDietData,
    family = binomial)

CombindedDietData$CH <- ifelse(CombindedDietData$SpeciesCode == "CH", 1, 0)
glm(CH ~ HabitatType + PoolComplexity +
      RootWad + PercentCover + SWDJam + LWDJam,
    data = CombindedDietData,
    family = binomial)

#PERMANOVA for aboitic factors == speciescode, habitiat type, pool surface area, percent cover, and small wood debris sig
library(ecodist)
library(vegan)

BrayCurtisMatrix3 <- vegdist(DietData, method= "bray")
PermanovaResult3 <- adonis2(BrayCurtisMatrix3 ~ SpeciesCode + HabitatType  +PoolComplexity +EstSurfaceArea_msq +RootWad +PercentCover +SWDJam +LWDJam,  
                           data= CombindedDietData, 
                           by= "margin")
print(PermanovaResult3)
summary(PermanovaResult3)