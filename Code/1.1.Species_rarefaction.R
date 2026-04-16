library(vegan) 
library(readxl)
library(dplyr)
library(iNEXT)

## Need environmental information x abundance matrix -> pull DietDataWhole from 0.2.manyglm_test 
View(DietDataWhole)

## ## Prep to create curves for prey type counts by salmonid species 
grouplabel <- DietDataWhole$SpeciesCode
preycount <- DietDataWhole[, 27:63] 
speciesgroups <- aggregate(preycount, by= list(Group=grouplabel), FUN=sum)
View(speciesgroups)

#format for rarefaction curve 
rownames(speciesgroups) <- speciesgroups$Group
speciesgroups_matrix <- as.matrix(speciesgroups[,-1]) #Removing label column 

## Plot rarefaction curve
rarecurve(speciesgroups_matrix, step = 5, 
          col = c("red","blue","green"),
          label= TRUE,
          xlab = "Sample Size", 
          ylab = "Species Richness",
          main= "Rarefaction curves for prey accumulation")

##reformat for iNEXT with species as rows 
speciesgroup <- iNEXT(t(speciesgroups_matrix), q=0, datatype = "abundance")

#plot iNEXT: rarefaction, extrapolation, confidence intervals 
plot(speciesgroup, type=3)


## now by habitat character as well 
#delineate prey assemblages by location 
DietHabitatChar <- DietDataWhole %>%
  mutate(HabChar = case_when(
    SectionNumber >= 0 & SectionNumber <= 3.9 ~ "PacificWay",
    SectionNumber >= 4 & SectionNumber <= 17.9 ~ "Highway1", 
    SectionNumber >= 18 & SectionNumber <= 28.9 ~ "FrankValley",
    SectionNumber >= 29 & SectionNumber <= 49.9 ~ "KentCreekTrail",
    SectionNumber >= 50 & SectionNumber <= 68.9  ~ "Dipsea",
    SectionNumber >= 69 & SectionNumber <= 74 ~ "Foot4",
  ))
View(DietHabitatChar)

## Separate by species and habitat character
  #CH localities ==== not sig dif in diversity 
    CHHabChar <- subset(DietHabitatChar, SpeciesCode == "CH")
    ## ## Prep to create curves for habitat character within CH samples 
    CHgrouplabel <- CHHabChar$HabChar
    CHpreycount <- CHHabChar[, 27:63]
    CHspeciesgroups <- aggregate(CHpreycount, by= list(Group=CHgrouplabel), FUN=sum)
    View(CHspeciesgroups)
    #format for rarefaction curve 
    rownames(CHspeciesgroups) <- CHspeciesgroups$Group
    CHspeciesgroups_matrix <- as.matrix(CHspeciesgroups[,-1]) #Removing label column 
    ## Plot rarefaction curve
    rarecurve(CHspeciesgroups_matrix, step = 5, 
          col = c("red","blue","green"),
          label= TRUE,
          xlab = "Sample Size", 
          ylab = "Species Richness",
          main= "Rarefaction curves for prey accumulation")
    ##reformat for iNEXT with species as rows 
    CHspeciesgroup <- iNEXT(t(CHspeciesgroups_matrix), q=0, datatype = "abundance")
    #plot iNEXT: rarefaction, extrapolation, confidence intervals 
    plot(CHspeciesgroup, type=3)
    
    #SH localities ==== more? just because pacific way though 
    SHHabChar <- subset(DietHabitatChar, SpeciesCode == "SH")
    ## ## Prep to create curves for habitat character within CH samples 
    SHgrouplabel <- SHHabChar$HabChar
    SHpreycount <- SHHabChar[, 27:63]
    SHspeciesgroups <- aggregate(SHpreycount, by= list(Group=SHgrouplabel), FUN=sum)
    View(SHspeciesgroups)
    #format for rarefaction curve 
    rownames(SHspeciesgroups) <- SHspeciesgroups$Group
    SHspeciesgroups_matrix <- as.matrix(SHspeciesgroups[,-1]) #Removing label column 
    ## Plot rarefaction curve
    rarecurve(SHspeciesgroups_matrix, step = 5, 
              col = c("red","blue","green"),
              label= TRUE,
              xlab = "Sample Size", 
              ylab = "Species Richness",
              main= "Rarefaction curves for prey accumulation")
    ##reformat for iNEXT with species as rows 
    SHspeciesgroup <- iNEXT(t(SHspeciesgroups_matrix), q=0, datatype = "abundance")
    #plot iNEXT: rarefaction, extrapolation, confidence intervals 
    plot(SHspeciesgroup, type=3)
    
    #CO localities ==== pretty much no difference between characters 
    COHabChar <- subset(DietHabitatChar, SpeciesCode == "CO")
    ## ## Prep to create curves for habitat character within CH samples 
    COgrouplabel <- COHabChar$HabChar
    COpreycount <- COHabChar[, 27:63]
    COspeciesgroups <- aggregate(COpreycount, by= list(Group=COgrouplabel), FUN=sum)
    View(COspeciesgroups)
    #format for rarefaction curve 
    rownames(COspeciesgroups) <- COspeciesgroups$Group
    COspeciesgroups_matrix <- as.matrix(COspeciesgroups[,-1]) #Removing label column 
    ## Plot rarefaction curve
    rarecurve(COspeciesgroups_matrix, step = 5, 
              col = c("red","blue","green"),
              label= TRUE,
              xlab = "Sample Size", 
              ylab = "Species Richness",
              main= "Rarefaction curves for prey accumulation")
    ##reformat for iNEXT with species as rows 
    COspeciesgroup <- iNEXT(t(COspeciesgroups_matrix), q=0, datatype = "abundance")
    #plot iNEXT: rarefaction, extrapolation, confidence intervals 
    plot(COspeciesgroup, type=3)





