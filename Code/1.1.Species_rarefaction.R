library(vegan) 
library(readxl)

## Need environmental information x abundance matrix -> pull DietDataWhole from 0.2.manyglm_test 
View(DietDataWhole)

##Separate data frame by species and site to create multiple curves
grouplabel <- DietDataWhole$SpeciesCode
preycount <- DietDataWhole[, 27:63]

#sum prey type counts by salmonid species 
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

## iNEXT 
library(iNEXT)
  ##reformat for iNEXT with species as rows 
speciesgroup <- iNEXT(t(speciesgroups_matrix), q=0, datatype = "abundance")
plot(speciesgroup, type=3)
