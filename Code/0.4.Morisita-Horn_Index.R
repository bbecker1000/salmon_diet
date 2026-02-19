# Morisita-Horn Index pulled from Willis et al. (2015)
Morisita_Horn_Index <- function(n, P_a, P_b) {
  Numerator_Total <- 0
  Denominator_Total <- 0
  for (i in 1:n) {
    # P_a should be a list or vector
    Numerator <- P_a[i] * P_b[i]
    Denominator <- P_a[i]^2 + P_b[i]^2
    
    Numerator_Total <- Numerator_Total + Numerator
    Denominator_Total <- Denominator_Total + Denominator
  }
  O = 2*Numerator_Total / Denominator_Total
  return(O)
}

# Pull Average Diet df from 0.3PERMANOVAandGRAPH.R

AverageDiet_Wider <- pivot_wider(AverageDiet, values_from = MeanPercentage, names_from = SpeciesCode)

CH_Diet_Vector <- AverageDiet_Wider$CH
CO_Diet_Vector <- AverageDiet_Wider$CO
SH_Diet_Vector <- AverageDiet_Wider$SH

n <- length(SH_Diet_Vector)

Morisita_Horn_Index(n, CH_Diet_Vector, CO_Diet_Vector)
Morisita_Horn_Index(n, SH_Diet_Vector, CO_Diet_Vector)
Morisita_Horn_Index(n, CH_Diet_Vector, SH_Diet_Vector)

AverageDiet 