# Run all scenarios for given inputs 
library(tidyverse)
source(file = 'scenarioA.R')
source(file = 'scenarioB.R')
source(file = 'scenarioC.R')
source(file = 'scenarioD.R')

### Constants
MW_O2 <<- 32
MW_CO2 <<- 44
MW_CH4 <<- 16
MW_N <<- 14
CH4_COD <<- 4
n_conv <<- 1.48          # gVSS/gCOD, conversion constant
H <- 0.0015 # Henry's Constant for methane
CO2eq_CH4 <<- 35 

# Stoichiometry
sCOD_HET <<- 5 # gCOD/gN, gCOD required per gN eaten by denitrifiers
sO2_AOB <<- 1.5 # Oxygen stoich coeff AOB 
sO2_NOB <<- 0.5 # Oxygen stoich coeff NOB
sCH4_NDAMO <<- 0.635 # stoich coeff of methane/nitrate for NDAMO    

# Temperature, Pressure, and pH impacts are largely not relevant to this model. 
T.mainstream <<- 25 # Degrees C, assumed
T.digester <<- 37 # Degrees C, assumed
pH <<- 7
P <<- 1 # atm, assumed

# Ideal Gas Law
R <- 0.082057338 # Universal Gas Constant, m3 atm/ kmol/K
vol.1molgas <- R * (T.digester + 273.15) / P

# Sludge production Factors
fx_AnMBR <<- 0.036   # gCOD/gCOD, gCOD of sludge produced in AnMBR per gCOD eaten
fx_DENIT <<- 0.32    # gCOD/gCOD, gCOD of sludge produced per gCOD eaten by denitrifiers
fx_HET <<- 0.21      # gCOD/gCOD, gCOD of sludge produced per gCOD eaten by COD oxidizing heterotrophs
fx_AOB <<- 0.11      # gCOD/gN, gCOD of sludge produced per gN eaten by AOB
fx_NOB <<- 0.036     # gCOD/gN, gCOD of sludge produced per gN eaten by NOB
fx_anamx <<- 0.13    # gCOD/gN, gCOD of sludge produced per gN eaten by anammox
fx_NDAMO <<- 0.22    # gCOD/gCH4,	gCOD of sludge produced per gCH4 eaten by NDAMO.

# Digester & CO2 Production
fx_digester <<- 0.59 # Assumed digester sludge conversion
fbiogas_CH4 <<- 0.62 # Typical concentration of CH4 in biogas
fCO2_HET <<- 0.08 # kgCO2/kg COD
fCO2_NDAMO <<- MW_CO2/MW_CH4 # kgCO2/kg CH4
fCO2_BURN <<- MW_CO2/MW_CH4 # kgCO2/kg CH4
rho_CH4 <<- MW_CH4 / vol.1molgas #kg/m3



scenarios <- function(Q, cNin, cCODin, 
                      compare=TRUE, expand=TRUE){
  
  #== Run at all combinations of values in N & C vectors if true
  if (expand) {
    df <- expand.grid(Flowrate=Q, Nitrogen=cNin, Carbon=cCODin) 
  } else {
    df <- data.frame(Flowrate=Q, Nitrogen=cNin, Carbon=cCODin)
  }
  
  #== Loading Calculations
  LNin <- df$Flowrate * df$Nitrogen #kgN/d, Nitrogen load per day
  LNcent <- .4 * LNin #kgN/d, Nitrogen load from centrate, assumed 40% of total load
  df$LN <- LNin + LNcent #kgN/d, total nitrogen load
  df$LCOD <- df$Flowrate * df$Carbon #kgCOD/d, COD load per day

  #== Run Scenarios 
  df.A <- MLE(df)
  df.B <- anamx(df)
  df.C <- anamx_NDAMO(df)
  df.D <- anamx_NDAMO_AnMBR(df)
  
  #== Compare all theoretical scenarios to base case MLE if true
  if (compare) {
    #= Calculate Sludge Production/O2 Demand as fraction of MLE
    df.B[, c(7:9,ncol(df.B))] <- (df.B[, c(7:9,ncol(df.B))]-df.A[, c(7:9,ncol(df.A))])/df.A[, c(7:9,ncol(df.A))]
    df.C[, c(7:9,ncol(df.C))] <- (df.C[, c(7:9,ncol(df.C))]-df.A[, c(7:9,ncol(df.A))])/df.A[, c(7:9,ncol(df.A))]
    df.D[, c(7:9,ncol(df.D))] <- (df.D[, c(7:9,ncol(df.D))]-df.A[, c(7:9,ncol(df.A))])/df.A[, c(7:9,ncol(df.A))]
    df.D$COD.added[is.nan(df.D$COD.added)] <- 0
  }
  
  
  return(list(A=df.A, B=df.B, C=df.C, D=df.D))
}




