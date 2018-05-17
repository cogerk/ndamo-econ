# Run all scenarios for given inputs 
require(tidyverse)
source(file = 'code/scenarioA.R')
source(file = 'code/scenarioB.R')
source(file = 'code/scenarioC.R')
source(file = 'code/scenarioD.R')
source(file = 'code/scenarioE.R')

### Constants
MW_O2 <<- 32
MW_CO2 <<- 44
MW_CH4 <<- 16
MW_N <<- 14
CH4_COD <<- 4
n_conv <<- 1.48          # gVSS/gCOD, conversion constant
H <- 0.0015 # Henry's Constant for methane
CO2eq_CH4 <<- 34 

# Stoichiometry
sCOD_DENIT <<- 5 # gCOD/gN, gCOD required per gN eaten by denitrifiers
sO2_AOB <<- 1.5 # Oxygen stoich coeff AOB 
sO2_NOB <<- 0.5 # Oxygen stoich coeff NOB
sCH4_NDAMO <<- 0.25 # stoich coeff of methane/nitrate for NDAMO    

# Temperature, Pressure, and pH impacts are largely not relevant to this model. 
T.mainstream <<- 20 # Degrees C, assumed
T.AD <<- 37 # Degrees C, assumed
P <<- 1 # atm, assumed

# Ideal Gas Law
R <- 0.082057338 # Universal Gas Constant, m3 atm/ kmol/K
V.molgas.AD <- R * (T.AD + 273.15) / P
V.molgas.main <- R * (T.mainstream + 273.15) / P

# Sludge production Factors
Y_AnMBR <<- 0.036   # gCOD/gCOD
Y_DENIT <<- 0.30/n_conv    # gCOD/gCOD
Y_HET <<- 0.45/n_conv      # gCOD/gCOD
Y_AOB <<- 0.12/n_conv      # gCOD/gN
Y_NOB <<- 0.05/n_conv      # gCOD/gN
Y_anamx <<- 0.13/n_conv    # gCOD/gN
Y_NDAMO <<- 0.071    # gCOD/gCOD
Y_MOB <<- 0.19      # gCOD/gCOD


# Digester & CO2 Production
fx_AD <<- 0.59 # Assumed digester sludge conversion
x_biogas_CH4 <<- 0.62 # Typical concentration of CH4 in biogas
x_biogas_CO2 <<- 1-x_biogas_CH4 # Typical concentration of CH4 in biogas
sCO2_HET <<- 1.2 # kgCO2/kg COD
sCO2_NDAMO <<- MW_CO2/MW_CH4 # kgCO2/kg CH4
sCO2_BURN <<- MW_CO2/MW_CH4 # kgCO2/kg CH4
rho_CH4.dig <<- MW_CH4 / V.molgas.AD #kg/m3
rho_CH4.main <<- MW_CH4 / V.molgas.main
N_cent <<- 0.25

scenarios <- function(Q, cNin, cCODin, 
                      compare=TRUE, expand=TRUE){
  
  #== Run at all combinations of values in N & C vectors if true
  if (expand) {
    df <- expand.grid(Flowrate=Q, Nitrogen=cNin, Carbon=cCODin) 
  } else {
    df <- data.frame(Flowrate=Q, Nitrogen=cNin, Carbon=cCODin)
  }
  

  #== Run Scenarios 
  df.A <- MLE(df)
  df.B <- anamx(df)
  df.C <- anamx_AnMBR(df)
  df.D <- anamx_NDAMO(df)
  df.E <- anamx_NDAMO_AnMBR(df)
  
  #== Compare all theoretical scenarios to base case MLE if true
  if (compare) {
    #= Calculate Sludge Production/O2 Demand as fraction of MLE
    df.B[, c(7:9,11,ncol(df.B))] <- (df.B[, c(7:9,11,ncol(df.B))]-df.A[, c(7:9,11,ncol(df.A))])/df.A[, c(7:9,11,ncol(df.A))]
    df.C[, c(7:9,11,ncol(df.C))] <- (df.C[, c(7:9,11,ncol(df.C))]-df.A[, c(7:9,11,ncol(df.A))])/df.A[, c(7:9,11,ncol(df.A))]
    df.D[, c(7:9,11,ncol(df.D))] <- (df.D[, c(7:9,11,ncol(df.D))]-df.A[, c(7:9,11,ncol(df.A))])/df.A[, c(7:9,11,ncol(df.A))]
    df.E[, c(7:9,11,ncol(df.E))] <- (df.E[, c(7:9,11,ncol(df.E))]-df.A[, c(7:9,11,ncol(df.A))])/df.A[, c(7:9,11,ncol(df.A))]
    
  }
  
  
  return(list(A=df.A, B=df.B, C=df.C, D=df.D, E=df.E))
}




