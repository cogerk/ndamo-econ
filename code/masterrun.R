# Run all scenarios for given inputs 
require(tidyverse)
source(file = 'code/scenarioA.R')
source(file = 'code/scenarioB.R')
source(file = 'code/scenarioC.R')
source(file = 'code/scenarioD.R')
source(file = 'code/scenarioE.R')

### Constants
MW_O2 <<- 32
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
sCH3OH_CO2 <<- 0.92 # kg CO2 released from oxidizing 1 kg of added methanol
sCH4_CO2 <<- 0.69 # kg CO2 released from oxidizing 1 kg of added methane

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
Y_anamx <<- 0.13/n_conv    # gCOD/gN-NH4
Y_NDAMO <<- 0.071    # gCOD/gCOD
Y_MOB <<- 0.19      # gCOD/gCOD


# Digester 
fx_AD <<- 0.59 # Assumed digester sludge conversion
x_biogas_CH4 <<- 0.62 # Typical concentration of CH4 in biogas
N_cent <<- 0.25
BG <<- 0.75 #Produced biogas per kgVSS destroyed
rho_BG_dig <<- 0.86 # Density of biogas at STP kg/m3

# Electrical Demand
kgCO2.kWh <<- 0.47 # kgCO2/kWh
e_Base_std <<- 93 #kWh/ML
e_Solids <<- 2.4 #kWh/kgVSS
e_O2 <<- 1.5 #kWh/kgO2
e_AnMBR <<- 190 #kWh/ML
e_Mix <<- 28 #kWh/ML
e_cogen <<- -2.2 #kWh/kgCH4

# Cost Factors
C_electricity <- 0.078
C_solids <<- -0.19
C_O2 <<- -e_O2 * C_electricity
C_CH4_added <<- -0.03 # COD Basis
C_CH3OH_added <<- -0.29 # COD Basis
C_CH4_prod <<- -e_cogen* C_electricity

scenarios <- function(Q, cNin, cCODin, e_Base=e_Base_std,
                      compare=TRUE, expand=TRUE){
  
  #== Run at all combinations of values in N & C vectors if true
  if (expand) {
    df <- expand.grid(Flowrate=Q, Nitrogen=cNin, Carbon=cCODin) 
  } else {
    df <- data.frame(Flowrate=Q, Nitrogen=cNin, Carbon=cCODin)
  }
  

  #== Run Scenarios 
  df.A <- MLE(df)
  df.B <- anamx(df, e_Base=e_Base)
  df.C <- anamx_AnMBR(df, e_Base=e_Base)
  df.D <- anamx_NDAMO(df, e_Base=e_Base)
  df.E <- anamx_NDAMO_AnMBR(df, e_Base=e_Base)
  
  #== Compare all theoretical scenarios to base case MLE if true
  if (compare) {
    #= Calculate Sludge Production/O2 Demand as fraction of MLE
    df.B[, c(7:11)] <- (df.B[, c(7:11)]-df.A[, c(7:11)])/abs(df.A[, c(7:11)])
    df.C[, c(7:11)] <- (df.C[, c(7:11)]-df.A[, c(7:11)])/abs(df.A[, c(7:11)])
    df.D[, c(7:11)] <- (df.D[, c(7:11)]-df.A[, c(7:11)])/abs(df.A[, c(7:11)])
    df.E[, c(7:11)] <- (df.E[, c(7:11)]-df.A[, c(7:11)])/abs(df.A[, c(7:11)])
    
    df.B$cost <- -(df.A$cost-df.B$cost)
    df.C$cost <- -(df.A$cost-df.C$cost)
    df.D$cost <- -(df.A$cost-df.D$cost)
    df.E$cost <- -(df.A$cost-df.E$cost)
    
  }
  
  
  return(list(A=df.A, B=df.B, C=df.C, D=df.D, E=df.E))
}




