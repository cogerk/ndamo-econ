anamx <- function(df, e_Base=e_Base_std, N2O_AobAmx=N2O_AobAmx_base){
# Mass and energy balance for an Anammox CANON system and High Rate BOD Removal
# By Kathryn Cogert
# Kathryn Cogert 12/6/15

  
  # Loading Calculations
  LNin <- df$Flowrate * df$Nitrogen #kgN/d, Nitrogen load per day
  LNcent <- N_cent * LNin #kgN/d, Nitrogen load from centrate, assumed 40% of total load
  df$LN <- LNin + LNcent #kgN/d, total nitrogen load
  df$LCOD <- df$Flowrate * df$Carbon #kgCOD/d, COD load per day
  temp <- df
  
  # High Rate Activated Sludge for COD Removal
  fCOD_HET <- 1 # Assume 100% conversion of COD 
  temp$px.HET <- fCOD_HET * temp$LCOD * Y_HET * n_conv # Biomass produced/d
  temp$O2.HET <- fCOD_HET * temp$LCOD # O2 demand in A Stage

  # Nitrification
  fN_AOB <- 1.3/2.3 # wt%, fraction of total N in converted by AOB, see appendix
  fN_NOB <- 0  # wt%, frac of totN converted by NOB, assumed 0%
  temp$O2.AOB <- (fN_AOB * temp$LN) / MW_N * MW_O2 * sO2_AOB # kg/D O2 req'd by AOB
  temp$O2.NOB <- (fN_NOB * temp$LN) / MW_N * MW_O2 * sO2_NOB # kg/D O2 req'd by NOB
  temp$px.AOB <- fN_AOB * Y_AOB * n_conv * temp$LN #kg/d, sludge produced from AOB
  temp$px.NOB <- fN_NOB * Y_NOB * n_conv * temp$LN #kg/d, sludge produced from NOB
  
  # Anammox
  fN_anamx <- (1-fN_AOB) # wt%, fraction of N
  
  
  
  temp$px.ANAMX <- fN_anamx * Y_anamx * temp$LN * n_conv #kg/d, sludge produced from anammox
  
  
  # No AnMBR or NDAMO present
  # Therefore these lines are left blank
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # Anaerobic Digester
  temp$px.TOT <- rowSums(dplyr::select(temp, starts_with('px'))) #kg/d, total sludge produced
  temp$px.OUT <- temp$px.TOT * (1-fx_AD)
  temp$V.Biogas.AD <- (temp$px.TOT - temp$px.OUT) * BG
  temp$M.CH4.AD <- temp$V.Biogas.AD * x_biogas_CH4 * rho_BG_dig
  
  # No Methane addition for NDAMO Required, 
  # Therefore these lines are left blank
  
  
  

  
  # Total stoichiometric O2 Demand
  temp$O2.TOT <- rowSums(select(temp, starts_with('O2'))) # Total stoichiometric O2 Demand
  
  # Electricity Req'mts
  temp$E.base <- temp$Flowrate * e_Base
  temp$E.O2 <- temp$O2.TOT * e_O2
  temp$E.Solids <- temp$px.OUT * e_Solids
  
  temp$E.CHP <- temp$M.CH4.AD * e_cogen
  temp$CO2 <- rowSums(select(temp, starts_with('E.'))) * kgCO2.kWh + temp$LN * N2O_AobAmx * CO2eq_N2O
  
  # Cost
  temp$cost <- temp$px.OUT * C_solids + temp$M.CH4.AD * C_CH4_prod + 
    temp$O2.TOT * C_O2 + temp$E.base * -C_electricity
  # Summary
  df$scenario <- rep('B', times=nrow(temp))
  df$COD.added <- 0
  df$sludge.out <- temp$px.OUT
  df$O2.demand <- temp$O2.TOT
  df$CH4.burn <- temp$M.CH4.AD
  df$CO2.equivs  <- temp$CO2
  df$cost  <- temp$cost
  return(df)
}
