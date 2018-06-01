anamx_AnMBR <- function(df){
  # Mass and energy balance for mainstream anaerobic membrane bioreactor 
  # with NDAMO and Anammox nitrogen removal system.
  # Kathryn Cogert 12/6/15
  
  
  # Loading Calculations
  LNin <- df$Flowrate * df$Nitrogen #kgN/d, Nitrogen load per day
  LNcent <- N_cent * LNin #kgN/d, Nitrogen load from centrate, assumed 40% of total load
  df$LN <- LNin + LNcent #kgN/d, total nitrogen load
  df$LCOD <- df$Flowrate * df$Carbon #kgCOD/d, COD load per day
  temp <- df 
  
  # No HRAS System present
  # Therefore, these lines are left blank
  
  
  
  
  # Nitrification
  fN_AOB <- 1.3/2.3 # wt%, fraction of total N in converted by AOB, see appendix
  fN_NOB <- 0  # wt%, frac of totN converted by NOB, assumed 100% in anammox system # Make temp dependent?
  temp$O2.AOB <- (fN_AOB * temp$LN) / MW_N * MW_O2 * sO2_AOB # kg/D O2 req'd by AOB
  temp$O2.NOB <- (fN_NOB * temp$LN) / MW_N * MW_O2 * sO2_NOB # kg/D O2 req'd by NOB
  temp$px.AOB <- fN_AOB * Y_AOB * n_conv * temp$LN #kg/d, sludge produced from AOB
  temp$px.NOB <- fN_NOB * Y_NOB * n_conv * temp$LN #kg/d, sludge produced from NOB
  
  # Anammox
  fN_anamx <- (1-fN_AOB) # wt%, fraction of N
  temp$px.ANAMX <- fN_anamx * Y_anamx * temp$LN * n_conv #kg/d, sludge produced from anammox
  
  
  
  
  
  # Mainstream Anaerobic Membrane Digester
  fCOD_AnMBR <- 1 # Assume 100% conversion of COD in AnMBR
  temp$px.AnMBR <- fCOD_AnMBR * temp$LCOD * Y_AnMBR * n_conv # kg/d
  temp$CH4prod.AnMBR <- fCOD_AnMBR * temp$LCOD * (1-Y_AnMBR) * CH4_COD # kg/d CH4 Produced from AnMBR
  
  # Dissolved vs. Gaseous Methane & CO2 Production
  cCH4dis <- H * P * x_biogas_CH4 * MW_CH4 * 1000/1000  # Sat. Dissolved CH4 conc, kgCH4/m3
  temp$LCH4diss <- cCH4dis * df$Flowrate * 10^3 # kg Dissolved, kg/d
  temp$CH4burn.AnMBR <- temp$CH4prod.AnMBR - temp$LCH4diss # Dissolved methane not avail for regen, kg/d
  temp$LCH4diss[which(temp$CH4burn.AnMBR<0)] <- temp$CH4prod.AnMBR[which(temp$CH4burn.AnMBR<0)] # If very little methane produced, assume all dissolves.
  temp$CH4burn.AnMBR[which(temp$CH4burn.AnMBR<0)] <- 0 # Then no methane avail for energy regen
  temp$V.biogas.AnMBR <- temp$CH4prod.AnMBR / rho_CH4.main / x_biogas_CH4 # Volume of biogas produced m3/d
  temp$V.CO2.AnMBR <- temp$V.biogas * (1 - x_biogas_CH4) # assume balance of biogas is CO2
  temp$CO2.AnMBR <- temp$V.CO2.AnMBR / V.molgas.main * MW_CO2
  
  
  
  # Dissolved Methane in Nitrification Reactor (100% Of AnMBR Flow)
  CminCH4 <- 5  # mgCOD/L, minimum concentration at which CH4 will be oxidized
  x.CH4ox <- 0.9 # wt%, fraction of methane oxidized if oxidation occurs
  if (CminCH4 < cCH4dis * 1000) {
    temp$CH4.MOB.ox <- temp$LCH4diss * x.CH4ox
  } else {
    temp$CH4.MOB.ox <- 0
  }
  temp$LCH4 <- temp$LCH4 - temp$CH4.MOB.ox # Residual dissolved methane
  temp$CO2.MOB <- temp$CH4.MOB.ox * sCO2_BURN # CO2 production by MOBs
  temp$O2.MOB <- temp$CH4.MOB.ox / CH4_COD # O2 demand by MOBs
  temp$px.MOB <- temp$CH4.MOB.ox / CH4_COD *  Y_MOB * n_conv # Sludge Production from MOBs
  
  # Anaerobic Digester
  temp$px.TOT <- rowSums(dplyr::select(temp, starts_with('px'))) #kg/d, total sludge produced
  temp$px.OUT <- temp$px.TOT * (1-fx_AD)
  temp$CH4prod.AD <- (temp$px.TOT - temp$px.OUT)/ n_conv * CH4_COD
  temp$CH4burn <- temp$CH4prod.AD + temp$CH4burn.AnMBR # Total methane produced from AnMBR + AD
  temp$V.biogas.AD <- temp$CH4prod.AD / rho_CH4.dig / x_biogas_CH4
  temp$V.CO2.AD <- temp$V.biogas.AD * (1 - x_biogas_CH4) # assume balance of biogas is CO2
  temp$CO2.AD <- temp$V.CO2.AD / V.molgas.AD * MW_CO2
  

  
  
  
  
  
  
  temp$CO2.burn <- temp$CH4burn * sCO2_BURN * MW_CO2 / MW_CH4
  
  # Summary
  temp$O2.TOT <- rowSums(select(temp, starts_with('O2'))) # Total stoichiometric O2 Demand
  temp$CO2.TOT <-  rowSums(select(temp, starts_with('CO2.'),-matches('CO2.burn'))) + (temp$LCH4) * CO2eq_CH4
  
  df$scenario <- rep('C', times=nrow(temp))
  df$COD.added <- 0
  df$sludge.out <- temp$px.OUT
  df$O2.demand <- temp$O2.TOT
  df$CH4.dissolved <- temp$LCH4
  df$CH4.burn <- temp$CH4burn
  df$CO2.equivs  <- temp$CO2.TOT
  return(df)
}
