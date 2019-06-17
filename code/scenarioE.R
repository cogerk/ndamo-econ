anamx_NDAMO_AnMBR <- function(df, e_Base=e_Base_std){
  # Mass and energy balance for mainstream anaerobic membrane bioreactor 
  # with NDAMO and Anammox nitrogen removal system.
  # Kathryn Cogert 12/6/15


  # Loading Calculations
  LNin <- df$Flowrate * df$Nitrogen #kgN/d, Nitrogen load per day
  LNcent <- N_cent * LNin #kgN/d, Nitrogen load from centrate, assumed 40% of total load
  df$LN <- LNin +LNcent  #kgN/d, total nitrogen load
  df$LCOD <- df$Flowrate * df$Carbon #kgCOD/d, COD load per day
  temp <- df  
  
  # No HRAS System present
  # Therefore, these lines are left blank
  
  
  
  # Nitrification
  fN_AOB <- 0.5 # wt%, fraction of total N in converted by AOB, see supplemental calculations
  fN_NOB <- fN_AOB
  temp$O2.AOB <- (fN_AOB * temp$LN) / MW_N * MW_O2 * sO2_AOB # kg/D O2 req'd by AOB
  temp$O2.NOB <- (fN_NOB * temp$LN) / MW_N * MW_O2 * sO2_NOB # kg/D O2 req'd by NOB
  temp$px.AOB<-fN_AOB * Y_AOB * n_conv * temp$LN #kg/d, sludge produced from AOB
  temp$px.NOB<-fN_NOB * Y_NOB* n_conv* temp$LN #kg/d, sludge produced from NOB
  
  # Anammox/NDAMO
  fN_anamx <- (1-fN_AOB) # wt%, fraction of N converted to N2 overall, see supplemental calculations
  fN_NDAMO <- fN_NOB+0.3 # wt%, frac of totN converted by NDAMO, see supplemental calculations
  temp$LCH4_cons <- fN_NDAMO * MW_CH4 / MW_N * sCH4_NDAMO * temp$LN # kgCH4/d, Methane consumed by NDAMO
  temp$px.Anamx<-fN_anamx * Y_anamx * temp$LN* n_conv  #kg/d, sludge produced from anammox
  temp$px.NDAMO<- Y_NDAMO * temp$LCH4_cons * n_conv / CH4_COD  #kg/d, sludge produced from NDAMO
  
  # Mainstream Anaerobic Membrane Digester
  fCOD_AnMBR <- 1 # Assume 100% conversion of COD in AnMBR
  temp$px.AnMBR <- fCOD_AnMBR * temp$LCOD * Y_AnMBR * n_conv # kg/d
  temp$CH4prod.AnMBR <- fCOD_AnMBR * temp$LCOD * (1-Y_AnMBR) * CH4_COD

  # Dissolved vs. Gaseous Methane
  cCH4dis <- 1.5 * H * P * x_biogas_CH4 * MW_CH4 * 1000/1000  # Sat. Dissolved CH4 conc, kgCH4/m3
  temp$LCH4diss <- cCH4dis * df$Flowrate * 10^3 # kg Dissolved, kg/d
  temp$CH4burn.AnMBR <- temp$CH4prod.AnMBR - temp$LCH4diss # Dissolved methane not avail for regen, kg/d
  temp$LCH4diss[which(temp$CH4burn.AnMBR<0)] <- temp$CH4prod.AnMBR[which(temp$CH4burn.AnMBR<0)] # If very little methane produced, assume all dissolves.
  temp$CH4burn.AnMBR[which(temp$CH4burn.AnMBR<0)] <- 0 # Then no methane avail for energy regen
  temp$LCH4diss.NIT <- fN_NOB * temp$LCH4diss # kgCH4 Dissolved/day, half to aerobic nitrification reactor
  temp$LCH4diss.AMX <- temp$LCH4diss - temp$LCH4diss.NIT
  
  # Dissolved Methane in Nitrification Reactor (50% of AnMBR Flow)
  CminCH4 <- 5  # mgCOD/L, minimum concentration at which CH4 will be oxidized
  x.CH4ox <- 0.9 # wt%, fraction of methane oxidized if oxidation occurs
  if (CminCH4 < cCH4dis * 1000) {
    temp$CH4.MOB.ox <- temp$LCH4diss.NIT * x.CH4ox
  } else {
    temp$CH4.MOB.ox <- 0
  } 
  temp$CH4.fromNIT <- 0
  temp$CH4.fromNIT <- temp$LCH4diss.NIT - temp$CH4.MOB.ox # Residual dissolved methane
  temp$O2.MOB <- temp$CH4.MOB.ox / CH4_COD # O2 demand by MOBs
  temp$px.MOB <- temp$CH4.MOB.ox / CH4_COD *  Y_MOB * n_conv # Sludge Production from MOBs
  
  # Anaerobic Digester
  temp$px.TOT <- rowSums(select(temp, starts_with('px'))) #kg/d, total sludge produced
  temp$px.OUT <- temp$px.TOT * (1-fx_AD)
  temp$V.Biogas.AD <- (temp$px.TOT - temp$px.OUT) * BG
  temp$M.CH4.AD <-  temp$V.Biogas.AD * x_biogas_CH4 * rho_BG_dig
  temp$CH4burn <- temp$M.CH4.AD + temp$CH4burn.AnMBR # Total methane produced from AnMBR + AD
  # Methane Balance
  temp$LCH4 <- temp$LCH4diss.AMX - temp$LCH4_cons # kg Dissolved after NDAMO consume, kg/d
  temp$CH4burn[which(temp$LCH4<0)] <- temp$CH4burn[which(temp$LCH4<0)] + temp$LCH4[which(temp$LCH4<0)] # if need more than dissolved, take it from CH4 gas
  temp$LCH4[which(temp$LCH4<0)] <- 0 # if need more than dissolved, dissolved CH4 = 0
  temp$COD.added <- 0
  temp$COD.added[which(temp$CH4burn<0)] <- -temp$CH4burn[which(temp$CH4burn<0)] / CH4_COD  # if need more than produced, get externally
  temp$CH4burn[which(temp$CH4burn<0)] <- 0 # if need more than produced, prodCH4  = 0
 
  # Total stoichiometric O2 Demand
  temp$O2.TOT <- rowSums(select(temp, starts_with('O2'))) 

  # Electricity Req'mts
  temp$E.base <- temp$Flowrate * e_Base
  temp$E.O2 <- temp$O2.TOT * e_O2
  temp$E.Solids <- temp$px.OUT * e_Solids
  temp$E.Mix <- temp$Flowrate * e_Mix
  temp$E.AnMBR <- temp$Flowrate * e_AnMBR
  temp$E.CHP <- temp$CH4burn * e_cogen
  temp$CO2 <- rowSums(select(temp, starts_with('E.'))) * kgCO2.kWh + (temp$LCH4 + temp$CH4.fromNIT)  * CO2eq_CH4  
  
  # Cost
  temp$cost <- temp$px.OUT * C_solids + temp$CH4burn * C_CH4_prod + temp$O2.TOT * C_O2 + 
    temp$COD.added *C_CH4_added  + (temp$E.Mix + temp$E.base + temp$E.AnMBR) * C_electricity

  #Summary
  df$scenario <- rep('E', times=nrow(temp))
  df$COD.added <- temp$COD.added
  df$sludge.out <- temp$px.OUT
  df$O2.demand <- temp$O2.TOT
  df$CH4.burn <- temp$CH4burn
  df$CO2.equivs  <- temp$CO2
  df$cost  <- temp$cost
  return(df)
}
