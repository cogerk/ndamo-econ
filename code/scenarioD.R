anamx_NDAMO<- function(df){
  # Mass and energy balance for anammox/NDAMO system
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
  fN_AOB <- 0.5 # wt%, fraction of total N in converted by AOB, see supplemental calculations
  fN_NOB <- fN_AOB
  temp$O2.AOB <- (fN_AOB * temp$LN) / MW_N * MW_O2 * sO2_AOB # kg/D O2 req'd by AOB
  temp$O2.NOB <- (fN_NOB * temp$LN) / MW_N * MW_O2 * sO2_NOB # kg/D O2 req'd by NOB
  temp$px.AOB <- fN_AOB * Y_AOB * n_conv * temp$LN #kg/d, sludge produced from AOB
  temp$px.NOB <- fN_NOB * Y_NOB * n_conv * temp$LN #kg/d, sludge produced from NOB
  
  # Anammox/NDAMO
  fN_anamx <- (1-fN_AOB) # wt%, fraction of N converted to N2 overall, see supplemental calculations
  fN_NDAMO <- fN_NOB + 0.3 # wt%, frac of totN converted by NDAMO, see supplemental calculations
  temp$LCH4_cons <- temp$LN * fN_NDAMO / MW_N * sCH4_NDAMO * MW_CH4  # kgCH4/d, Methane consumed by NDAMO
  temp$px.ANAMX <- fN_anamx * Y_anamx * temp$LN * n_conv #kg/d, sludge produced from anammox
  temp$px.NDAMO <- Y_NDAMO * temp$LCH4_cons / CH4_COD * n_conv  #kg/d, sludge produced from NDAMO

  # No AnMBR system present
  # Therefore, these lines are left blank
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # Anaerobic Digester
  temp$px.TOT <- rowSums(select(temp, starts_with('px'))) #kg/d, total sludge produced
  temp$px.OUT <- temp$px.TOT * (1-fx_AD)
  temp$CH4prod <- (temp$px.TOT - temp$px.OUT)/ n_conv * CH4_COD
  
  
  # Methane Addition for NDAMO/Methane Production for Energy Regeneration
  
  temp$CH4burn <- temp$CH4prod - temp$LCH4_cons # kg Dissolved after NDAMO consume, kg/d
  
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
  
  temp$E.CHP <- temp$CH4burn * e_cogen
  temp$CO2 <- rowSums(select(temp, starts_with('E.'))) * kgCO2.kWh
  
  # Cost
  temp$cost <- temp$px.OUT * .31 - temp$CH4burn * .36 + temp$O2.TOT * 0.052 + temp$COD.added *.142
  
  #Summary
  df$scenario <- rep('D', times=nrow(temp))
  df$COD.added <- temp$COD.added
  df$sludge.out <- temp$px.OUT
  df$O2.demand <- temp$O2.TOT
  df$CH4.burn <- temp$CH4burn
  df$CO2.equivs  <-  temp$CO2
  df$cost  <- temp$cost
  return(df)
}
