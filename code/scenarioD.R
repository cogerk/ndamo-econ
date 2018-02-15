anamx_NDAMO_AnMBR <- function(df){
  # Mass and energy balance for mainstream anaerobic membrane bioreactor 
  # with NDAMO and Anammox nitrogen removal system.
  # Kathryn Cogert 12/6/15


  # Loading Calculations
  LNin <- df$Flowrate * df$Nitrogen #kgN/d, Nitrogen load per day
  df$LN <- LNin  #kgN/d, total nitrogen load
  df$LCOD <- df$Flowrate * df$Carbon #kgCOD/d, COD load per day
  temp <- df  
  
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
  temp$CO2.DAMO <- temp$LCH4_cons * sCO2_NDAMO * MW_CO2 / MW_CH4
  temp$px.Anamx<-fN_anamx * Y_anamx * temp$LN* n_conv  #kg/d, sludge produced from anammox
  temp$px.NDAMO<- Y_NDAMO * temp$LCH4_cons * n_conv / CH4_COD  #kg/d, sludge produced from NDAMO
  
  # Mainstream Anaerobic Membrane Digester
  fCOD_AnMBR <- 1 # Assume 100% conversion of COD in AnMBR
  temp$px.AnMBR <- fCOD_AnMBR * temp$LCOD * Y_AnMBR * n_conv # kg/d
  temp$CH4prod <- fCOD_AnMBR * temp$LCOD * (1-Y_AnMBR) * CH4_COD
  temp$V.biogas <- temp$CH4prod / rho_CH4.main / x_biogas_CH4
  temp$V.CO2.digester <- temp$V.biogas * (1 - x_biogas_CH4) # assume balance of biogas is CO2
  temp$CO2.digester <- temp$V.CO2.digester / V.molgas.main * MW_CO2
  
  # Dissolved vs. Gaseous Methane
  cCH4dis <- H * P * x_biogas_CH4 * MW_CH4 * 1000/1000  # Sat. Dissolved CH4 conc, kgCH4/m3
  temp$LCH4diss <- cCH4dis * df$Flowrate * 10^3 # kg Dissolved, kg/d
  temp$CH4burn <- temp$CH4prod - temp$LCH4diss # Dissolved methane not avail for regen, kg/d
  temp$LCH4diss[which(temp$CH4burn<0)] <- temp$CH4prod[which(temp$CH4burn<0)] # If very little methane produced, assume all dissolves.
  temp$CH4burn[which(temp$CH4burn<0)] <- 0 # Then no methane avail for energy regen
  temp$LCH4diss.NDAMO <- 0.5 * temp$LCH4diss # kg Dissolved, kg/d, half will be consumed by MOBs in nitrification reactor
  temp$LCH4diss.NIT <- 0.5 * temp$LCH4diss # kg Dissolved, half to aerobic nitrification reactor
  
  # Dissolved Methane in Nitrification Reactor
  CminCH4 <- 5  # mgCOD/L, minimum concentration at which CH4 will be oxidized
  x.CH4ox <- 0.9 # wt%, fraction of methane oxidized if oxidation occurs
  temp$CCH4diss.NIT <- (temp$LCH4diss.NIT/CH4_COD) / (temp$Flowrate * 0.5) # mgCOD/L, Concentration of methane to the nitrification reactor
  temp$CH4.MOB.ox <- 0
  temp$CH4.MOB.ox[which(temp$CCH4diss.NIT>CminCH4)] <- temp$LCH4diss.NIT[which(temp$CCH4diss.NIT>CminCH4)] * x.CH4ox # If methane concentration exceeds 5 mgCOD/L, 90% will be oxidized
  temp$CH4.fromNIT <- temp$LCH4diss.NIT
  temp$CH4.fromNIT <- temp$CH4.fromNIT - temp$CH4.MOB.ox # Residual dissolved methane
  temp$CO2.MOB <- 0
  temp$CO2.MOB <- temp$CH4.MOB.ox * sCO2_BURN # CO2 production by MOBs
  temp$O2.MOB <- temp$CH4.MOB.ox / CH4_COD # O2 demand by MOBs
  temp$px.MOB <- temp$CH4.MOB.ox / CH4_COD *  Y_MOB * n_conv # Sludge Production from 
  
  # Methane Addition for NDAMO/Methane Production for Energy Regeneration
  temp$LCH4 <- temp$LCH4diss.NDAMO - temp$LCH4_cons # kg Dissolved after NDAMO consume, kg/d
  temp$CH4burn[which(temp$LCH4<0)] <- temp$CH4burn[which(temp$LCH4<0)] + temp$LCH4[which(temp$LCH4<0)] # if need more than dissolved, take it from CH4 gas
  temp$LCH4[which(temp$LCH4<0)] <- 0 # if need more than dissolved, dissolved CH4 = 0
  temp$COD.added <- 0
  temp$COD.added[which(temp$CH4burn<0)] <- -temp$CH4burn[which(temp$CH4burn<0)] / CH4_COD  # if need more than produced, get externally
  temp$CH4burn[which(temp$CH4burn<0)] <- 0 # if need more than produced, prodCH4  = 0
  temp$CO2.burn <- temp$CH4burn * sCO2_BURN * MW_CO2 / MW_CH4 # CO2 from energy regeneration  
  
  # Summary
  temp$px.TOT <- rowSums(select(temp, starts_with('px'))) #kg/d, total sludge produced
  temp$O2.TOT <- rowSums(select(temp, starts_with('O2'))) # Total stoichiometric O2 Demand
  temp$CO2.TOT <-  rowSums(select(temp, starts_with('CO2.'))) + (temp$LCH4 + temp$CH4.fromNIT) * CO2eq_CH4
  
  df$scenario <- rep('D', times=nrow(temp))
  df$COD.added <- temp$COD.added
  df$sludge.out <- temp$px.TOT
  df$O2.demand <- temp$O2.TOT
  df$CH4.dissolved <- temp$LCH4
  df$CH4.burn <- temp$CH4burn
  df$CO2.equivs  <- temp$CO2.TOT
  return(df)
}
