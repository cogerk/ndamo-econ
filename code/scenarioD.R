


anamx_NDAMO_AnMBR <- function(df){
  # Mass and energy balance for mainstream anaerobic membrane bioreactor 
  # with NDAMO and Anammox nitrogen removal system.
  # Kathryn Cogert 12/6/15
  # Updated 8/5/16
  temp <- df
  
  # Nitrification
  fN_AOB <- 0.5 # wt%, fraction of total N in converted by AOB, see supplemental calculations
  fN_NOB <- fN_AOB
  temp$O2.AOB <- (fN_AOB * temp$LN) / MW_N * MW_O2 * sO2_AOB # kg/D O2 req'd by AOB
  temp$O2.NOB <- (fN_NOB * temp$LN) / MW_N * MW_O2 * sO2_NOB # kg/D O2 req'd by NOB
  temp$px.AOB<-fN_AOB * fx_AOB * n_conv * temp$LN #kg/d, sludge produced from AOB
  temp$px.NOB<-fN_NOB * fx_NOB* n_conv* temp$LN #kg/d, sludge produced from NOB
  
  # Anammox/NDAMO
  fN_anamx <- 1 # wt%, fraction of N converted to N2 overall, see supplemental calculations
  fN_NDAMO <- 1.3 # wt%, frac of totN converted by NDAMO, see supplemental calculations
  temp$LCH4_cons <- fN_NDAMO * MW_CH4 / MW_N * sCH4_NDAMO * temp$LN # kgCH4/d, Methane consumed by NDAMO
  temp$CO2.DAMO <- temp$LCH4_cons * sCO2_NDAMO * MW_CO2 / MW_CH4
  temp$px.Anamx<-fN_anamx * fx_anamx * temp$LN* n_conv  #kg/d, sludge produced from anammox
  temp$px.NDAMO<-fN_NDAMO * fx_NDAMO * temp$LN * n_conv  #kg/d, sludge produced from NDAMO
  
  # Mainstream Anaerobic Membrane Digester
  fCOD_AnMBR <- 1 # Assume 100% conversion of COD in AnMBR
  temp$LCOD_conv <- fCOD_AnMBR * temp$LCOD # kgCOD/d, COD converted
  temp$px.AnMBR <- temp$LCOD_conv * fx_AnMBR * n_conv # kg/d
  temp$CH4prod <- temp$LCOD_conv * (1-fx_AnMBR) * CH4_COD
  temp$biogasvol <- temp$CH4prod / rho_CH4 / x_biogas_CH4
  temp$CO2vol.digester <- temp$biogasvol * (1 - x_biogas_CH4) # assume balance of biogas is CO2
  temp$CO2.digester <- temp$CO2vol.digester / vol.1molgas * MW_CO2
  
  # Dissolved vs. Gaseous Methane
  cCH4dis <- H * P * x_biogas_CH4 * MW_CH4 # Sat. Dissolved CH4 conc, kg/m3
  temp$LCH4diss <- cCH4dis * df$Flowrate * 10^3 # kg Dissolved, kg/d
  temp$CH4regen <- temp$CH4prod - temp$LCH4diss # Dissolved methane not avail for regen, kg/d
  temp$LCH4diss[which(temp$CH4regen<0)] <- temp$CH4prod[which(temp$CH4regen<0)] # If very little methane produced, assume all dissolves.
  temp$CH4regen[which(temp$CH4regen<0)] <- 0 # Then no methane avail for energy regen
  temp$LCH4diss <- 0.5 * temp$LCH4diss # kg Dissolved, kg/d, half will be consumed by MOBs in nitrification reactor
  temp$CO2.MOB <- 0.5 * temp$LCH4diss * MW_CO2 / MW_CH4 # CO2 production by MOBs, 
  temp$LCH4 <- temp$LCH4diss - temp$LCH4_cons # kg Dissolved after NDAMO consume, kg/d
  temp$CH4regen[which(temp$LCH4<0)] <- temp$CH4regen[which(temp$LCH4<0)] + temp$LCH4[which(temp$LCH4<0)] # if need more than dissolved, take it from CH4 gas
  temp$LCH4[which(temp$LCH4<0)] <- 0 # if need more than dissolved, dissolved CH4 = 0
  temp$COD.added <- 0
  temp$COD.added[which(temp$CH4regen<0)] <- -temp$CH4regen[which(temp$CH4regen<0)] / CH4_COD  # if need more than produced, get externally
  temp$CH4regen[which(temp$CH4regen<0)] <- 0 # if need more than produced, prodCH4  = 0
  temp$CO2.burn <- temp$CH4regen * sCO2_BURN * MW_CO2 / MW_CH4 # CO2 from energy regeneration  
  
  # Totalize
  temp$px.TOT <- rowSums(select(temp, starts_with('px'))) #kg/d, total sludge produced
  temp$O2.TOT <- rowSums(select(temp, starts_with('O2'))) # Total stoichiometric O2 Demand
  temp$CO2.TOT <-  rowSums(select(temp, starts_with('CO2.'))) + temp$LCH4 * CO2eq_CH4
  
  df$scenario <- rep('D', times=nrow(temp))
  df$COD.added <- temp$COD.added
  df$sludge.out <- temp$px.TOT
  df$O2.demand <- temp$O2.TOT
  df$CH4.dissolved <- temp$LCH4
  df$CH4.toburn <- temp$CH4regen
  df$CO2.equivs  <- temp$CO2.TOT
  return(df)
}
