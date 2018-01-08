anamx_NDAMO<- function(df){
# Mass and energy balance for anammox/NDAMO system
# Kathryn Cogert 12/6/15
# Last Updated 10/22/17
  temp <- df
  
  # A Stage - High Rate BOD Removal
  fCOD_HET <- 1 # Assume 100% conversion of COD 
  temp$px.HET <- fCOD_HET * temp$LCOD * fx_HET * n_conv # Biomass produced/d
  temp$O2.HET <- fCOD_HET * temp$LCOD # O2 demand in A Stage
  temp$CO2.HET <- temp$LCOD * sCO2_HET #CO2 produced
  
  # Nitrification
  fN_AOB <- 1 - 0.485 # wt%, fraction of total N in converted by AOB, see appendix
  fN_NOB <- fN_AOB
  
  # Anammox/NDAMO
  fN_anamx <- (1 - fN_AOB) * 2 * 1.02 # wt%, fraction of N converted to N2 overall
  fN_NDAMO <- 1.32 - .32 * fN_AOB # wt%, frac of totN converted by NDAMO, See appendix
  temp$LCH4_cons <- temp$LN * fN_NDAMO / MW_N * sCH4_NDAMO * MW_CH4  # kgCH4/d, Methane consumed by NDAMO
  temp$CO2.DAMO <- temp$LCH4_cons * sCO2_NDAMO 
  
  # Oxygen Demand/Sludge Handling
  temp$O2.AOB <- (fN_AOB * temp$LN) / MW_N * MW_O2 * sO2_AOB # kg/D O2 req'd by AOB
  temp$O2.NOB <- (fN_NOB * temp$LN) / MW_N * MW_O2 * sO2_NOB # kg/D O2 req'd by NOB
  temp$px.AOB <- fN_AOB * fx_AOB * n_conv * temp$LN #kg/d, sludge produced from AOB
  temp$px.NOB <- fN_NOB * fx_NOB * n_conv * temp$LN #kg/d, sludge produced from NOB
  temp$px.ANAMX <- fN_anamx * fx_anamx * temp$LN #kg/d, sludge produced from anammox
  temp$px.NDAMO <- fN_NDAMO * fx_NDAMO * temp$LN #kg/d, sludge produced from NDAMO
  
  # Totalize
  temp$px.TOT <- rowSums(select(temp, starts_with('px'))) #kg/d, total sludge produced
  temp$O2.TOT <- rowSums(select(temp, starts_with('O2'))) # Total stoichiometric O2 Demand
  
  # Anaerobic Digester
  temp$px.OUT <- temp$px.TOT * (1-sx_digester)
  temp$CH4prod <- (temp$px.TOT - temp$px.OUT)/ n_conv * CH4_COD
  temp$LCH4 <- rep(0, times=nrow(temp))
  temp$biogasvol <- temp$CH4prod / rho_CH4 / x_biogas_CH4
  temp$CO2vol.digester <- temp$biogasvol * (1 - fbiogas_CH4) # assume balance of biogas is CO2
  temp$CO2.digester <- temp$CO2vol.digester / vol.1molgas * MW_CO2
  
  # Methane Balance
  temp$CH4regen <- temp$CH4prod - temp$LCH4_cons # kg Dissolved after NDAMO consume, kg/d
  temp$COD.added <- 0
  temp$COD.added[which(temp$CH4regen<0)] <- -temp$CH4regen[which(temp$CH4regen<0)] / CH4_COD  # if need more than produced, get externally
  temp$CH4regen[which(temp$CH4regen<0)] <- 0 # if need more than produced, prodCH4  = 0
  temp$CO2.burn <- temp$CH4regen * sCO2_BURN # CO2 from energy regeneration
  
  
  df$scenario <- rep('C', times=nrow(temp))
  df$COD.added <- temp$COD.added
  df$sludge.out <- temp$px.OUT
  df$O2.demand <- temp$O2.TOT
  df$CH4.dissolved <- temp$LCH4
  df$CH4.toburn <- temp$CH4regen
  df$CO2.equivs  <- rowSums(select(temp, starts_with('CO2.'))) 
  return(df)
  }
