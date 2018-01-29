anamx <- function(df){
# Mass and energy balance for an Anammox CANON system and High Rate BOD Removal
# By Kathryn Cogert
# Kathryn Cogert 12/6/15
# Last Updated 10/17/17 
  temp <- df
  
  # A Stage - High Rate BOD Removal
  fCOD_HET <- 1 # Assume 100% conversion of COD 
  temp$px.HET <- fCOD_HET * temp$LCOD * Y_HET * n_conv # Biomass produced/d
  temp$O2.HET <- fCOD_HET * temp$LCOD # O2 demand in A Stage
  temp$CO2.HET <- temp$LCOD * sCO2_HET #CO2 produced
  0.
  # Nitrification
  fN_AOB <- 1.3/2.3 # wt%, fraction of total N in converted by AOB, see appendix
  fN_NOB <- 0  # wt%, frac of totN converted by NOB, assumed 100% in anammox system # Make temp dependent?
  
  # Anammox
  fN_anamx <- 1 # wt%, fraction of N converted to N2 overall
  
  # Oxygen Demand/Sludge Handling
  temp$O2.AOB <- (fN_AOB * temp$LN) / MW_N * MW_O2 * sO2_AOB # kg/D O2 req'd by AOB
  temp$O2.NOB <- (fN_NOB * temp$LN) / MW_N * MW_O2 * sO2_NOB # kg/D O2 req'd by NOB
  temp$px.AOB <- fN_AOB * Y_AOB * n_conv * temp$LN #kg/d, sludge produced from AOB
  temp$px.NOB <- fN_NOB * Y_NOB * n_conv * temp$LN #kg/d, sludge produced from NOB
  temp$px.ANAMX <- fN_anamx * Y_anamx * temp$LN #kg/d, sludge produced from anammox
  temp$px.TOT <- rowSums(select(temp, starts_with('px'))) #kg/d, total sludge produced
  temp$O2.TOT <- rowSums(select(temp, starts_with('O2'))) # Total stoichiometric O2 Demand
 
  temp$COD_bal <- rep(0, times=nrow(temp))
  temp$LCH4 <- rep(0, times=nrow(temp))
  
  
  # Anaerobic Digester
  temp$px.OUT <- temp$px.TOT * (1-x_digester)
  temp$CH4prod <- (temp$px.TOT - temp$px.OUT)/ n_conv * CH4_COD
  temp$biogasvol <- temp$CH4prod / rho_CH4 / x_biogas_CH4
  temp$CO2vol.digester <- temp$biogasvol * (1 - x_biogas_CH4) # assume balance of biogas is CO2
  temp$CO2.digester <- temp$CO2vol.digester / vol.1molgas * MW_CO2
  temp$CO2.burn <- temp$CH4prod * sCO2_BURN * MW_CO2 / MW_CH4
  
  
  df$scenario <- rep('B', times=nrow(temp))
  df$COD.added <- 0
  df$sludge.out <- temp$px.OUT
  df$O2.demand <- temp$O2.TOT
  df$CH4.dissolved <- temp$LCH4
  df$CH4.toburn <- temp$CH4prod
  df$CO2.equivs  <- rowSums(select(temp, starts_with('CO2.'))) 
  return(df)
  }

