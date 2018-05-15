anamx <- function(df){
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
  temp$CO2.HET <- temp$LCOD * sCO2_HET #CO2 produced

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
  
  
  # No AnMBR or NDAMO present
  # Therefore these lines are left blank
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # Anaerobic Digester
  temp$px.TOT <- rowSums(dplyr::select(temp, starts_with('px'))) #kg/d, total sludge produced
  temp$px.OUT <- temp$px.TOT * (1-fx_digester)
  temp$CH4prod <- (temp$px.TOT - temp$px.OUT)/ n_conv * CH4_COD
  temp$V.biogas <- temp$CH4prod / rho_CH4.dig / x_biogas_CH4
  temp$V.CO2.digester <- temp$V.biogas * (1 - x_biogas_CH4) # assume balance of biogas is CO2
  temp$CO2.digester <- temp$V.CO2.digester / V.molgas.digester * MW_CO2
  
  # No Methane addition for NDAMO Required, 
  # Therefore these lines are left blank
  
  
  
  
  # Methane Production for Energy Regeneration
  temp$CO2.burn <- temp$CH4prod * sCO2_BURN * MW_CO2 / MW_CH4
  
  # Summary
  temp$O2.TOT <- rowSums(select(temp, starts_with('O2'))) # Total stoichiometric O2 Demand
  temp$CO2.equivs <- rowSums(select(temp, starts_with('CO2.')))
  
  df$scenario <- rep('B', times=nrow(temp))
  df$COD.added <- rep(0, times=nrow(temp))
  df$sludge.out <- temp$px.OUT
  df$O2.demand <- temp$O2.TOT
  df$CH4.dissolved <- rep(0, times=nrow(temp))
  df$CH4.burn <- temp$CH4prod
  df$CO2.equivs  <- temp$CO2.equivs
  return(df)
  }

