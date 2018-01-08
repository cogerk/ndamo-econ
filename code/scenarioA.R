MLE <- function(df) {
# Mass and energy balance for an MLE system
# By Kathryn Cogert
# Kathryn Cogert 12/6/15
# Last Updated 12/6/17 
  temp <- df
  
  # Nitrification
  fN_AOB <- 1 # wt%, fraction of total N in converted by AOB, see appendix , assumed 100% in MLE system
  fN_NOB <- fN_AOB  # wt%, frac of totN converted by NOB, assumed 100% in MLE system
  
  # Denitrification
  temp$COD_reqd <- temp$LN * sCOD_HET # ammt of COD req'd.
  temp$COD_bal <- temp$COD_reqd-temp$LCOD
  temp$px.DENIT <- fx_DENIT * temp$COD_reqd
  temp$COD_added <- 0
  temp$COD_added[which(temp$COD_bal>0)] <- temp$COD_bal[which(temp$COD_bal>0)]
  temp$O2.HET <- rep(0, times=nrow(temp))
  temp$O2.HET[which(temp$COD_bal<0)] <-  -temp$COD_bal[which(temp$COD_bal<0)]
  temp$CO2.HET <- (temp$LCOD + temp$COD_added) * sCO2_HET #CO2 produced
  
  # Oxygen Demand/Sludge Handling (Universal)
  temp$O2.AOB <- (fN_AOB * temp$LN) / MW_N * MW_O2 * sO2_AOB # kg/D O2 req'd by AOB
  temp$O2.NOB <- (fN_NOB * temp$LN) / MW_N * MW_O2 * sO2_NOB # kg/D O2 req'd by NOB
  temp$px.AOB <- fN_AOB * fx_AOB * n_conv * temp$LN #kg/d, sludge produced from AOB
  temp$px.NOB <- fN_NOB * fx_NOB * n_conv * temp$LN #kg/d, sludge produced from NOB
  temp$px.TOT <- rowSums(dplyr::select(temp, starts_with('px'))) #kg/d, total sludge produced
                     
  # Anaerobic Digester
  temp$px.OUT <- temp$px.TOT * (1-x_digester)
  temp$CH4prod <- (temp$px.TOT - temp$px.OUT)/ n_conv * CH4_COD
  temp$biogasvol <- temp$CH4prod / rho_CH4 / x_biogas_CH4
  temp$CO2vol.digester <- temp$biogasvol * (1 - fbiogas_CH4) # assume balance of biogas is CO2
  temp$CO2.digester <- temp$CO2vol.digester / vol.1molgas * MW_CO2
  temp$CO2.burn <- temp$CH4prod * sCO2_BURN
  
  # No CH4 in effluent in scenario A
  temp$LCH4 <- rep(0, times=nrow(temp))
  
  # Total stoichiometric O2 Demand
  temp$O2.TOT <- rowSums(select(temp, starts_with('O2'))) 
  
  # Return parameters of interest
  df$scenario <- rep('A', times=nrow(temp))
  df$COD.added <- temp$COD_added
  df$sludge.out <- temp$px.OUT
  df$O2.demand <- temp$O2.TOT
  df$CH4.dissolved <- temp$LCH4
  df$CH4.toburn <- temp$CH4prod
  df$CO2.equivs  <- rowSums(select(temp, starts_with('CO2.'))) 
  return(df)
  }

