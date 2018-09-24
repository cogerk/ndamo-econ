MLE <- function(df) {
# Mass and energy balance for an MLE system
# By Kathryn Cogert
# Kathryn Cogert 12/6/15
  
  
  # Loading Calculations
  df$LCOD <- df$Flowrate * df$Carbon #kgCOD/d, COD load per day
  LNin <- df$Flowrate * df$Nitrogen #kgN/d, Nitrogen load per day
  LNcent <- N_cent * LNin #kgN/d, Nitrogen load from centrate, assumed 40% of total load
  df$LN <- LNin + LNcent #kgN/d, total nitrogen load
  temp <- df
  
  # No HRAS System present
  # Therefore, these lines are left blank
  
  
  
  # Nitrification
  fN_AOB <- 1 # wt%, fraction of total N in converted by AOB, see appendix , assumed 100% in MLE system
  fN_NOB <- fN_AOB  # wt%, frac of totN converted by NOB, assumed 100% in MLE system
  temp$O2.AOB <- (fN_AOB * temp$LN) / MW_N * MW_O2 * sO2_AOB # kg/D O2 req'd by AOB
  temp$O2.NOB <- (fN_NOB * temp$LN) / MW_N * MW_O2 * sO2_NOB # kg/D O2 req'd by NOB
  temp$px.AOB <- fN_AOB * Y_AOB * n_conv * temp$LN #kg/d, sludge produced from AOB
  temp$px.NOB <- fN_NOB * Y_NOB * n_conv * temp$LN #kg/d, sludge produced from NOB
  
  # Denitrification
  fN_Denit <- 1
  temp$COD_reqd <- temp$LN * sCOD_DENIT *   fN_Denit # ammt of COD req'd.
  temp$px.DENIT <- Y_DENIT * temp$COD_reqd * n_conv
  temp$COD_bal <- temp$COD_reqd-temp$LCOD
  temp$COD_added <- 0
  temp$COD_added[which(temp$COD_bal>0)] <- temp$COD_bal[which(temp$COD_bal>0)]
  # If not all COD is denitrified, oxidize the rest with heterotrophs.
  temp$O2.HET <- rep(0, times=nrow(temp))
  temp$O2.HET[which(temp$COD_bal<0)] <-  -temp$COD_bal[which(temp$COD_bal<0)]
  temp$px.HET <- 0 
  temp$px.HET[which(temp$COD_bal<0)] <-  -temp$COD_bal[which(temp$COD_bal<0)] * n_conv * Y_HET
                     
  # No AnMBR system present
  # Therefore, these lines are left blank
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # Anaerobic Digester
  temp$px.TOT <- rowSums(select(temp, starts_with('px'))) #kg/d, total sludge produced
  temp$px.OUT <- temp$px.TOT * (1-fx_AD)
  temp$CH4prod.AD <- (temp$px.TOT - temp$px.OUT)/ n_conv * CH4_COD
  
  
  # No Methane addition for NDAMO Required, 
  # Therefore these lines are left blank
  
  
  
  
  
  
  # Total stoichiometric O2 Demand
  temp$O2.TOT <- rowSums(select(temp, starts_with('O2'))) 
  
  # Electrical Demand
  temp$E.O2 <- temp$O2.TOT * e_O2
  temp$E.SludgeThicken <- temp$px.OUT * e_SludgeThicken
  
  temp$E.CHP <- -temp$CH4prod.AD * H_c_CH4 * n.CHP
  temp$CO2 <- rowSums(select(temp, starts_with('E.'))) * kgCO2.kWh
  
  # Summary
  df$scenario <- rep('A', times=nrow(temp))
  df$COD.added <- temp$COD_added
  df$sludge.out <- temp$px.OUT
  df$O2.demand <- temp$O2.TOT
  df$CH4.burn <- temp$CH4prod.AD
  df$CO2.equivs  <- temp$CO2
  return(df)
}
