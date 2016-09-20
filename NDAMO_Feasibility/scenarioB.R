anamx<- function(V, pH, Temp, cNin, cCODin){

  # Mass and energy balance for mainstream anaerobic membrane bioreactor 
  # with NDAMO and Anammox nitrogen removal system.
  # Takes input wastewater concentrations and returns 
    # 1) Oxygen Requirements, 
    # 2) Sludge Production,
    # 3) Methane production
    # 4) % Dissolved Methane Consumed
    # 4) Alkalinity req'mts?
  # Kathryn Cogert 12/6/15
  # Updated 8/5/16
  
  # Input Parameters
  # V, ML/day, influent flowrate
  # pH, system pH
  # Temp, deg C, influent temperature
  # cNin, mg/l, concentration of nitrogen in influent
  # cCODin, mg/l, concentration of COD in influent
  
  # TODO: Include temperature and pH effects
  # TODO: Greenhouse gas production? Where to draw the box?
  
  ### Constants
  MW_O2 = 32
  MW_CH4 = 16
  MW_N = 14
  CH4_COD = 4
  ## Methane Production
  rho_CH4 = 0.65477
  n_BG = 0.95 #Biogas Burning Eff.
  n_Cogen = 0.39 #Cogen Efficiency
  H_CH4 = 23 #MJ/m3, methane caloric potential
  
  # Nitrogen
  sO2_AOB = 1.5 # Oxygen stoich coeff AOB 
  sO2_NOB = 0.5 # Oxygen stoich coeff NOB 
  
  # Sludge production Factors
  fx_AnMBR = 0.14  # gCOD/gCOD, gCOD of sludge produced in AnMBR per gCOD eaten, Yeo, 2015 (Confirm)
  fx_HET = 0.56    # gCOD/gCOD, gCOD of sludge produced per gCOD eaten by heterotrophs, Citation needed
  fx_AOB = 0.20    # gCOD/gN, gCOD of sludge produced per gN eaten by AOB, Wiesmann, 1994
  fx_NOB = 0.06    # gCOD/gN, gCOD of sludge produced per gN eaten by NOB, Wiesmann, 1994 T=20C pH=8
  fx_anamx = 0.17  # gCOD/gN, gCOD of sludge produced per gN eaten by anammox.  Van der Starr
  fx_NDAMO = 0.22  # gCOD/gCH4,	gCOD of sludge produced per gCH4 eaten by NDAMO.  From Metabolic reaction
  n = 1.4          # gVSS/gCOD, conversion constant
  #fSolids=0.1    # gVSS/gSludge, fraction of solids in final sludge from plant
  
  ## Nitrogen system
  fN_AOB = 0.57 # wt%, fraction of total N in converted by AOB, see appendix
  
  ## AnMBR System
  MixingEnergy = 0.15 #kWh/m3, Mixing energy of reactor calculated per reactor volume, Damien Batstone
  HRT_AnMBR = 1 #days, Damien Batstone
  MembraneScouringEnergy=0.25 #kWh/kL/d, scouring energy per flowrate, Damien Batstone
  
  ## Cost Factors - Commented out b/c we are going to no cost model
  #C_Elect_Seattle=0.02; %$/kWh
  #C_SHARONAerationpN=1.38; %$/kgN
  #C_SludgeTreatment=100; %$/metric ton
  
  ### Calcuations
  LNin = V * cNin #kgN/d, Nitrogen load per day
  LNcent = .4 * LNin #kgN/d, Nitrogen load from centrate, assumed 40% of total load
  LN = LNin + LNcent #kgN/d, total nitrogen load
  LCOD_inf = V * cCODin #kgCOD/d, COD load per day
  
  #A Stage - High Rate BOD Removal
  # Total Methane Produced in mainstream digester
  fCOD_Astage = 1 # Assume 100% conversion of COD in AnMBR
  LCOD_conv = fCOD_Astage * LCOD_inf # kgCOD/d, COD converted
  px_Astage = LCOD_conv * fx_HET # Biomass produced/d
  O2_Astage = LCOD_conv # O2 demand in A Stage
  
  # Nitrogen Removal System
  fN_NOB = 0  # wt%, frac of totN converted by NOB, assumed 100% in NDAMO system
  O2_AOB = (fN_AOB * LNin) / MW_N * MW_O2 * sO2_AOB # kg/D O2 req'd by AOB
  O2_NOB = (fN_NOB * LNin) / MW_N * MW_O2 * sO2_NOB # kg/D O2 req'd by NOB
  O2_dem = O2_AOB + O2_NOB + O2_Astage # Total stoichiometric O2 Demand
  fN_anamx = (1 - fN_AOB) * 2 * 1.02 # wt%, fraction of N converted to N2 overall
    
  
  #Slude Produced
  px_AOB = fN_AOB*fx_AOB*n*LN #kg/d, sludge produced from AOB
  px_NOB = fN_NOB*fx_NOB*n*LN #kg/d, sludge produced from NOB
  px_Anamx = fN_anamx*fx_anamx*LN #kg/d, sludge produced from anammox
  px_TOT = px_Astage+px_AOB+px_NOB+px_Anamx #kg/d, total sludge produced
  
  # Anaerobic Digester
  #LCH4dis_eff=(cCH4dis*V-LCH4_cons)/V; %mg/L, methane in effluent
  fx_digester = 0.59 # Assumed digester sludge conversion
  px_TOT = px_TOT * (1-fx_digester)
  CH4prod_COD = px_TOT * fx_digester # kg CH4 produced as COD/day
  CH4prod = CH4prod_COD * CH4_COD # kg CH4 produced/day
  fCH4gas = 0.62 # Assume 62% of biogas is methane
  P=1 # atm, system pressure
  H=0.0015 # Henry's Constant, (Add temperature and pH dependence)
  CH4gas = CH4prod # Assume all Methane goes to gas phase
  CO2gas = CH4gas/fCH4gas*(1-fCH4gas) # Assume balance of biogas is CO2
  LCH4gas = CH4gas/rho_CH4 #m3/d, volume methane produced
  
  Results= data.frame(V, pH, Temp, cNin, cCODin, px_TOT, O2_dem, LCH4gas)
  return(Results)
  }

