nit_denit<- function(V, pH, Temp, cNin, cCODin){


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
  fx_HET = 0.56    # gCOD/gCOD, gCOD of sludge produced per gCOD eaten by heterotrophs, WWT book
  fx_AOB = 0.20    # gCOD/gN, gCOD of sludge produced per gN eaten by AOB, Wiesmann, 1994
  fx_NOB = 0.06    # gCOD/gN, gCOD of sludge produced per gN eaten by NOB, Wiesmann, 1994 T=20C pH=8
  fx_anamx = 0.17  # gCOD/gN, gCOD of sludge produced per gN eaten by anammox.  Van der Starr
  fx_NDAMO = 0.22  # gCOD/gCH4,	gCOD of sludge produced per gCH4 eaten by NDAMO.  From Metabolic reaction
  n = 1.4          # gVSS/gCOD, conversion constant
  fCOD_HET = 5     # gCOD/gN, gCOD required per gN eaten by denitrifiers.  From Metabolic reaction
  f_CH4_COD = 0.40 # Methane gas production at 35 C, m3 CH4/kgCOD
  fbiogas_CH4 = 0.62 # Typical concentration of methane in biogas
  #fSolids=0.1    # gVSS/gSludge, fraction of solids in final sludge from plant
  
  ## Nitrogen system
  fN_AOB = 1 # wt%, fraction of total N in converted by AOB, see appendix
  
  ## AnMBR System
  MixingEnergy = 0.15 #kWh/m3, Mixing energy of reactor calculated per reactor volume, Damien Batstone
  HRT_AnMBR = 1 #days, Damien Batstone
  MembraneScouringEnergy=0.25 #kWh/kL/d, scouring energy per flowrate, Damien Batstone
  
  ## Cost Factors - Commented out b/c we are going to no cost model
  #C_Elect_Seattle=0.02; %$/kWh
  #C_SHARONAerationpN=1.38; %$/kgN
  #C_SludgeTreatment=100; %$/metric ton
  
  ### Calcuations
  # Make this universal
  LNin = V * cNin #kgN/d, Nitrogen load per day
  LNcent = .4 * LNin #kgN/d, Nitrogen load from centrate, assumed 40% of total load
  Vcent = 0.01 * V #ML/d, centrate per day
  LN = LNin + LNcent #kgN/d, total nitrogen load
  LCODin = V * cCODin #kgCOD/d, COD load per day
  LCOD = LCODin
  
  # Nitrogen Removal System
  # Convert to matrix format
  fN_NOB = fN_AOB  # wt%, frac of totN converted by NOB, assumed 100% in NDAMO system
  O2_AOB = (fN_AOB * LN) / MW_N * MW_O2 * sO2_AOB # kg/D O2 req'd by AOB
  O2_NOB = (fN_NOB * LN) / MW_N * MW_O2 * sO2_NOB # kg/D O2 req'd by NOB
  px_AOB = fN_AOB*fx_AOB*n*LN #kg/d, sludge produced from AOB
  px_NOB = fN_NOB*fx_NOB*n*LN #kg/d, sludge produced from NOB
  

  # Denitrification
  COD_reqd = LN * fCOD_HET # ammt of COD req'd.
  COD_added = max(0,COD_reqd-LCOD)

  px_HET = (LCOD + COD_added)*n*fx_HET #Slude produced

  O2_dem = O2_AOB + O2_NOB # Total stoichiometric O2 Demand
  
  #Slude Produced
  px_TOT = px_HET+px_AOB+px_NOB #kg/d, total sludge produced
  
  
  # Anaerobic Digester
  #LCH4dis_eff=(cCH4dis*V-LCH4_cons)/V; %mg/L, methane in effluent
  fx_digester = 0.59 # Assumed digester sludge conversion
  px_out = px_TOT * (1-fx_digester)
  CH4prod = (px_TOT-px_out)/1.4 * f_CH4_COD - #m3 CH4 produced/day calculated as a mass balance
  CO2prod = CH4prod/fbiogas_CH4*(1-fbiogas_CH4) # Assume balance of biogas is CO2
  
  
  Results= data.frame(V, pH, Temp, cNin, cCODin, COD_added, px_out, O2_dem, LCH4)
  return(Results)
  }

