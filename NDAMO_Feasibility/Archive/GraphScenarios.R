delN <- 10#.25
delC <- 10#3
N_vector <- seq(5, 50, delN)
COD_vector <- seq(50, 300, delC)
flowrate = 20 # In Million Liters/Day
pH = 7 # No effect currently
Temp = 20 # No effect currently


for(i in N_vector){
  for(j in COD_vector){
    D_New_Results = anamx_NDAMO_AnMBR(flowrate, pH, Temp, i, j)
    C_New_Results = anamx_NDAMO(flowrate, pH, Temp, i, j)
    B_New_Results = anamx(flowrate, pH, Temp, i, j)
    if(j == COD_vector[1] & i == N_vector[1]) {
      D_Results = D_New_Results
      C_Results = C_New_Results
      B_Results = B_New_Results
    } else {
      D_Results <- rbind(D_Results, D_New_Results)
      C_Results <- rbind(C_Results, C_New_Results)
      B_Results <- rbind(B_Results, B_New_Results)
    }
  }
}

D_CH4_Cons <- matrix(D_Results$fCH4_cons*100, length(COD_vector),length(N_vector))
D_px_TOT <- matrix(D_Results$px_TOT/1000, length(COD_vector),length(N_vector))
D_O2_dem <- matrix(D_Results$O2_dem/1000, length(COD_vector),length(N_vector))
D_LCH4gas <-matrix(D_Results$LCH4gas/1000, length(COD_vector),length(N_vector))

C_CH4_Cons <- matrix(C_Results$fCH4_cons*100, length(COD_vector),length(N_vector))
C_px_TOT <- matrix(C_Results$px_TOT/1000, length(COD_vector),length(N_vector))
C_O2_dem <- matrix(C_Results$O2_dem/1000, length(COD_vector),length(N_vector))
C_LCH4gas <-matrix(C_Results$LCH4gas/1000, length(COD_vector),length(N_vector))

B_px_TOT <- matrix(C_Results$px_TOT/1000, length(COD_vector),length(N_vector))
B_O2_dem <- matrix(C_Results$O2_dem/1000, length(COD_vector),length(N_vector))
B_LCH4gas <-matrix(C_Results$LCH4gas/1000, length(COD_vector),length(N_vector))

get_zrange <- function(m){
  if (m=='CH4_Cons'){
    maxval=max(D_CH4_Cons, C_CH4_Cons)
    minval=min(D_CH4_Cons, C_CH4_Cons)
  } else if (m=='px_TOT'){
    maxval=max(D_px_TOT, C_px_TOT, B_px_TOT)
    minval=min(D_px_TOT, C_px_TOT, B_px_TOT)
  } else if (m=='O2_dem'){
    maxval=max(D_O2_dem, C_O2_dem, B_O2_dem)
    minval=min(D_O2_dem, C_O2_dem, B_O2_dem)
  } else if (m=='LCH4gas'){
    maxval=max(D_LCH4gas, C_LCH4gas, B_LCH4gas)
    minval=min(D_LCH4gas, C_LCH4gas, B_LCH4gas)
  }
  # Fix scale
  #Implement scenario A
  zrange = c(minval, maxval)
  return(zrange)
}


get_cbar <- function(P){
  if (P){
    suffix='%'
  }
  else{
    suffix=''
  }
  cbar <- list(
    #tickvals=signif(seq(min(minval), max(maxval), length.out = 8),3), 
    ticksuffix=suffix,
    title = "")
  return(cbar)
}

library(plotly)
f <- list(
  family = "Courier New, monospace",
  size = 18,
  color = "#7f7f7f"
)
x <- list(
  title = "Influent Nitrogen Concentration, mgN/L"
)
y <- list(
  title = "Influent COD Concentration, mg/L"
)

pD_CH4_cons <- plot_ly(z = D_CH4_Cons, x=N_vector, y=COD_vector, type = "heatmap", colorbar = get_cbar(TRUE)) %>%
  layout(xaxis = x, yaxis = y, zaxis =list(range=get_zrange('CH4_Cons')), title='Scenario D: % CH4 Consumed by NDAMO')
print(pD_CH4_cons)
pD_px_tot <- plot_ly(z = D_px_TOT, x=N_vector, y=COD_vector, type = "heatmap", colorbar = get_cbar(FALSE)) %>%
  layout(xaxis = x, yaxis = y, zaxis =list(range=get_zrange('px_TOT')), title='Scenario D: Metric Tons Sludge Produced per Year')
print(pD_px_tot)
pD_O2_dem <- plot_ly(z = D_O2_dem, x=N_vector, y=COD_vector, type = "heatmap", colorbar = get_cbar(FALSE)) %>%
  layout(xaxis = x, yaxis = y, zaxis =list(range=get_zrange('O2_dem')), title='Scenario D: Annual O2 Demand in Metric Tons')
print(pD_O2_dem)
pD_Methane <- plot_ly(z = D_LCH4gas, x=N_vector, y=COD_vector, type = "heatmap", colorbar = get_cbar(FALSE)) %>%
  layout(xaxis = x, yaxis = y, zaxis =list(range=get_zrange('LCH4gas')), title='Scenario D: Annual Methane for Energy Recovery in Metric Tons')
print(pD_Methane)


#
pC_CH4_cons <- plot_ly(z = C_CH4_Cons, x=N_vector, y=COD_vector, type = "heatmap", colorbar = get_cbar(TRUE)) %>%
  layout(xaxis = x, yaxis = y, zaxis =list(range=get_zrange('CH4_Cons')), title='Scenario C: % CH4 Consumed by NDAMO')
print(pC_CH4_cons)
pC_px_tot <- plot_ly(z = C_px_TOT, x=N_vector, y=COD_vector, type = "heatmap", colorbar = get_cbar(FALSE)) %>%
  layout(xaxis = x, yaxis = y, zaxis =list(range=get_zrange('px_TOT')), title='Scenario C: Metric Tons Sludge Produced per Year')
print(pC_px_tot)
pC_O2_dem <- plot_ly(z = C_O2_dem, x=N_vector, y=COD_vector, type = "heatmap", colorbar = get_cbar(FALSE)) %>%
  layout(xaxis = x, yaxis = y, zaxis =list(range=get_zrange('O2_dem')), title='Scenario C: Annual O2 Demand in Metric Tons')
print(pC_O2_dem)
pC_Methane <- plot_ly(z = C_LCH4gas, x=N_vector, y=COD_vector, type = "heatmap", colorbar = get_cbar(FALSE)) %>%
  layout(xaxis = x, yaxis = y, zaxis =list(range=get_zrange('LCH4gas')), title='Scenario C: Annual Methane for Energy Recovery in Metric Tons')
print(pC_Methane)


#
pB_px_tot <- plot_ly(z = B_px_TOT, x=N_vector, y=COD_vector, type = "heatmap", colorbar = get_cbar(FALSE)) %>%
  layout(xaxis = x, yaxis = y, zaxis =list(range=get_zrange('px_TOT')), title='Scenario B: Metric Tons Sludge Produced per Year')
print(pB_px_tot)
pB_O2_dem <- plot_ly(z = B_O2_dem, x=N_vector, y=COD_vector, type = "heatmap", colorbar = get_cbar(FALSE)) %>%
  layout(xaxis = x, yaxis = y, zaxis =list(range=get_zrange('O2_dem')), title='Scenario B: Annual O2 Demand in Metric Tons')
print(pB_O2_dem)
pB_Methane <- plot_ly(z = B_LCH4gas, x=N_vector, y=COD_vector, type = "heatmap", colorbar = get_cbar(FALSE)) %>%
  layout(xaxis = x, yaxis = y, zaxis =list(range=get_zrange('LCH4gas')), title='Scenario B: Annual Methane for Energy Recovery in Metric Tons')
print(pB_Methane)