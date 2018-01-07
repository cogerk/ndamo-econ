setwd("/Users/kathryncogert/Reference/ndamo-econ/NDAMO_Feasibility/Images")
Nmax <- 50
CODmax <- 300
rough_del <- 10
fine_del <- 150
N_vector <- seq(Nmax/rough_del, Nmax, Nmax/(fine_del-1))
COD_vector <- seq(CODmax/rough_del, CODmax, CODmax/(fine_del-1))
flowrate <- 20 # In Million Liters/Day
pH <- 7 # No effect currently
Temp <- 20 # No effect currently


for(i in N_vector){
  cat('Percent Complete = ',i/Nmax*100,'%\n')
  for(j in COD_vector){
    D_New_Results = anamx_NDAMO_AnMBR(flowrate, pH, Temp, i, j)
    C_New_Results = anamx_NDAMO(flowrate, pH, Temp, i, j)
    B_New_Results = anamx(flowrate, pH, Temp, i, j)
    A_New_Results = nit_denit(flowrate, pH, Temp, i, j)
    if(j == COD_vector[1] & i == N_vector[1]) {
      D_Results <- D_New_Results
      C_Results <- C_New_Results
      B_Results <- B_New_Results
      A_Results <- A_New_Results
    } else {
      D_Results <- rbind(D_Results, D_New_Results)
      C_Results <- rbind(C_Results, C_New_Results)
      B_Results <- rbind(B_Results, B_New_Results)
      A_Results <- rbind(A_Results, A_New_Results)
      }
  }
}

D_CH4_Cons <- matrix(D_Results$fCH4_cons*100, length(COD_vector),length(N_vector))

D_px_TOT <- matrix((D_Results$px_TOT-A_Results$px_TOT)/A_Results$px_TOT*100, length(COD_vector),length(N_vector))
D_O2_dem <- matrix((D_Results$O2_dem-A_Results$O2_dem)/A_Results$O2_dem*100, length(COD_vector),length(N_vector))
D_LCH4gas <-matrix((D_Results$LCH4gas-A_Results$LCH4gas)/A_Results$LCH4gas*100, length(COD_vector),length(N_vector))

C_CH4_Cons <- matrix(C_Results$fCH4_cons*100, length(COD_vector),length(N_vector))

C_px_TOT <- matrix((C_Results$px_TOT-A_Results$px_TOT)/A_Results$px_TOT*100, length(COD_vector),length(N_vector))
C_O2_dem <- matrix((C_Results$O2_dem-A_Results$O2_dem)/A_Results$O2_dem*100, length(COD_vector),length(N_vector))
C_LCH4gas <-matrix((C_Results$LCH4gas-A_Results$LCH4gas)/A_Results$LCH4gas*100, length(COD_vector),length(N_vector))

B_px_TOT <- matrix((B_Results$px_TOT-A_Results$px_TOT)/A_Results$px_TOT*100, length(COD_vector),length(N_vector))
B_O2_dem <- matrix((B_Results$O2_dem-A_Results$O2_dem)/A_Results$O2_dem*100, length(COD_vector),length(N_vector))
B_LCH4gas <-matrix((B_Results$LCH4gas-A_Results$LCH4gas)/A_Results$LCH4gas*100, length(COD_vector),length(N_vector))

A_px_TOT <- matrix(A_Results$px_TOT/1000, length(COD_vector),length(N_vector))
A_O2_dem <- matrix(A_Results$O2_dem/1000, length(COD_vector),length(N_vector))
A_LCH4gas <-matrix(A_Results$LCH4gas/1000, length(COD_vector),length(N_vector))

A_COD_added <- matrix(A_Results$COD_added, length(COD_vector),length(N_vector))

D_CH4_added <-matrix(-D_Results$LCH4gas/4, length(COD_vector),length(N_vector))
D_CH4_added[D_CH4_added<0]=0
D_CH4_added_compare <- matrix((D_CH4_added-A_Results$LCH4gas)/A_Results$LCH4gas*100, length(COD_vector),length(N_vector))


get_cscale <- function(ident){
  suf <- '%'
  if (ident=='CH4_Cons'){
    C_max <- max(C_CH4_Cons)
    C_min <- min(C_CH4_Cons)
    D_max <- max(D_CH4_Cons)
    D_min <- min(D_CH4_Cons)
    zminval <- min(C_min, D_min)
    zmaxval <- max(C_max, D_max)
    } else if (ident=='px_TOT'){
    A_min <- min(A_px_TOT)
    B_min <- min(B_px_TOT)
    C_min <- min(C_px_TOT)
    D_min <- min(D_px_TOT)
    zminval <- min(A_min, B_min, C_min, D_min)
    zmaxval <- -zminval
  } else if (ident=='O2_dem'){
    A_min <- min(A_O2_dem)
    B_min <- min(B_O2_dem)
    C_min <- min(C_O2_dem)
    D_min <- min(D_O2_dem)
    zminval <- min(A_min, B_min, C_min, D_min)
    zmaxval <- -zminval
  } else if (ident=='LCH4gas'){
    A_min <- min(A_LCH4gas)
    B_min <- min(B_LCH4gas)
    C_min <- min(C_LCH4gas)
    D_min <- min(D_LCH4gas)
    A_max <- max(A_LCH4gas)
    B_max <- max(B_LCH4gas)
    C_max <- max(C_LCH4gas)
    D_max <- max(D_LCH4gas)
    zminval <- min(A_min, B_min, C_min, D_min)
    zmaxval <- -zminval
  } else if (ident=='COD_added'){
    suf <- ''
    A_min <- min(A_COD_added)
    B_min <- A_min
    C_min <- A_min
    D_min <- A_min
    zminval <- min(A_min, B_min, C_min, D_min)
    zmaxval <- -zminval
  }
    return(list(zminval,zmaxval,suf))
}

Sys.setenv("plotly_username" = "cogerk")
Sys.setenv("plotly_api_key" = "w74yv3t5o7")
library(plotly)

f <- list(
  family="Times New Roman",
  size = 18
)
x <- list(
  title = "Influent Nitrogen Concentration, mgN/L",
  titlefont = f,
  tickfont = f
)
y <- list(
  title = "Influent COD Concentration, mg/L",
  titlefont = f,
  tickfont = f
)
blank<- list(
  title="",
  tickfont=f)


CH4_Cons_formatter <- get_cscale('CH4_Cons')
px_TOT_formatter <- get_cscale('px_TOT')
O2_dem_formatter <- get_cscale('O2_dem')
LCH4gas_formatter <- get_cscale('LCH4gas')
COD_added_formatter <- get_cscale('COD_added')


pC_CH4_cons <- plot_ly(z = C_CH4_Cons, 
                       x=N_vector, 
                       y=COD_vector, 
                       zmin = CH4_Cons_formatter[[1]],
                       zmax = CH4_Cons_formatter[[2]],
                       colorbar = list(ticksuffix=CH4_Cons_formatter[3],
                                       title = ""),
                       type = "heatmap") %>%
  layout(xaxis = x, yaxis = y, title='Scenario C: % CH4 Consumed by NDAMO', titlefont=f)
pD_CH4_cons <- plot_ly(z = D_CH4_Cons, 
                       x = N_vector, 
                       zmin = CH4_Cons_formatter[[1]],
                       zmax = CH4_Cons_formatter[[2]],
                       colorbar = list(ticksuffix=CH4_Cons_formatter[3],
                                       title = ""),
                       y=COD_vector, type = 'heatmap') %>%
  layout(xaxis = x, yaxis = y, title='Scenario D: % CH4 Consumed by NDAMO', titlefont=f)
# Don't Print, too obvi
#print(pC_CH4_cons)
#print(pD_CH4_cons)
m = list(
  b = 50,
  t = 150,
  pad = 0
)
mplot = list(
  pad = 20,
  t=100
)
mplot2 = list(
  pad = 50,
  t=75,
  l=50
)
notes = list(
  list(x = .025, y = 1.1, text = "SHARON Anammox vs. MLE", font=f, showarrow = F, xref='paper', yref='paper'),
  list(x = 0.5, y = 1.175, text = "SHARON Anammox +<br>n-damo vs MLE", font=f, showarrow = F, xref='paper', yref='paper'),
  list(x = .975, y = 1.175, text = "SHARON Anammox + <br>n-damo + AnMBR vs MLE", font=f, showarrow = F, xref='paper', yref='paper'))

p_px <- subplot(plot_ly(z = B_px_TOT, 
                     x=N_vector, 
                     y=COD_vector, 
                     type = "heatmap", 
                     margin=mplot,
                     zmin = px_TOT_formatter[[1]],
                     zmax = px_TOT_formatter[[2]],
                     colorscale='RdBu',
                     colorbar = list(ticksuffix="%",
                                     title ="", tickfont=f)),
                plot_ly(z = C_px_TOT, 
                        x=N_vector, 
                        y=COD_vector, 
                        type = "heatmap", 
                        margin=mplot2,
                        zmin = px_TOT_formatter[[1]],
                        zmax = px_TOT_formatter[[2]],
                        colorscale='RdBu',
                        showscale=FALSE),
                plot_ly(z = D_px_TOT, 
                        x=N_vector, 
                        y=COD_vector, 
                        type = "heatmap", 
                        margin=mplot2,
                        zmin = px_TOT_formatter[[1]],
                        zmax = px_TOT_formatter[[2]],
                        colorscale='RdBu',
                        showscale=FALSE))%>%
  layout(xaxis = blank,
         xaxis2 = x,
         xaxis3 = blank,
         yaxis = y,
         yaxis2 = blank,
         yaxis3 = blank,
         autosize = F, width = 1100, height = 500, margin = m,
         title='% Change in Sludge Produced Between Scenarios',
         titlefont=f,
         annotations = notes)
#pC_px_tot <- ) %>%
#  layout(xaxis = x, yaxis = y, title='% Change in Sludge produced in Scenario C vs. Scenario A')
#pD_px_tot <-  %>%
#  layout(xaxis = x, yaxis = y, title='% Change in Sludge Produced in Scenario D vs. Scenario A')

#print(pA_px_tot) #we compare everything to A now
#print(pB_px_tot)
#print(pC_px_tot)
print(p_px)

pO2_dem <- subplot(plot_ly(z = B_O2_dem, 
                     x=N_vector, 
                     y=COD_vector,
                     margin=mplot,
                     type = "heatmap",
                     margin=mplot,
                     zmin = O2_dem_formatter[[1]],
                     zmax = O2_dem_formatter[[2]],
                     colorscale='RdBu',
                     colorbar = list(ticksuffix="%",
                                     title = "")),
                   plot_ly(z = C_O2_dem, 
                     x=N_vector, 
                     y=COD_vector, 
                     type = "heatmap",
                     margin=mplot2,
                     zmin = O2_dem_formatter[[1]],
                     zmax = O2_dem_formatter[[2]],
                     colorscale='RdBu',
                     colorbar = list(ticksuffix=O2_dem_formatter[3],
                                     title = "")),
                     plot_ly(z = D_O2_dem, 
                     x=N_vector, 
                     y=COD_vector, 
                     type = "heatmap",
                     margin=mplot2,
                     zmin = O2_dem_formatter[[1]],
                     zmax = O2_dem_formatter[[2]],
                     colorscale='RdBu',
                     colorbar = list(ticksuffix=O2_dem_formatter[3],
                                     title = ""))) %>%
  layout(xaxis = blank,
         xaxis2 = x,
         xaxis3 = blank,
         yaxis = y,
         yaxis2 = blank,
         yaxis3 = blank,
         autosize = F, width = 1100, height = 500, margin = m,
         titlefont=f,
         title='% Change in Oxygen Demand Between Scenarios',
         annotations = notes)
#print(pA_O2_dem) #we compare everything to A now
print(pO2_dem)
plot_ly(z = A_LCH4gas, 
        x=N_vector, 
        y=COD_vector, 
        type = "heatmap", 
        zmin = LCH4gas_formatter[[1]],
        zmax = LCH4gas_formatter[[2]],
        colorscale='RdBu',
        colorbar = list(ticksuffix=LCH4gas_formatter[3],
                        title = ""))
p_Methane <- subplot(plot_ly(z = B_LCH4gas, 
                      x=N_vector, 
                      y=COD_vector, 
                      type = "heatmap", 
                      zmin = LCH4gas_formatter[[1]],
                      zmax = LCH4gas_formatter[[2]],
                      colorscale='RdBu',
                      colorbar = list(ticksuffix="%",
                                      title = "")),
                     plot_ly(z = C_LCH4gas, 
                      x=N_vector, 
                      y=COD_vector, 
                      type = "heatmap", 
                      zmin = LCH4gas_formatter[[1]],
                      zmax = LCH4gas_formatter[[2]],
                      colorscale='RdBu',
                      colorbar = list(ticksuffix=LCH4gas_formatter[3],
                                      title = "")),
                    plot_ly(z = D_LCH4gas, 
                      x=N_vector, 
                      y=COD_vector, 
                      type = "heatmap", 
                      zmin = LCH4gas_formatter[[1]],
                      zmax = LCH4gas_formatter[[2]],
                      colorscale='RdBu',
                      colorbar = list(ticksuffix=LCH4gas_formatter[3],
                                      title = ""))) %>%
  layout(xaxis = blank,
         xaxis2 = x,
         xaxis3 = blank,
         yaxis = y,
         yaxis2 = blank,
         yaxis3 = blank,
         titlefont=f,
         autosize = F, width = 1100, height = 500, margin = m,
         title='% Change in Methane Produced Between Scenarios',
         annotations = notes)
#print(pA_Methane) #we compare everything to A now
print(p_Methane)
#print(pC_Methane)
#print(pD_Methane)

pA_COD_added <- plot_ly(z = A_COD_added, 
                      x=N_vector, 
                      y=COD_vector, 
                      type = "heatmap", 
                      zmin = 0,
                      zmax = 5,
                      colorbar = list(ticksuffix=COD_added_formatter[3],
                                      title = "")) %>%
  layout(xaxis = x, yaxis = y, title='Daily COD Addition Required in Metric Tons in Scenario D',
         titlefont=f)
#print(pA_COD_added) #Ammount of COD req'd in scenario A, maybe do we need this I don't know...
pD_CH4_added <- plot_ly(z = D_CH4_added_compare, 
                        x=N_vector, 
                        y=COD_vector, 
                        type = "heatmap", 
                        zmin = -100,
                        zmax = 0,
                        colorbar = list(ticksuffix=COD_added_formatter[3],
                                        title = "")) %>%
  layout(xaxis = x, yaxis = y, title='% Change in COD Addition Required in AnMBR/CANON system versus MLE system',
         titlefont=f)
print(pD_CH4_added) #Ammount of CH4 req'd in scenario D, maybe do we need this I don't know...
plotly_IMAGE(p_px, format = "png", out_file = "sludge.png", width = 1100, height = 500)
plotly_IMAGE(pO2_dem, format = "png", out_file = "o2.png", width = 1100, height = 500)
plotly_IMAGE(p_Methane, format = "png", out_file = "methane.png", width = 1100, height = 500)
#plotly_IMAGE(pA_COD_added, format = "png", out_file = "A_COD.png", width = 500, height = 500)
plotly_IMAGE(pD_CH4_added_compare, format = "png", out_file = "D_COD.png", width = 500, height = 500)

