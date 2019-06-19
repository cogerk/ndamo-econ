#=== main.R
#== Load necessary files 
source('code/masterrun.R')
source('code/draw_graphs.R')

Nmin <- .6
Nmax <- 60
CODmax <- 400
CODmin <- 4
cNin <- seq(Nmin, Nmax, length=100) # Nitrogen concentration varies
cCODin <- seq(CODmin, CODmax, length=100) # Carbon concentration varies
Q <- 60 # constant flow rate

#== Run simultions
#result_1 <- scenarios(Q, Nmax/2, CODmax/2, compare=TRUE)
#result_SS1 <- scenarios(Q, Nmax/2, CODmax/2, e_Base=200, compare=TRUE)

result_SS_Base <- scenarios(Q, cNin, cCODin, compare=TRUE)
result_SS <- scenarios(Q, cNin, cCODin, e_Base=90*5, compare=TRUE)
result_SS_compare <- result_SS

for (i in 1:(length(result_SS_Base)-1)) {
  for(j in 7:length(result_SS_Base$A)) {
    result_SS_compare[[i+1]][j] <- 1- result_SS[[i+1]][j]/result_SS_Base[[i+1]][j]
  }
}


#= Plot Cost 
col.percent <- colorRampPalette(c('darkred','red', 'white', 'blue', 'darkblue'))
col.bar.range <- seq(-20000,20000, length=1000)
col.bar.range.lab <- c('-$20000', '$10000', '$0', '$10000', '$20000')
col.bar.range.lab.at <- seq(-20000,20000,length=5)
col.bar.lab <- '$ Saved from Base Case (MLE)'
graph.title <- 'Total Reduction in Primary Cost Factors from the Base Case (MLE)'
fig.no<-2
draw_graph(CODmax, Nmax, font, result_SS, select_var='cost', max=Inf, min=-Inf,
                                                  col.percent, col.bar.range, col.bar.range.lab, 
                                                  col.bar.range.lab.at, col.bar.lab, 
                                                  fig.no, graph.title)

#= Plot GHG
col.percent <- colorRampPalette(c('darkblue','blue', 'white', 'red', 'darkred'))
col.bar.range <- seq(-5, 5, length=1000)
col.bar.range.lab <- c('<-500%', '-100%', '0%', '100%', '>500%')
col.bar.range.lab.at <- c(-5, -1, 0, 1, 5)
col.bar.lab <- '% Difference from Base Case'
graph.title <- 'GHG Production w/ respect to the Base Case (MLE)'
fig.no<-3
draw_graph(CODmax, Nmax, font, result_SS, select_var='CO2.equivs', max=5, min=-5,
                                          col.percent, col.bar.range, col.bar.range.lab, 
                                          col.bar.range.lab.at, col.bar.lab, 
                                          fig.no, graph.title)

draw_graph(CODmax, Nmax, font, result_SS_compare, select_var='CO2.equivs', max=5, min=-5,
           col.percent, col.bar.range, col.bar.range.lab, 
           col.bar.range.lab.at, col.bar.lab, 
           fig.no, graph.title)
