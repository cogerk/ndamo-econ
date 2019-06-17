#=== main.R
#== Load necessary files 
source('code/masterrun.R')
source('code/draw_graphs.R')

#== Set up graphs
font <- 'serif'
width <- 7.5
height <- 6.5

#== Set up conditions to run
Nmax <- 60
Nmin <- .6
CODmax <- 400
CODmin <- 4
cNin <- seq(Nmin, Nmax, length=100) # Nitrogen concentration varies
cCODin <- seq(CODmin, CODmax, length=100) # Carbon concentration varies
Q <- 60 # constant flow rate

#== Run simultions
result <- scenarios(Q,cNin,cCODin, compare=TRUE)

#= Plot Cost
col.percent <- colorRampPalette(c('darkred','red', 'white', 'blue', 'darkblue'))
col.bar.range <- seq(-20000,20000, length=1000)
col.bar.range.lab <- c('-$20000', '$10000', '$0', '$10000', '$20000')
col.bar.range.lab.at <- seq(-20000,20000,length=5)
col.bar.lab <- '$ Saved from Base Case (MLE)'
graph.title <- 'Total Reduction in Primary Cost Factors from the Base Case (MLE)'
fig.no<-2
draw_graph(CODmax, Nmax, font, result, select_var='cost', max=Inf, min=-Inf,
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
ghg_graph <- draw_graph(CODmax, Nmax, font, result, select_var='CO2.equivs', max=5, min=-5,
           col.percent, col.bar.range, col.bar.range.lab, 
           col.bar.range.lab.at, col.bar.lab, 
           fig.no, graph.title)


#= Plot O2 Demand
col.percent <- colorRampPalette(c('blue', 'white', 'red' ))
col.bar.range <- seq(-1, 1, length=1000)
col.bar.range.lab <- c('-100%', '-50%', '0%', '50%', '100%')
col.bar.range.lab.at <- c(-1, -0.5, 0, 0.5, 1)
col.bar.lab <- '% Difference from Base Case'
graph.title <- 'Oxygen Demand w/ respect to the Base Case (MLE)'
fig.no<-'S6'
o2_graph <- draw_graph(CODmax, Nmax, font, result, select_var='O2.demand', max=Inf, min=-Inf,
                        col.percent, col.bar.range, col.bar.range.lab, 
                        col.bar.range.lab.at, col.bar.lab, 
                        fig.no, graph.title)

#= Plot Sludge Demand
col.percent <- colorRampPalette(c('blue', 'white', 'red' ))
col.bar.range <- seq(-1, 1, length=1000)
col.bar.range.lab <- c('-100%', '-50%', '0%', '50%', '100%')
col.bar.range.lab.at <- c(-1, -0.5, 0, 0.5, 1)
col.bar.lab <- '% Difference from Base Case'
graph.title <- 'Sludge Production w/ respect to the Base Case (MLE)'
fig.no<-'S7'
sludge_graph <- draw_graph(CODmax, Nmax, font, result, select_var='sludge.out', max=Inf, min=-Inf,
                       col.percent, col.bar.range, col.bar.range.lab, 
                       col.bar.range.lab.at, col.bar.lab, 
                       fig.no, graph.title)

#= Plot Methane Production
col.percent <- colorRampPalette(c('red', 'white', 'blue', 'darkblue'))
col.bar.range <- c(seq(-1, 0, length=325), seq(.004, 5, length=675))
col.bar.range.lab <- c('-100%', '0%', '100%', '250%', '>500%')
col.bar.range.lab.at <- c(-1, 0, 1, 2.5, 5)
col.bar.lab <- '% Difference from Base Case'
graph.title <- 'Methane Production w/ respect to the Base Case (MLE)'
fig.no<-'S8'
sludge_graph <- draw_graph(CODmax, Nmax, font, result, select_var='CH4.burn', max=5, min=-Inf,
                           col.percent, col.bar.range, col.bar.range.lab, 
                           col.bar.range.lab.at, col.bar.lab, 
                           fig.no, graph.title)
