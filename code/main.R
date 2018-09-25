#=== main.R
#== Load necessary files 
source('code/masterrun.R')
require(latticeExtra)
require(gridExtra)
require(ggplot2)
require(grid)
require(extrafont)


#== Set up conditions to run
Nmax <- 60
Nmin <- 0.6
CODmax <- 400
CODmin <- 4
Qmax <- 600
cNin <- seq(Nmin, Nmax, length=100) # Nitrogen concentration varies
cCODin <- seq(CODmin, CODmax, length=100) # Carbon concentration varies
Q <- 60 # constant flow rate

#== Run simultions
result <- scenarios(Q,cNin,cCODin, compare=TRUE)

# Setup the plot margins
lw <- list(left.padding = list(x = -.33, units = "inches"))
lw$right.padding <- list(x = -.23, units = "inches")
lh <- list(bottom.padding = list(x = -.55, units = "inches"))
lh$top.padding <- list(x = -.325, units = "inches")
lattice.options(layout.widths = lw, layout.heights = lh)

# Set up labels
col.labels <- c('-100%', '-50%', '0%','50%','100%')
col.labels.ext1 <- c('-100%', '0%', '100%','200%','300%','400%','>500%')
col.labels.ext2 <- c('<-200%', '-100%','0%','100%','>200%')
col.labels.low <- c('-100%', '-95%', '-50%', '0%','50%','100%')
col.percent <- colorRampPalette(c('blue', 'white', 'red'))
col.percent.low <- colorRampPalette(c('blue', 'lightblue', 'white', 'red'))
col.percent.ext1 <- colorRampPalette(c('blue', 'white', 'red', 'darkred'))
col.percent.ext2 <- colorRampPalette(c('darkblue', 'blue', 'white', 'red', 'darkred'))
ys <- list(at=seq(0,CODmax, by=50), labels=rep('', length(seq(0,CODmax, by=10))))
xs <- list(at=seq(0, Nmax, by=15), labels=rep('', length(seq(0, Nmax, by=15))))
gs <- list()

# Plot Titles
lab1.name <- 'HRAS/anammox vs. MLE'
lab2.name <- 'AnMBR/anammox vs. MLE'
lab3.name <- 'HRAS/anammox/n-damo vs. MLE'
lab4.name <- 'AnMBR/anammox/n-damo vs. MLE'


# Axes
lab5 <- textGrob('Nitrogen in Influent, mg/L', gp=gpar(fontfamily='serif', cex=0.9))
ax7 <- textGrob(seq(0, Nmax, by=15), x=seq(0, 1.02, length.out = 5), y=.25, just='right', gp=gpar(fontfamily='serif', cex=0.9))
ax8 <- textGrob(seq(15, Nmax, by=15), x=seq(0.255, 1.02, length.out = 4), y=.25, just='right', gp=gpar(fontfamily='serif', cex=0.9))
lab10 <- textGrob('COD in Influent, mg/L', rot=90, gp=gpar(fontfamily='serif', cex=0.9), x=.75)
ax9 <- textGrob(seq(0,CODmax, by=50), y=seq(0, 1, length.out = 9), x=.75, just='right', gp=gpar(fontfamily='serif', cex=0.9))
lab23 <- textGrob('% Difference from Base Case (MLE)',
                  gp=gpar(fontfamily='serif'), rot=270, x=-.25)
fig.labels <- c('.1', '.2', '.3', '.4')


# Set up plot layout
lay <- as.matrix(read.csv(file = 'code/figures/figure_layout.csv', header=FALSE)) # Scenarios C&D
width <- 7.5
height <- 6.5
p <- list()


#= Plot Oxygen Demand
png('code/figures/O2 Demand.png', width=width, height=height, units='in', res=750)
fig.no <- 2 
for (i in 1:(length(result)-1)) {
  d <- select(result[[i+1]], Nitrogen, Carbon, O2.demand)
  p[[i]] <- levelplot(O2.demand ~ Nitrogen * Carbon, data=d,
                      panel = function(...){
                        panel.levelplot(...)
                        panel.abline(v = seq(0,Nmax-1, by=15), alpha=0.5)
                        panel.abline(h = seq(0,CODmax-1, by=50), alpha=0.5)
                        },
                      at=seq(-1,1,by=.01), col.regions=col.percent, 
                      xlab="", ylab="",
                      scales=list(cex=1, tck = c(1,0), 
                                  x=xs,
                                  y=ys),
                      colorkey = list(labels=list(cex=1, 
                                                  labels=col.labels, 
                                                  par.settings=list(fontfamily='serif'))))
  
  leg.list <- p[[i]]$legend$right$args$key
  leg13 <-  draw.colorkey(leg.list)
  leg13$children[[4]]$children[[1]]$gp$fontfamily <- 'serif'
  p[[i]] <- update(p[[i]], legend=NULL)
}


# Assign Oxygen Demand graphs to layout
lab14 <- textGrob('Oxygen Demand w/ respect to Base Case (MLE)', gp=gpar(cex=1.25, fontfamily='serif'))
lab1 <- textGrob(paste(paste(fig.no, fig.labels[1], sep=''), lab1.name), 
                 gp=gpar(fontfamily='serif'))
lab2 <- textGrob(paste(paste(fig.no, fig.labels[2], sep=''), lab2.name),
                 gp=gpar(fontfamily='serif'))
lab3 <- textGrob(paste(paste(fig.no, fig.labels[3], sep=''), lab3.name),
                 gp=gpar(fontfamily='serif'))
lab4 <- textGrob(paste(paste(fig.no, fig.labels[4], sep=''), lab4.name),
                 gp=gpar(fontfamily='serif'))

grid.arrange(grobs = list(lab1, lab2, lab3, lab4,
                          lab5, lab5, ax7, ax8, # X-Axis, Bottom, 5,6,7,8
                          ax9, lab10, ax9, lab10, # Y-Axis left, 9,10,11,12
                          leg13,  # Colorbar, 13
                          lab14, # Figure Name, 14
                          p[[1]], p[[2]],p[[3]], p[[4]], # Plots, 15,16,17,18
                          ax7, ax8, lab5, lab5, # X-Axis, Top, 19,20,21,22
                          lab23, # Colorbar Label, 23
                          lab10, ax9, lab10, ax9, # Y-Axis right, 24,25,26,27
                          textGrob(''), textGrob(''), textGrob(''), textGrob(''), textGrob(''), textGrob(''), textGrob(''), textGrob('') #Gaps
                          ), layout_matrix = lay)
 dev.off()


#= Plot Sludge Production
png('code/figures/Sludge Production.png', width=width, height=height, units='in', res=750)
fig.no <- 3
for (i in 1:(length(result)-1)) {
  d <- select(result[[i+1]], Nitrogen, Carbon, sludge.out)
  p[[i]] <- levelplot(sludge.out ~ Nitrogen * Carbon, data=d,
                      panel = function(...){
                        panel.levelplot(...)
                        panel.abline(v = seq(0,Nmax-1, by=15), alpha=0.5)
                        panel.abline(h = seq(0,CODmax-1, by=50), alpha=0.5)
                      },
                      at=seq(-1,1,by=.01), col.regions=col.percent, 
                      xlab="", ylab="",
                      scales=list(cex=1, tck = c(1,0), 
                                  x=xs,
                                  y=ys),
                      colorkey = list(labels=list(cex=1, 
                                                  labels=col.labels, 
                                                  par.settings=list(fontfamily='serif'))))
  
  leg.list <- p[[i]]$legend$right$args$key
  leg13 <-  draw.colorkey(leg.list)
  leg13$children[[4]]$children[[1]]$gp$fontfamily <- 'serif'
  p[[i]] <- update(p[[i]], legend=NULL)
}


# Assign Oxygen Demand graphs to layout
lab14 <- textGrob('Sludge Production w/ respect to Base Case (MLE)', gp=gpar(cex=1.25, fontfamily='serif'))
lab1 <- textGrob(paste(paste(fig.no, fig.labels[1], sep=''), lab1.name), 
                 gp=gpar(fontfamily='serif'))
lab2 <- textGrob(paste(paste(fig.no, fig.labels[2], sep=''), lab2.name),
                 gp=gpar(fontfamily='serif'))
lab3 <- textGrob(paste(paste(fig.no, fig.labels[3], sep=''), lab3.name),
                 gp=gpar(fontfamily='serif'))
lab4 <- textGrob(paste(paste(fig.no, fig.labels[4], sep=''), lab4.name),
                 gp=gpar(fontfamily='serif'))

grid.arrange(grobs = list(lab1, lab2, lab3, lab4,
                          lab5, lab5, ax7, ax8, # X-Axis, Bottom, 5,6,7,8
                          ax9, lab10, ax9, lab10, # Y-Axis left, 9,10,11,12
                          leg13,  # Colorbar, 13
                          lab14, # Figure Name, 14
                          p[[1]], p[[2]],p[[3]], p[[4]], # Plots, 15,16,17,18
                          ax7, ax8, lab5, lab5, # X-Axis, Top, 19,20,21,22
                          lab23, # Colorbar Label, 23
                          lab10, ax9, lab10, ax9, # Y-Axis right, 24,25,26,27
                          textGrob(''), textGrob(''), textGrob(''), textGrob(''), textGrob(''), textGrob(''), textGrob(''), textGrob('') #Gaps
), layout_matrix = lay)
dev.off()

#= Plot Methane Production
png('code/figures/Methane Production.png', width=width, height=height, units='in', res=500)
fig.no <- 4
for (i in 1:(length(result)-1)) {
  d <- select(result[[i+1]], Nitrogen, Carbon, CH4.burn)
  d$CH4.burn[d$CH4.burn>5] <- 5 # Max 500%
  p[[i]] <- levelplot(CH4.burn ~ Nitrogen * Carbon, data=d,
                      par.settings=list(axis.text=list(fontfamily="serif")),
                      panel = function(...){
                        panel.levelplot(...)
                        panel.abline(v = seq(0,Nmax-1, by=15), alpha=0.5)
                        panel.abline(h = seq(0,CODmax-1, by=50), alpha=0.5)
                      },
                      at=c(seq(-1,1, length=100),seq(1+.01, 5, length=50)),
                      col.regions=col.percent.ext1,
                      xlab="", ylab="",
                      scales=list(cex=1, tck = c(1,0),
                                  x=xs,
                                  y=ys),
                      colorkey = list(labels=list(cex=1,
                                                  labels=col.labels.ext1)))
  leg.list <- p[[i]]$legend$right$args$key
  leg13 <-  draw.colorkey(leg.list)
  leg13$children[[4]]$children[[1]]$gp$fontfamily <- 'serif'
  p[[i]] <- update(p[[i]], legend=NULL)
}


# Assign Methane Production graphs to layout
lab14 <- textGrob('Methane Production w/ respect to Base Case (MLE)', gp=gpar(cex=1.25, fontfamily='serif'))
lab1 <- textGrob(paste(paste(fig.no, fig.labels[1], sep=''), lab1.name), 
                 gp=gpar(fontfamily='serif'))
lab2 <- textGrob(paste(paste(fig.no, fig.labels[2], sep=''), lab2.name),
                 gp=gpar(fontfamily='serif'))
lab3 <- textGrob(paste(paste(fig.no, fig.labels[3], sep=''), lab3.name),
                 gp=gpar(fontfamily='serif'))
lab4 <- textGrob(paste(paste(fig.no, fig.labels[4], sep=''), lab4.name),
                 gp=gpar(fontfamily='serif'))

grid.arrange(grobs = list(lab1, lab2, lab3, lab4,
                          lab5, lab5, ax7, ax8, # X-Axis, Bottom, 5,6,7,8
                          ax9, lab10, ax9, lab10, # Y-Axis left, 9,10,11,12
                          leg13,  # Colorbar, 13
                          lab14, # Figure Name, 14
                          p[[1]], p[[2]],p[[3]], p[[4]], # Plots, 15,16,17,18
                          ax7, ax8, lab5, lab5, # X-Axis, Top, 19,20,21,22
                          lab23, # Colorbar Label, 23
                          lab10, ax9, lab10, ax9, # Y-Axis right, 24,25,26,27
                          textGrob(''), textGrob(''), textGrob(''), textGrob(''), textGrob(''), textGrob(''), textGrob(''), textGrob('') #Gaps
), layout_matrix = lay)
dev.off()


#= Plot GHG Emissions
png('code/figures/GHG.png', width=width, height=height, units='in', res=500)
fig.no <- 5
for (i in 1:(length(result)-1)) {
  d <- select(result[[i+1]], Nitrogen, Carbon, CO2.equivs)
  d$CO2.equivs[d$CO2.equivs>2] <- 2
  d$CO2.equivs[-2>d$CO2.equivs] <- -2
  p[[i]] <- levelplot(CO2.equivs ~ Nitrogen * Carbon, data=d,
                      par.settings=list(axis.text=list(fontfamily="serif")),
                      panel = function(...){
                        panel.levelplot(...)
                        panel.abline(v = seq(0,Nmax-1, by=15), alpha=0.5)
                        panel.abline(h = seq(0,CODmax-1, by=50), alpha=0.5)
                      },
                      at=seq(-2,2,by=.01),
                      col.regions=col.percent.ext2,
                      xlab="", ylab="",
                      scales=list(cex=1, tck = c(1,0),
                                  x=xs,
                                  y=ys),
                      colorkey = list(labels=list(cex=1,at=c(-2, -1, 0,1, 2),
                                                  labels=col.labels.ext2)))
  leg.list <- p[[i]]$legend$right$args$key
  leg13 <-  draw.colorkey(leg.list)
  leg13$children[[4]]$children[[1]]$gp$fontfamily <- 'serif'
  p[[i]] <- update(p[[i]], legend=NULL)
}


# Assign GHG Production graphs to layout
lab14 <- textGrob('GHG Production w/ respect to Base Case (MLE)', gp=gpar(cex=1.25, fontfamily='serif'))
lab1 <- textGrob(paste(paste(fig.no, fig.labels[1], sep=''), lab1.name), 
                 gp=gpar(fontfamily='serif'))
lab2 <- textGrob(paste(paste(fig.no, fig.labels[2], sep=''), lab2.name),
                 gp=gpar(fontfamily='serif'))
lab3 <- textGrob(paste(paste(fig.no, fig.labels[3], sep=''), lab3.name),
                 gp=gpar(fontfamily='serif'))
lab4 <- textGrob(paste(paste(fig.no, fig.labels[4], sep=''), lab4.name),
                 gp=gpar(fontfamily='serif'))

grid.arrange(grobs = list(lab1, lab2, lab3, lab4,
                          lab5, lab5, ax7, ax8, # X-Axis, Bottom, 5,6,7,8
                          ax9, lab10, ax9, lab10, # Y-Axis left, 9,10,11,12
                          leg13,  # Colorbar, 13
                          lab14, # Figure Name, 14
                          p[[1]], p[[2]],p[[3]], p[[4]], # Plots, 15,16,17,18
                          ax7, ax8, lab5, lab5, # X-Axis, Top, 19,20,21,22
                          lab23, # Colorbar Label, 23
                          lab10, ax9, lab10, ax9, # Y-Axis right, 24,25,26,27
                          textGrob(''), textGrob(''), textGrob(''), textGrob(''), textGrob(''), textGrob(''), textGrob(''), textGrob('') #Gaps
), layout_matrix = lay)
dev.off()
