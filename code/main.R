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
Nmin <- 2
CODmax <- 400
CODmin <- 0.01
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
col.labels.ext1 <- c('-100%', '0%', '100%','200%','300%','400%','500%', '600%', '>700%')
col.labels.low <- c('-100%', '-95%', '-50%', '0%','50%','100%')
col.percent <- colorRampPalette(c('blue', 'white', 'red'))
col.percent.low <- colorRampPalette(c('blue', 'lightblue', 'white', 'red'))
col.percent.ext1 <- colorRampPalette(c('blue', 'white', 'red', 'darkred'))
col.percent.ext2 <- colorRampPalette(c('blue', 'white', 'red', 'red3'))
ys <- list(at=seq(0,CODmax, by=50), labels=rep('', length(seq(0,CODmax, by=10))))
xs <- list(at=seq(0, Nmax, by=15), labels=rep('', length(seq(0, Nmax, by=15))))
gs <- list()

# Plot Titles
lab1 <- textGrob(c('HRAS/anammox','vs. MLE'), gp=gpar(fontfamily='serif'), y=c(0.9,0.33))
lab2 <- textGrob(c('AnMBR/anammox','vs. MLE'), gp=gpar(fontfamily='serif'), y=c(0.9,0.33))
lab3 <- textGrob(c('HRAS/anammox & n-damo', 'vs. MLE'),gp=gpar(fontfamily='serif'), y=c(0.9,0.33))
lab4 <- textGrob(c('AnMBR/anammox & n-damo','vs. MLE'), gp=gpar(fontfamily='serif'), y=c(0.9,0.33))


# Axes
lab5 <- textGrob('Nitrogen in Influent, mg/L', gp=gpar(fontfamily='serif'))
ax7 <- textGrob(seq(0, Nmax, by=15), x=seq(0, 1.02, length.out = 5), y=.25, just='right', gp=gpar(fontfamily='serif'))
ax8 <- textGrob(seq(15, Nmax, by=15), x=seq(0.255, 1.02, length.out = 4), y=.25, just='right', gp=gpar(fontfamily='serif'))
lab10 <- textGrob('COD in Influent, mg/L', rot=90, gp=gpar(fontfamily='serif'))
ax9 <- textGrob(seq(0,CODmax, by=50), y=seq(0, 1, length.out = 9), x=.75, just='right', gp=gpar(fontfamily='serif'))

fig.labels <- c('.1', '.2', '.3', '.4')


# Set up plot layout
lay <- rbind(rep(14, each=17),
             c(23,23,rep(19:20, each=6), rep(13,3)), #Scenarios A&B Plot #s
             c(23,23,rep(1:2, each=6), rep(13,3)), # Scenarios A&B Titles
             matrix(rep(c(12,11,rep(15:16, each=6), rep(13,3)), times=6), 
                    nrow=6, 
                    byrow=T), # Scenarios A&B
             rep(24, each=17),
             c(24,24,rep(21:22, each=6), rep(13,3)), #Scenarios C&D Plot #s
             c(24,24, rep(3:4, each=6), rep(13,3)), # Scenarios C&D Titles
             matrix(rep(c(10,9,rep(17:18, each=6), rep(13,3)), times=6), 
                    nrow=6, 
                    byrow=T),
             c(25,25,rep(7:8,   each=6), rep(13,3)),
             c(25,25,rep(5:6,   each=6), rep(13,3)),
             rep(25, each=17)) # Scenarios C&D

p <- list()


#= Plot Oxygen Demand
png('code/figures/O2 Demand.png', width=8.5, height=5.5, units='in', res=750)
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
lab14 <- textGrob(expression('Oxygen Demand w/ respect to Base Case'), gp=gpar(cex=1.25, fontfamily='serif'))
lab19 <- textGrob(paste(fig.no, fig.labels[1], sep=''), gp=gpar(fontface='bold', fontfamily='serif'))
lab20 <- textGrob(paste(fig.no, fig.labels[2], sep=''), gp=gpar(fontface='bold', fontfamily='serif'))
lab21 <- textGrob(paste(fig.no, fig.labels[3], sep=''), gp=gpar(fontface='bold', fontfamily='serif'))
lab22 <- textGrob(paste(fig.no, fig.labels[4], sep=''), gp=gpar(fontface='bold', fontfamily='serif'))

grid.arrange(grobs = list(lab1, lab2, lab3, lab4, # Plot titles
                          lab5, lab5, ax7, ax8, # X-Axis
                          ax9, lab10, ax9, lab10, # Y-Axis
                          leg13, # Colorbar
                          lab14,
                          p[[1]], p[[2]],p[[3]], p[[4]], # Plots
                          lab19, lab20, lab21, lab22,
                          textGrob(''), textGrob(''), textGrob('')
                          ), layout_matrix = lay)
 dev.off()


#= Plot Sludge Production
png('code/figures/Sludge Production.png', width=8.5, height=5.5, units='in', res=750)
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


# Assign Sludge Production graphs to layout
lab14 <- textGrob(expression('Sludge Production w/ respect to Base Case'), gp=gpar(cex=1.25, fontfamily='serif'))
lab19 <- textGrob(paste(fig.no, fig.labels[1], sep=''), gp=gpar(fontface='bold', fontfamily='serif'))
lab20 <- textGrob(paste(fig.no, fig.labels[2], sep=''), gp=gpar(fontface='bold', fontfamily='serif'))
lab21 <- textGrob(paste(fig.no, fig.labels[3], sep=''), gp=gpar(fontface='bold', fontfamily='serif'))
lab22 <- textGrob(paste(fig.no, fig.labels[4], sep=''), gp=gpar(fontface='bold', fontfamily='serif'))

grid.arrange(grobs = list(lab1, lab2, lab3, lab4, # Plot titles
                          lab5, lab5, ax7, ax8, # X-Axis
                          ax9, lab10, ax9, lab10, # Y-Axis
                          leg13, # Colorbar
                          lab14,
                          p[[1]], p[[2]],p[[3]], p[[4]], # Plots
                          lab19, lab20, lab21, lab22,
                          textGrob(''), textGrob(''), textGrob('')
                          ), layout_matrix = lay)
dev.off()

#= Plot Methane Production
png('code/figures/Methane Production.png', width=8.5, height=5.5, units='in', res=500)
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
lab14 <- textGrob(expression('Methane Production w/ respect to Base Case'), gp=gpar(cex=1.25, fontfamily='serif'))
lab19 <- textGrob(paste(fig.no, fig.labels[1], sep=''), gp=gpar(fontface='bold', fontfamily='serif'))
lab20 <- textGrob(paste(fig.no, fig.labels[2], sep=''), gp=gpar(fontface='bold', fontfamily='serif'))
lab21 <- textGrob(paste(fig.no, fig.labels[3], sep=''), gp=gpar(fontface='bold', fontfamily='serif'))
lab22 <- textGrob(paste(fig.no, fig.labels[4], sep=''), gp=gpar(fontface='bold', fontfamily='serif'))

grid.arrange(grobs = list(lab1, lab2, lab3, lab4, # Plot titles
                          lab5, lab5, ax7, ax8, # X-Axis
                          ax9, lab10, ax9, lab10, # Y-Axis
                          leg13, # Colorbar
                          lab14,
                          p[[1]], p[[2]],p[[3]], p[[4]], # Plots
                          lab19, lab20, lab21, lab22,
                          textGrob(''), textGrob(''), textGrob('')
), layout_matrix = lay)
dev.off()


#= Plot GHG Emissions
png('code/figures/GHG.png', width=8.5, height=5.5, units='in', res=500)
fig.no <- 5
for (i in 1:(length(result)-1)) {
  d <- select(result[[i+1]], Nitrogen, Carbon, CO2.equivs)
  d$CO2.equivs[d$CO2.equivs>5] <- 5
  p[[i]] <- levelplot(CO2.equivs ~ Nitrogen * Carbon, data=d,
                      par.settings=list(axis.text=list(fontfamily="serif")),
                      panel = function(...){
                        panel.levelplot(...)
                        panel.abline(v = seq(0,Nmax-1, by=15), alpha=0.5)
                        panel.abline(h = seq(0,CODmax-1, by=50), alpha=0.5)
                      },
                      at=c(seq(-1,1, length=100), seq(1+.01, 7, length=50)),
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
lab14 <- textGrob(expression('GHG Emissions w/ respect to Base Case'), gp=gpar(cex=1.25, fontfamily='serif'))
lab19 <- textGrob(paste(fig.no, fig.labels[1], sep=''), gp=gpar(fontface='bold', fontfamily='serif'))
lab20 <- textGrob(paste(fig.no, fig.labels[2], sep=''), gp=gpar(fontface='bold', fontfamily='serif'))
lab21 <- textGrob(paste(fig.no, fig.labels[3], sep=''), gp=gpar(fontface='bold', fontfamily='serif'))
lab22 <- textGrob(paste(fig.no, fig.labels[4], sep=''), gp=gpar(fontface='bold', fontfamily='serif'))

grid.arrange(grobs = list(lab1, lab2, lab3, lab4, # Plot titles
                          lab5, lab5, ax7, ax8, # X-Axis
                          ax9, lab10, ax9, lab10, # Y-Axis
                          leg13, # Colorbar
                          lab14,
                          p[[1]], p[[2]],p[[3]], p[[4]], # Plots
                          lab19, lab20, lab21, lab22,
                          textGrob(''), textGrob(''), textGrob('')
), layout_matrix = lay)
dev.off()


# Define High/Med/Low COD/N ratio
qqO2.B = quantile(result$B$O2.demand, probs = seq(0, 1, 0.3333))
qqO2.C = quantile(result$C$O2.demand, probs = seq(0, 1, 0.3333))
qqO2.D = quantile(result$D$O2.demand, probs = seq(0, 1, 0.3333))



### Not Used
#= Plot external carbon addition
png('code/figures/External Carbon.png', width=8.5, height=3, units='in', res=500)
fig.no <- 0
for (i in 1:(length(result)-1)) {
  d <- select(result[[i+1]], Nitrogen, Carbon, COD.added)
  p[[i]] <- levelplot(COD.added ~ Nitrogen * Carbon, data=d,
                      par.settings=list(axis.text=list(fontfamily="serif")),
                      panel = function(...){
                        panel.levelplot(...)
                        panel.abline(v = seq(0,Nmax-1, by=15), alpha=0.5)
                        panel.abline(h = seq(0,CODmax-1, by=50), alpha=0.5)
                      },
                      at=c(seq(-1,-0.95, length=66),seq(-0.95+.01, 1, length=66*2)),
                      col.regions=col.percent.low,
                      xlab="", ylab="",
                      scales=list(cex=1, tck = c(1,0),
                                  x=x_tck,
                                  y=ys),
                      colorkey = list(labels=list(cex=1,
                                                  at=c(-1.0, -0.95, -0.5 ,0, 0.5, 1),
                                                  labels=col.labels.low)))
  
  leg.list <- p[[i]]$legend$right$args$key
  leg8 <-  draw.colorkey(leg.list)
  leg8$children[[4]]$children[[1]]$gp$fontfamily <- 'serif'
  p[[i]] <- update(p[[i]], legend=NULL)
}

# Assign graphs to layout
lab7 <- textGrob(expression('External Carbon Addition w/ respect to Base Case'), gp=gpar(cex=1.25, fontfamily='serif'))
lab12 <- textGrob(paste(fig.no, fig.labels[1], sep=''), gp=gpar(fontface='bold', fontfamily='serif'))
lab13 <- textGrob(paste(fig.no, fig.labels[2], sep=''), gp=gpar(fontface='bold', fontfamily='serif'))
lab14 <- textGrob(paste(fig.no, fig.labels[3], sep=''), gp=gpar(fontface='bold', fontfamily='serif'))
grid.arrange(grobs = list(lab1,lab2,lab3,lab4,ax5, lab6, lab7, leg8, p[[1]], p[[2]],p[[3]], lab12, lab13, lab14), layout_matrix = lay)
dev.off()

