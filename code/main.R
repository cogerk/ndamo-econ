#=== main.R
#== Load necessary files 
source('code/masterrun.R')
require(latticeExtra)
require(gridExtra)
require(ggplot2)
require(grid)


#== Set up conditions to run
Nmax <- 60
Nmin <- 2
CODmax <- 400
CODmin <- 30
Qmax <- 600
cNin <- seq(Nmin, Nmax, length=100) # Nitrogen concentration varies
cCODin <- seq(CODmin, CODmax, length=100) # Carbon concentration varies
Q <- 60 # constant flow rate

#== Run simultions
result <- scenarios(Q,cNin,cCODin, compare=TRUE)

# Setup the plot margins
lw <- list(left.padding = list(x = -.33, units = "inches"))
lw$right.padding <- list(x = -.23, units = "inches")
lh <- list(bottom.padding = list(x = -.25, units = "inches"))
lh$top.padding <- list(x = -.325, units = "inches")
lattice.options(layout.widths = lw, layout.heights = lh)

# Set up labels
col.labels <- c('-100%', '-50%', '0%','50%','100%')
col.labels.ext1 <- c('-100%', '0%', '100%','200%','300%','400%','>500%')
col.labels.ext2 <- c('-100%', '-50%', '0%','50%','100%','150%')
col.labels.low <- c('-100%', '-95%', '-50%', '0%','50%','100%')
col.percent <- colorRampPalette(c('blue', 'white', 'red'))
col.percent.low <- colorRampPalette(c('blue', 'lightblue', 'white', 'red'))
col.percent.ext1 <- colorRampPalette(c('blue', 'white', 'red', 'darkred'))
col.percent.ext2 <- colorRampPalette(c('blue', 'white', 'red', 'red3'))
ys <-list(at=seq(0,CODmax, by=50), labels=rep('', length(seq(0,CODmax, by=10))))
xs <- list(at=seq(15,Nmax, by=15), labels=seq(15,Nmax, by=15))
xs_0 <- list(at=seq(0,Nmax, by=15), labels=seq(0,Nmax, by=15))
gs <- list()
gs[1] <- grob(textGrob('CANON vs. MLE'))
gs[2] <- grob(textGrob(c('anammox/n-damo', 'vs. MLE'), y=c(0.8,0.33)))
gs[3] <- grob(textGrob(c('anammox/n-damo/AnMBR','vs. MLE'), y=c(0.8,0.33)))
gs[4] <- grob(textGrob('Total Nitrogen Concentration in Influent, mg/L'))
gs[5] <- grob(textGrob(seq(0,CODmax, by=50), y=seq(0.12, .85, length.out = 8), x=.75, just='right'))
gs[6] <- grob(textGrob('COD Concentration in Influent, mg/L', rot=90))
fig.labels <- c('.1', '.2', '.3')


# Set up plot layout
lay <- rbind(rep(7, times=20),
             c(15,15,rep(12:14,each=5), 8, 8, 8),
             c(6,5,rep(1:3,each=5), 8, 8, 8),
             matrix(rep(c(6,5,rep(9:11, each=5),rep(8, times=3)),times=6), 
                    nrow=6, 
                    byrow=T),
             rep(4, times=20))
p <- list()


#= Plot Oxygen Demand
png('code/figures/O2 Demand.png', width=8.5, height=3.3, units='in', res=500)
fig.no <- 3
for (i in 1:(length(result)-1)) {
  d <- select(result[[i+1]], Nitrogen, Carbon, O2.demand)
  if (i > 1) {x_tck <- xs} else {x_tck <- xs_0}
  p[[i]] <- levelplot(O2.demand ~ Nitrogen * Carbon, data=d,
                      panel = function(...){
                        panel.levelplot(...)
                        panel.abline(v = seq(0,Nmax-1, by=15), alpha=0.5)
                        panel.abline(h = seq(0,CODmax-1, by=50), alpha=0.5)
                        },
                      at=seq(-1,1,by=.01), col.regions=col.percent, 
                      xlab="", ylab="",
                      scales=list(cex=1, tck = c(1,0), 
                                  x=x_tck,
                                  y=ys),
                      colorkey = list(labels=list(cex=1, 
                                                  labels=col.labels)))
  
  leg.list <- p[[i]]$legend$right$args$key
  leg.list$cex <- 1.5
  leg <-  grob(draw.colorkey(leg.list))
  p[[i]] <- update(p[[i]], legend=NULL)
}

# Assign Oxygen Demand graphs to layout
gs[7] <- grob(textGrob(expression('Oxygen Demand w/ respect to Base Case'), gp=gpar(cex=1.25)))
gs[8] <- leg
gs[9:11] <- p
gs[12] <- grob(textGrob(paste(fig.no, fig.labels[1], sep=''), gp=gpar(fontface='bold')))
gs[13] <- grob(textGrob(paste(fig.no, fig.labels[2], sep=''), gp=gpar(fontface='bold')))
gs[14] <- grob(textGrob(paste(fig.no, fig.labels[3], sep=''), gp=gpar(fontface='bold')))
grid.arrange(grobs = gs, layout_matrix = lay)
dev.off()



#= Plot Sludge Production 
png('code/figures/Sludge Production.png', width=8.5, height=3, units='in', res=500)
fig.no <- 4
for (i in 1:(length(result)-1)) {
  d <- select(result[[i+1]], Nitrogen, Carbon, sludge.out)
  p[[i]] <- levelplot(sludge.out ~ Nitrogen * Carbon, data=d,
                      panel = function(...){
                        panel.levelplot(...)
                        panel.abline(v = seq(0,Nmax-1, by=15), alpha=0.5)
                        panel.abline(h = seq(0,CODmax-1, by=50), alpha=0.5)
                      },
                      at=c(seq(-1,1, length=100),seq(1+.01, 1.5, length=50)), col.regions=col.percent.ext2, 
                      xlab="", ylab="",
                      scales=list(cex=1, tck = c(1,0), 
                                  x=xs,
                                  y=ys),
                      colorkey = list(labels=list(cex=1, 
                                                  labels=col.labels.ext2)))  
  leg.list <- p[[i]]$legend$right$args$key
  leg.list$cex <- 1.5
  leg <-  grob(draw.colorkey(leg.list))
  p[[i]] <- update(p[[i]], legend=NULL)
}

# Assign sludge production graphs to layout
gs[7] <- grob(textGrob(expression('Sludge Production w/ respect to Base Case'), gp=gpar(cex=1.25)))
gs[8] <- leg
gs[9:11] <- p
gs[12] <- grob(textGrob(paste(fig.no, fig.labels[1], sep=''), gp=gpar(fontface='bold')))
gs[13] <- grob(textGrob(paste(fig.no, fig.labels[2], sep=''), gp=gpar(fontface='bold')))
gs[14] <- grob(textGrob(paste(fig.no, fig.labels[3], sep=''), gp=gpar(fontface='bold')))
grid.arrange(grobs = gs, layout_matrix = lay)
dev.off()

#= Plot external carbon addition
png('code/figures/External Carbon.png', width=8.5, height=3, units='in', res=500)
fig.no <- 0
for (i in 1:(length(result)-1)) {
  d <- select(result[[i+1]], Nitrogen, Carbon, COD.added)
  if (i > 1) {x_tck <- xs} else {x_tck <- xs_0}
  p[[i]] <- levelplot(COD.added ~ Nitrogen * Carbon, data=d,
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
  leg.list$cex <- 1.5
  leg <-  grob(draw.colorkey(leg.list))
  p[[i]] <- update(p[[i]], legend=NULL)
}

# Assign graphs to layout
gs[7] <- grob(textGrob(expression('External Carbon Addition w/ respect to Base Case'), gp=gpar(cex=1.25)))
gs[8] <- leg
gs[9:11] <- p
gs[12] <- grob(textGrob(paste(fig.no, fig.labels[1], sep=''), gp=gpar(fontface='bold')))
gs[13] <- grob(textGrob(paste(fig.no, fig.labels[2], sep=''), gp=gpar(fontface='bold')))
gs[14] <- grob(textGrob(paste(fig.no, fig.labels[3], sep=''), gp=gpar(fontface='bold')))
grid.arrange(grobs = gs, layout_matrix = lay)
dev.off()

#= Plot Methane Production
png('code/figures/Methane Production.png', width=8.5, height=3, units='in', res=500)
fig.no <- 5
for (i in 1:(length(result)-1)) {
  d <- select(result[[i+1]], Nitrogen, Carbon, CH4.toburn)
  d$CH4.toburn[d$CH4.toburn>5] <- 5
  p[[i]] <- levelplot(CH4.toburn ~ Nitrogen * Carbon, data=d,
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
  leg.list$cex <- 1.5
  leg <-  grob(draw.colorkey(leg.list))
  p[[i]] <- update(p[[i]], legend=NULL)
}

# Assign graphs to layout
gs[7] <- grob(textGrob(expression('Methane Production w/ respect to Base Case'), gp=gpar(cex=1.25)))
gs[8] <- leg
gs[9:11] <- p
gs[12] <- grob(textGrob(paste(fig.no, fig.labels[1], sep=''), gp=gpar(fontface='bold')))
gs[13] <- grob(textGrob(paste(fig.no, fig.labels[2], sep=''), gp=gpar(fontface='bold')))
gs[14] <- grob(textGrob(paste(fig.no, fig.labels[3], sep=''), gp=gpar(fontface='bold')))
grid.arrange(grobs = gs, layout_matrix = lay)
dev.off()


#= Plot GHG Emissions
png('code/figures/GHG.png', width=8.5, height=3, units='in', res=500)
fig.no <- 6
for (i in 1:(length(result)-1)) {
  d <- select(result[[i+1]], Nitrogen, Carbon, CO2.equivs)
  d$CO2.equivs[d$CO2.equivs>5] <- 5
  p[[i]] <- levelplot(CO2.equivs ~ Nitrogen * Carbon, data=d,
                      panel = function(...){
                        panel.levelplot(...)
                        panel.abline(v = seq(0,Nmax-1, by=15), alpha=0.5)
                        panel.abline(h = seq(0,CODmax-1, by=50), alpha=0.5)
                      },
                      at=c(seq(-1,1, length=100),seq(1+.01, 5, length=50)),
                      col.regions=col.percent.ext, 
                      xlab="", ylab="",
                      scales=list(cex=1, tck = c(1,0), 
                                  x=xs,
                                  y=ys),
                      colorkey = list(labels=list(cex=1, 
                               labels=col.labels.ext1)))
  leg.list <- p[[i]]$legend$right$args$key
  leg.list$cex <- 1.5
  leg <-  grob(draw.colorkey(leg.list))
  p[[i]] <- update(p[[i]], legend=NULL)
}

# Assign GHG Production graphs to layout
gs[7] <- grob(textGrob(expression('GHG Emissions w/ respect to Base Case'), gp=gpar(cex=1.25)))
gs[8] <- leg
gs[9:11] <- p
gs[12] <- grob(textGrob(paste(fig.no, fig.labels[1], sep=''), gp=gpar(fontface='bold')))
gs[13] <- grob(textGrob(paste(fig.no, fig.labels[2], sep=''), gp=gpar(fontface='bold')))
gs[14] <- grob(textGrob(paste(fig.no, fig.labels[3], sep=''), gp=gpar(fontface='bold')))
grid.arrange(grobs = gs, layout_matrix = lay)
dev.off()

# TODO: 
# Supplemental Calcs


