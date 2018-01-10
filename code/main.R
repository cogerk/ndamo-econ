#=== main.R
#== Load necessary files 
setwd(paste(getwd(), '/code', sep = ''))
source('masterrun.R')
source('MonteCarloGenerationBuilder.R')
library(latticeExtra)
library(gridExtra)
library(ggplot2)
library(grid)


#== Set up conditions to run
Nmax <- 60
CODmax <- 400
Qmax <- 600
cNin <- seq(Nmax*.001, Nmax, length=100) # Nitrogen concentration varies
cCODin <- seq(CODmax*.001, CODmax, length=100) # Carbon concentration varies
Q <- 60 # constant flow rate

#== Run simultions
result <- scenarios(Q,cNin,cCODin, compare=TRUE)


#== Plot Figures
# #= External Carbon Addition
# # Set up Margins
# lw <- list(left.padding = list(x = 0, units = "inches"))
# lw$right.padding <- list(x = 0, units = "inches")
# lh <- list(bottom.padding = list(x = 0, units = "inches"))
# lh$top.padding <- list(x = 0, units = "inches")
# lattice.options(layout.widths = lw, layout.heights = lh)
# 
# # Set up scales
# col.carbon <- colorRampPalette(c('blue', 'light blue', rep('white', times=78)))
 ys <- list(at=seq(0,CODmax, by=50), seq(0,CODmax, by=50))
 xs <- list(at=seq(15,Nmax, by=15), labels=seq(15,Nmax, by=15))
 xs_0 <- list(at=seq(0,Nmax, by=15), labels=seq(0,Nmax, by=15))
# 
# # Plot
# #png('figures/External Carbon.png', width=4, height=5, units='in', res=500)
# # natural gas is 10 cents per kWh 
# # methane is 14.5 kwh/kg
# # Won't include this plot, it's not very interesting.
# d <- select(result$D, Nitrogen, Carbon, COD.added)
# levelplot(COD.added ~ Nitrogen * Carbon, data=d, col.regions=col.carbon,
#           main='% Reduction of Carbon Addition in Anammox/NDAMO/AnMBR Treatment',
#           xlab='Total Nitrogen Concentration in Influent, mg/L',
#           ylab='COD Concentration in Influent, mg/L')




# Setup the plot margins
lw <- list(left.padding = list(x = -.33, units = "inches"))
lw$right.padding <- list(x = -.23, units = "inches")
lh <- list(bottom.padding = list(x = -.25, units = "inches"))
lh$top.padding <- list(x = -.325, units = "inches")
lattice.options(layout.widths = lw, layout.heights = lh)

# Set up labels
col.labels <- c('-100%', '-50%', '0%','50%','100%')
col.labels.ext1 <- c('-100%', '0%', '100%','200%','300%','400%','>500%')
col.percent <- colorRampPalette(c('blue', 'white', 'red'))
col.percent.ext <- colorRampPalette(c('blue', 'white', 'red', 'darkred'))
ys<-list(at=seq(0,CODmax, by=50), labels=rep('', length(seq(0,CODmax, by=10))))
gs <- list()
gs[1] <- grob(textGrob('SHARON vs. MLE'))
gs[2] <- grob(textGrob(c('SHARON/NDAMO', 'vs. MLE'), y=c(0.8,0.33)))
gs[3] <- grob(textGrob(c('SHARON/NDAMO/AnMBR','vs. MLE'), y=c(0.8,0.33)))
gs[4] <- grob(textGrob('Total Nitrogen Concentration in Influent, mg/L'))
gs[5] <- grob(textGrob(seq(0,CODmax, by=50), y=seq(0.12, .85, length.out = 8), x=.75, just='right'))
gs[6] <- grob(textGrob('COD Concentration in Influent, mg/L', rot=90))


# Set up plot layout
lay <- rbind(rep(7, times=20),
             c(6,5,rep(1:3,each=5), 8, 8, 8),
             matrix(rep(c(6,5,rep(9:11, each=5),rep(8, times=3)),times=6), 
                    nrow=6, 
                    byrow=T),
             rep(4, times=20))


#= Plot Oxygen Demand
png('figures/O2 Demand.png', width=8.5, height=3, units='in', res=500)
p <- list()
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

# Assign O2 Demand graphs to layout
gs[7] <- grob(textGrob(expression('Oxygen Demand w/ respect to Base Case'), gp=gpar(cex=1.25)))
gs[8] <- leg
gs[9:11] <- p
grid.arrange(grobs = gs, layout_matrix = lay)
dev.off()


#= Plot Sludge Production 
png('figures/Sludge Production.png', width=8.5, height=3, units='in', res=500)
for (i in 1:(length(result)-1)) {
  d <- select(result[[i+1]], Nitrogen, Carbon, sludge.out)
  p[[i]] <- levelplot(sludge.out ~ Nitrogen * Carbon, data=d,
                      panel = function(...){
                        panel.levelplot(...)
                        panel.abline(v = seq(0,Nmax-1, by=15), alpha=0.5)
                        panel.abline(h = seq(0,CODmax-1, by=50), alpha=0.5)
                      },
                      at=seq(-1,1,by=.01),
                      col.regions=col.percent, 
                      xlab="", ylab="",
                      scales=list(cex=1, tck = c(1,0), 
                                  x=xs,
                                  y=ys),
                      colorkey = NULL)
}

# Assign Sludge Production graphs to layout
gs[7] <- grob(textGrob(expression('Sludge Production w/ respect to Base Case'), gp=gpar(cex=1.25)))
gs[8] <- leg
gs[9:11] <- p
grid.arrange(grobs = gs, layout_matrix = lay)
dev.off()


#= Plot Methane Production
png('figures/Methane Production.png', width=8.5, height=3, units='in', res=500)
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

# Assign Methane Production graphs to layout
gs[7] <- grob(textGrob(expression('Methane Production w/ respect to Base Case'), gp=gpar(cex=1.25)))
gs[8] <- leg
gs[9:11] <- p
grid.arrange(grobs = gs, layout_matrix = lay)
dev.off()


#= Plot GHG Emissions
png('figures/GHG.png', width=8.5, height=3, units='in', res=500)
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
grid.arrange(grobs = gs, layout_matrix = lay)
dev.off()

# TODO: 
# External carbon addition paragraph
# Supplemental Calcs


