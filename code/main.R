# Run all reasonable parameters
setwd(paste(getwd(), '/code', sep = ''))
source('masterrun.R')
source('MonteCarloGenerationBuilder.R')


#== Set up conditions to run
Nmax <- 60
CODmax <- 400
Qmax <- 600
cNin <- seq(Nmax*.01, Nmax, length=100) # Nitrogen concentration varies
cCODin <- seq(CODmax*.01, CODmax, length=100) # Carbon concentration varies
Q <- 60 # constant flow rate

#== Run simultions
result <- scenarios(Q,cNin,cCODin, compare=TRUE)


#== Plot Figures
library(latticeExtra)
library(gridExtra)
library(ggplot2)
library(grid)

#== External Carbon Addition
# Set up Margins
lw <- list(left.padding = list(x = 0, units = "inches"))
lw$right.padding <- list(x = 0, units = "inches")
lh <- list(bottom.padding = list(x = 0, units = "inches"))
lh$top.padding <- list(x = 0, units = "inches")
lattice.options(layout.widths = lw, layout.heights = lh)

# Setup up scales
col.carbon <- colorRampPalette(c('blue', 'white'))
ys<-list(at=seq(0,CODmax, by=50), seq(0,CODmax, by=50))
xs<-list(at=seq(0,Nmax, by=15), labels=seq(0,Nmax, by=15))

# Plot
#png('figures/External Carbon.png', width=4, height=5, units='in', res=500)
d <- select(result$D, Nitrogen, Carbon, COD.added)
levelplot(COD.added ~ Nitrogen * Carbon, data=d,
          main='% Reduction of Carbon Addition in Anammox/NDAMO/AnMBR Treatment',
          xlab='Total Nitrogen Concentration in Influent, mg/L',
          ylab='COD Concentration in Influent, mg/L',
          at=seq(-1,-0.95, by=0.001), col.regions=col.carbon,
          panel = function(...){
            panel.levelplot(...)
            panel.abline(v = seq(0,Nmax-1, by=15), alpha=0.5)
            panel.abline(h = seq(0,CODmax-1, by=50), alpha=0.5)},
          scales=list(cex=0.75, tck = c(1,0), 
                      x=xs,
                      y=ys))



levelplot(COD.added ~ Nitrogen * Carbon, data=result$A,
          main='Carbon Addition Saved By Alternative Treatment',
          xlab='Total Nitrogen Concentration in Influent, mg/L',
          ylab='COD Concentration in Influent, mg/L',
          at=seq(0, 25000, by=1), col.regions=col.carbon, 
          scales=list(cex=0.75, tck = c(1,0), 
                      x=xs,
                      y=ys),
          colorkey = list(c('0 kgCOD/day', 5000, 10000, 15000, 20000, 25000),
                          at=seq(0,25000, by=5000)) ,
          panel = function(...){
            panel.levelplot(...)
            panel.abline(v = seq(0,Nmax-1, by=15), alpha=0.5)
            panel.abline(h = seq(0,CODmax-1, by=50), alpha=0.5)})
dev.off()

#== Plot Scenarios
# Setup the plot margins

lw <- list(left.padding = list(x = -.33, units = "inches"))
lw$right.padding <- list(x = -.23, units = "inches")
lh <- list(bottom.padding = list(x = -.25, units = "inches"))
lh$top.padding <- list(x = -.325, units = "inches")
lattice.options(layout.widths = lw, layout.heights = lh)

# Set up labels
col.labels <- c('-100%', '-50%', '0%','50%','100%')
col.percent <- colorRampPalette(c('blue', 'white', 'red'))
ys<-list(at=seq(0,CODmax, by=50), labels=rep('', length(seq(0,CODmax, by=10))))
gs <- list()
gs[1] <- grob(textGrob('SHARON vs. MLE'))
gs[2] <- grob(textGrob(c('SHARON/NDAMO', 'vs. MLE'), y=c(0.8,0.33)))
gs[3] <- grob(textGrob(c('SHARON/NDAMO/AnMBR','vs. MLE'), y=c(0.8,0.33)))
gs[4] <- grob(textGrob('Total Nitrogen Concentration in Influent, mg/L'))
gs[5] <- grob(textGrob(seq(50,CODmax, by=50), y=seq(0.15, 1, length.out = 8), x=.75, just = 'right'))
gs[6] <- grob(textGrob('COD Concentration in Influent, mg/L', rot=90))


# Set up plot layout
lay <- rbind(rep(7, times=20),
             c(6,5,rep(1:3,each=5), rep(8,times=3)),
             matrix(rep(c(6,5,rep(9:11, each=5),rep(8, times=3)),times=6), 
                    nrow=6, 
                    byrow=T),
             rep(4, times=20))

#== Plot Oxygen Demand
png('figures/O2 Demand.png', width=8.5, height=3, units='in', res=500)
p <- list()
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
                                                  labels=col.labels)))
  
  leg.list <- p[[i]]$legend$right$args$key
  leg.list$cex <- 1.5
  leg <-  grob(draw.colorkey(leg.list))
  p[[i]] <- update(p[[i]], legend=NULL)
}

# Assign O2 Demand graphs to layout
gs[7] <- grob(textGrob(expression('Oxygen Demand'), gp=gpar(cex=1.25)))
gs[8] <- leg
gs[9:11] <- p
grid.arrange(grobs = gs, layout_matrix = lay)
dev.off()


#== Plot Oxygen Demand
png('figures/External Carbon.png', width=8.5, height=3, units='in', res=500)


p <- list()
for (i in 1:(length(result)-1)) {
  d <- select(result[[i+1]], Nitrogen, Carbon, COD.added)
  p[[i]] <- levelplot(COD.added ~ Nitrogen * Carbon, data=d,
                      panel = function(...){
                        panel.levelplot(...)
                        panel.abline(v = seq(0,Nmax-1, by=15), alpha=0.5)
                        panel.abline(h = seq(0,CODmax-1, by=50), alpha=0.5)
                      },
                      at=seq(-1,-.9,by=.01), col.regions=col.percent, 
                      xlab="", ylab="",
                      scales=list(cex=1, tck = c(1,0), 
                                  x=xs,
                                  y=ys))
  
  leg.list <- p[[i]]$legend$right$args$key
  leg.list$cex <- 1.5
  leg <-  grob(draw.colorkey(leg.list))
  p[[i]] <- update(p[[i]], legend=NULL)
}

# Assign O2 Demand graphs to layout
gs[7] <- grob(textGrob(expression('Oxygen Demand'), gp=gpar(cex=1.25)))
gs[8] <- leg
gs[9:11] <- p
grid.arrange(grobs = gs, layout_matrix = lay)
dev.off()


#== Plot Sludge Production 
png('figures/Sludge Production.png', width=8.5, height=3, units='in', res=500)
for (i in 1:(length(result)-1)) {
  d <- select(result[[i+1]], Nitrogen, Carbon, sludge.out)
  p[[i]] <- levelplot(sludge.out ~ Nitrogen * Carbon, data=d,
                      panel = function(...){
                        panel.levelplot(...)
                        panel.abline(v = seq(0,Nmax-1, by=15), alpha=0.5)
                        panel.abline(h = seq(0,CODmax-1, by=50), alpha=0.5)
                      },
                      at=seq(-1,1,by=.01),col.regions=col.percent, 
                      xlab="", ylab="",
                      scales=list(cex=1, tck = c(1,0), 
                                  x=xs,
                                  y=ys),
                      colorkey = list(labels=list(cex=1, 
                                                  labels=col.labels)))
  
  leg.list <- p[[i]]$legend$right$args$key
  leg.list$cex <- 1.5
  leg <-  grob(draw.colorkey(leg.list))
  p[[i]] <- update(p[[i]], legend=NULL)
}

# Assign Sludge Production graphs to layout
gs[7] <- grob(textGrob(expression('Sludge Production'), gp=gpar(cex=1.25)))
gs[8] <- leg
gs[9:11] <- p
grid.arrange(grobs = gs, layout_matrix = lay)
dev.off()

# TODO: 
# Greenhouse gas image
# External Carbon Image
# Supplemental Calcs
# M&M Review & Submit

