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
lab1 <- textGrob('CANON vs. MLE', gp=gpar(fontfamily='serif'))
lab2 <- textGrob(c('HRAS/anammox/n-damo', 'vs. MLE'),gp=gpar(fontfamily='serif'), y=c(0.8,0.33))
lab3 <- textGrob(c('AnMBR/anammox/n-damo','vs. MLE'), gp=gpar(fontfamily='serif'), y=c(0.8,0.33))
lab4 <- textGrob('Nitrogen in Influent, mg/L', gp=gpar(fontfamily='serif'))
ax5 <- textGrob(seq(0,CODmax, by=50), y=seq(0.12, .85, length.out = 8), x=.75, just='right', gp=gpar(fontfamily='serif'))
lab6 <- textGrob('COD in Influent, mg/L', rot=90, gp=gpar(fontfamily='serif'))
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
fig.no <- 2
for (i in 1:(length(result)-1)) {
  d <- select(result[[i+1]], Nitrogen, Carbon, O2.demand)
  if (i > 1) {x_tck <- xs} else {x_tck <- xs_0}
  p[[i]] <- levelplot(O2.demand ~ Nitrogen * Carbon, data=d,
                      par.settings=list(axis.text=list(fontfamily="serif")),
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
                                                  labels=col.labels, 
                                                  par.settings=list(fontfamily='serif'))))
  
  leg.list <- p[[i]]$legend$right$args$key
  leg8 <-  draw.colorkey(leg.list)
  leg8$children[[4]]$children[[1]]$gp$fontfamily <- 'serif'
  p[[i]] <- update(p[[i]], legend=NULL)
}


# Assign Oxygen Demand graphs to layout
lab7 <- textGrob(expression('Oxygen Demand w/ respect to Base Case'), gp=gpar(cex=1.25, fontfamily='serif'))
lab12 <- textGrob(paste(fig.no, fig.labels[1], sep=''), gp=gpar(fontface='bold', fontfamily='serif'))
lab13 <- textGrob(paste(fig.no, fig.labels[2], sep=''), gp=gpar(fontface='bold', fontfamily='serif'))
lab14 <- textGrob(paste(fig.no, fig.labels[3], sep=''), gp=gpar(fontface='bold', fontfamily='serif'))
grid.arrange(grobs = list(lab1,lab2,lab3,lab4,ax5, lab6, lab7, leg8, p[[1]], p[[2]],p[[3]], lab12, lab13, lab14), layout_matrix = lay)
 dev.off()


#= Plot Sludge Production
png('code/figures/Sludge Production.png', width=8.5, height=3, units='in', res=500)
fig.no <- 3
for (i in 1:(length(result)-1)) {
  d <- select(result[[i+1]], Nitrogen, Carbon, sludge.out)
  p[[i]] <- levelplot(sludge.out ~ Nitrogen * Carbon, data=d,
                      par.settings=list(axis.text=list(fontfamily="serif")),
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
  leg8 <-  draw.colorkey(leg.list)
  leg8$children[[4]]$children[[1]]$gp$fontfamily <- 'serif'
  p[[i]] <- update(p[[i]], legend=NULL)
}

# Assign sludge production graphs to layout
lab7 <- textGrob(expression('Sludge Production w/ respect to Base Case'), gp=gpar(cex=1.25, fontfamily='serif'))
lab12 <- textGrob(paste(fig.no, fig.labels[1], sep=''), gp=gpar(fontface='bold', fontfamily='serif'))
lab13 <- textGrob(paste(fig.no, fig.labels[2], sep=''), gp=gpar(fontface='bold', fontfamily='serif'))
lab14 <- textGrob(paste(fig.no, fig.labels[3], sep=''), gp=gpar(fontface='bold', fontfamily='serif'))
grid.arrange(grobs = list(lab1,lab2,lab3,lab4,ax5, lab6, lab7, leg8, p[[1]], p[[2]],p[[3]], lab12, lab13, lab14), layout_matrix = lay)
dev.off()

#= Plot external carbon addition
png('code/figures/External Carbon.png', width=8.5, height=3, units='in', res=500)
fig.no <- 0
for (i in 1:(length(result)-1)) {
  d <- select(result[[i+1]], Nitrogen, Carbon, COD.added)
  if (i > 1) {x_tck <- xs} else {x_tck <- xs_0}
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

#= Plot Methane Production
png('code/figures/Methane Production.png', width=8.5, height=3, units='in', res=500)
fig.no <- 4
for (i in 1:(length(result)-1)) {
  d <- select(result[[i+1]], Nitrogen, Carbon, CH4.burn)
  d$CH4.burn[d$CH4.burn>5] <- 5
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
  leg8 <-  draw.colorkey(leg.list)
  leg8$children[[4]]$children[[1]]$gp$fontfamily <- 'serif'
  p[[i]] <- update(p[[i]], legend=NULL)
}

# Assign graphs to layout
lab7 <- textGrob(expression('Methane Production w/ respect to Base Case'), gp=gpar(cex=1.25, fontfamily='serif'))
lab12 <- textGrob(paste(fig.no, fig.labels[1], sep=''), gp=gpar(fontface='bold', fontfamily='serif'))
lab13 <- textGrob(paste(fig.no, fig.labels[2], sep=''), gp=gpar(fontface='bold', fontfamily='serif'))
lab14 <- textGrob(paste(fig.no, fig.labels[3], sep=''), gp=gpar(fontface='bold', fontfamily='serif'))
grid.arrange(grobs = list(lab1,lab2,lab3,lab4,ax5, lab6, lab7, leg8, p[[1]], p[[2]],p[[3]], lab12, lab13, lab14), layout_matrix = lay)
dev.off()


#= Plot GHG Emissions
png('code/figures/GHG.png', width=8.5, height=3, units='in', res=500)
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
                      at=c(seq(-1,1, length=100),seq(1+.01, 5, length=50)),
                      col.regions=col.percent.ext1,
                      xlab="", ylab="",
                      scales=list(cex=1, tck = c(1,0),
                                  x=xs,
                                  y=ys),
                      colorkey = list(labels=list(cex=1,
                               labels=col.labels.ext1)))
  leg.list <- p[[i]]$legend$right$args$key
  leg8 <-  draw.colorkey(leg.list)
  leg8$children[[4]]$children[[1]]$gp$fontfamily <- 'serif'
  p[[i]] <- update(p[[i]], legend=NULL)
}

# Assign GHG Production graphs to layout
lab7 <- textGrob(expression('GHG Emissions w/ respect to Base Case'), gp=gpar(cex=1.25, fontfamily='serif'))
lab12 <- textGrob(paste(fig.no, fig.labels[1], sep=''), gp=gpar(fontface='bold', fontfamily='serif'))
lab13 <- textGrob(paste(fig.no, fig.labels[2], sep=''), gp=gpar(fontface='bold', fontfamily='serif'))
lab14 <- textGrob(paste(fig.no, fig.labels[3], sep=''), gp=gpar(fontface='bold', fontfamily='serif'))
grid.arrange(grobs = list(lab1,lab2,lab3,lab4,ax5, lab6, lab7, leg8, p[[1]], p[[2]],p[[3]], lab12, lab13, lab14), layout_matrix = lay)
dev.off()


# Define High/Med/Low COD/N ratio
qqO2.B = quantile(result$B$O2.demand, probs = seq(0, 1, 0.3333))
qqO2.C = quantile(result$C$O2.demand, probs = seq(0, 1, 0.3333))
qqO2.D = quantile(result$D$O2.demand, probs = seq(0, 1, 0.3333))



