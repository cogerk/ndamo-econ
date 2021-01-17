require(lattice)
require(gridExtra)
require(ggplot2)
require(grid)
require(extrafont)

par(family = 'serif')

# Setup the plot margins
lw <- list(left.padding = list(x = -.33, units = "inches"))
lw$right.padding <- list(x = -.23, units = "inches")
lh <- list(bottom.padding = list(x = -.55, units = "inches"))
lh$top.padding <- list(x = -.325, units = "inches")
lattice.options(layout.widths = lw, layout.heights = lh)


draw_graph <- function(CODmax, Nmax, font, result, select_var, max, min,
                       col.percent, col.bar.range, col.bar.range.lab, 
                       col.bar.range.lab.at, col.bar.lab, 
                       fig.no, graph.title) {
  
  # Setup labels
  ys <- list(at=seq(0,CODmax, by=50), labels=rep('', length(seq(0,CODmax, by=10))))
  xs <- list(at=seq(0, Nmax, by=15), labels=rep('', length(seq(0, Nmax, by=15))))
  gs <- list()
  
  # Plot Titles
  lab1.name <- 'HRAS/anammox vs. MLE'
  lab2.name <- 'AnMBR/anammox vs. MLE'
  lab3.name <- 'HRAS/anammox/n-damo\nvs. MLE'
  lab4.name <- 'AnMBR/anammox/n-damo\nvs. MLE'
  
  
  # Axes
  lab5 <- textGrob('Nitrogen in Influent, mg/L', gp=gpar(fontfamily=font, cex=0.9))
  ax7 <- textGrob(seq(0, Nmax, by=15), x=seq(0, 1.02, length.out = 5), y=.25, just='right', gp=gpar(fontfamily=font, cex=0.9))
  ax8 <- textGrob(seq(15, Nmax, by=15), x=seq(0.255, 1.02, length.out = 4), y=.25, just='right', gp=gpar(fontfamily=font, cex=0.9))
  lab10 <- textGrob('COD in Influent, mg/L', rot=90, gp=gpar(fontfamily=font, cex=0.9), x=.75)
  ax9 <- textGrob(seq(0,CODmax, by=50), y=seq(0, 1, length.out = 9), x=.75, just='right', gp=gpar(fontfamily=font, cex=0.9))
  lab23 <- textGrob(col.bar.lab,
                    gp=gpar(fontfamily=font), rot=270, x=-.25)
  fig.labels <- c('.1', '.2', '.3', '.4')
  
  
  # Set up plot layout
  lay <- as.matrix(read.csv(file = 'code/figures/figure_layout.csv', header=FALSE)) # Scenarios C&D
  p <- list()
  
  for (i in 1:(length(result)-1)) {
    d <- select(result[[i+1]], Nitrogen, Carbon, select_var)
    d[,select_var][d[,select_var]>max] <- max
    d[,select_var][d[,select_var]<min] <- min
    form <- paste(select_var, '~ Nitrogen * Carbon')
    p[[i]] <- levelplot(as.formula(form), data=d,
                        panel = function(...){
                          panel.levelplot(...)
                          panel.abline(v = seq(0,Nmax-1, by=15), alpha=0.5)
                          panel.abline(h = seq(0,CODmax-1, by=50), alpha=0.5)
                        },
                        at=col.bar.range, col.regions=col.percent, 
                        xlab="", ylab="",
                        scales=list(cex=1, tck = c(1,0), 
                                    x=xs,
                                    y=ys),
                        colorkey = list(labels=list(cex=1, 
                                                    labels=col.bar.range.lab, at=col.bar.range.lab.at)))
    
    leg.list <- p[[i]]$legend$right$args$key
    leg13 <-  draw.colorkey(leg.list)
    leg13$children[[6]]$children[[1]]$gp$fontfamily <- font
    p[[i]] <- update(p[[i]], legend=NULL)
  }
  
  
  # Assign Oxygen Demand graphs to layout
  lab14 <- textGrob(graph.title, gp=gpar(cex=1.25, fontfamily=font))
  lab1 <- textGrob(paste(paste(fig.no, fig.labels[1], sep=''), lab1.name), 
                   gp=gpar(fontfamily=font))
  lab2 <- textGrob(paste(paste(fig.no, fig.labels[2], sep=''), lab2.name),
                   gp=gpar(fontfamily=font))
  lab3 <- textGrob(paste(paste(fig.no, fig.labels[3], sep=''), lab3.name),
                   gp=gpar(fontfamily=font))
  lab4 <- textGrob(paste(paste(fig.no, fig.labels[4], sep=''), lab4.name),
                   gp=gpar(fontfamily=font))
  
  return(grid.arrange(grobs = list(lab1, lab2, lab3, lab4,
                            lab5, lab5, ax7, ax8, # X-Axis, Bottom, 5,6,7,8
                            ax9, lab10, ax9, lab10, # Y-Axis left, 9,10,11,12
                            leg13,  # Colorbar, 13
                            lab14, # Figure Name, 14
                            p[[1]], p[[2]],p[[3]], p[[4]], # Plots, 15,16,17,18
                            ax7, ax8, lab5, lab5, # X-Axis, Top, 19,20,21,22
                            lab23, # Colorbar Label, 23
                            lab10, ax9, lab10, ax9, # Y-Axis right, 24,25,26,27
                            textGrob(''), textGrob(''), textGrob(''), textGrob(''), textGrob(''), textGrob(''), textGrob(''), textGrob('') #Gaps
  ), layout_matrix = lay))

}


blank_graph <- function(CODmax, Nmax, font, result, graph.title) {
  # Setup labels
  ys <- list(at=seq(0,CODmax, by=50), labels=rep('', length(seq(0,CODmax, by=10))))
  xs <- list(at=seq(0, Nmax, by=15), labels=rep('', length(seq(0, Nmax, by=15))))
  gs <- list()
  
  # Axes
  lab3 <- textGrob('Nitrogen in Influent, mg/L', gp=gpar(fontfamily=font, cex=0.9))
  ax4 <- textGrob(seq(0, Nmax, by=15), x=seq(0, 1.02, length.out = 5), y=.25, just='right', gp=gpar(fontfamily=font, cex=0.9))
  lab6 <- textGrob('COD in Influent, mg/L', rot=90, gp=gpar(fontfamily=font, cex=0.9), x=.75)
  ax5 <- textGrob(seq(0,CODmax, by=50), y=seq(0, 1, length.out = 9), x=.75, just='right', gp=gpar(fontfamily=font, cex=0.9))
  
  
  # Set up plot layout
  lay <- as.matrix(read.csv(file = 'code/figures/blank_layout.csv', header=FALSE))
  d <- select(result[[1]], Nitrogen, Carbon, Flowrate)
  p <- levelplot(Flowrate/Flowrate ~ Nitrogen * Carbon, data=d,
                      panel = function(...){
                      panel.levelplot(...)
                      panel.abline(v = seq(0,Nmax-1, by=15), alpha=0.5)
                      panel.abline(h = seq(0,CODmax-1, by=50), alpha=0.5)
                      },
                      at=seq(0,2), col.regions=colorRampPalette(c('white', 'white')), 
                      xlab="", ylab="",
                      scales=list(cex=1, tck = c(1,0), 
                                  x=xs,
                                  y=ys))
    p <- update(p, legend=NULL)
  
  lab1 <- textGrob(graph.title, gp=gpar(cex=1.25, fontfamily=font))
  
  return(grid.arrange(grobs = list(lab1, p, # Title and graph
                                   lab3, ax4, # N axis and label
                                   ax5, lab6, # C axix and label
                                   textGrob(''), textGrob(''), textGrob('') #Gaps
  ), layout_matrix = lay))
  
}


