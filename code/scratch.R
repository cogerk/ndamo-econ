# MC Analysis
cNin <- rlnorm(n = 100, meanlog=log(46/sqrt(1+13^2/46^2)), sdlog=sqrt(log(1+13^2/46^2)))
cCODin <- rlnorm(n = 100, meanlog=log(266.5/sqrt(1+69.6^2/266.5^2)), sdlog=sqrt(log(1+69.6^2/266.5^2)))
MC_analysis <- scenarios(Q,cNin,cCODin, compare=FALSE)


#vary.N.data <- scenarios(average(Data$AverageFlow),cNin,cCODin, compare=FALSE)



set.seed(420)
syn.data<-generate.data(n=500)

syn.results <- scenarios(syn.data$flow*3.78541,syn.data$TN,syn.data$BOD, 
                         expand=FALSE, compare=FALSE)

ymax <- 50000
ymax.sludge <-15000
par(mfcol=c(4,2), mar=c(0,3,1,0), oma=c(5,3,0,0))
hist(result_range$A$O2.demand, xaxt='n', yaxt='n', 
     main=NA, breaks=10, xlim=c(0,ymax))
axis(side=1, labels = NA)
title(ylab='Conventional', line=1, cex.lab=1.75)
hist(result_range$B$O2.demand, xaxt='n',yaxt='n',  ylab='Anammox', 
     main=NA, breaks=10, xlim=c(0,ymax))
axis(side=1, labels = NA)
title(ylab='Anammox', line=1, cex.lab=1.75)
hist(result_range$C$O2.demand, xaxt='n', yaxt='n', ylab='Anammox-NDAMO',
     main=NA, breaks=10, xlim=c(0,ymax))
axis(side=1, labels = NA)
title(ylab=c('Anammox/','','NDAMO'), line=1,cex.lab=1.75)
hist(result_range$D$O2.demand, yaxt='n', ylab='Anammox-NDAMO-AnMBR',
     main=NA, breaks=10, xlim=c(0,ymax), cex.axis=1.75)
axis(side=1, labels = NA)
title(xlab='Oxygen Demand, kgO2/Day', line=3, cex.lab=1.75, outer=TRUE, adj=.2)
title(ylab=c('Anammox/','','NDAMO/AnMBR'), line=1, cex.lab=1.75)

par( mar=c(0,1,1,2))
hist(result_range$A$sludge.out, xaxt='n', yaxt='n', 
     main=NA, breaks=10, xlim=c(0,ymax.sludge))
axis(side=1, labels = NA)
title(ylab='Conventional', line=1, cex.lab=1.75)
hist(result_range$B$sludge.out, xaxt='n',yaxt='n',
     main=NA, breaks=10, xlim=c(0,ymax.sludge))
axis(side=1, labels = NA)
hist(result_range$C$sludge.out, xaxt='n', yaxt='n',
     main=NA, breaks=10, xlim=c(0,ymax.sludge))
axis(side=1, labels = NA)
hist(result_range$D$sludge.out, yaxt='n',
     main=NA, breaks=10, xlim=c(0,ymax.sludge), cex.axis=1.75)
axis(side=1, labels = NA)
title(xlab='Sludge Production, kgVSS/Day', line=3, cex.lab=1.75, outer=TRUE, adj=.8)
title(ylab='Scenario', cex.lab=1.75, outer=TRUE, line=1, cex.lab=2.25)




# Notes 
# And then all this stuff s
sum(QR.bod.residuals^2)
sum(flow.bod$residuals^2)
Data <- Data[order(Data$Date),]
pdf('figs/report_figure4.pdf')
par(opar)
par(mar=c(4,5,1,1))
plot(TN~Date, data=Data, type='l',
     ylab='Total Nitrogen, mg/L', xlab='', cex.lab=1.5, cex.axis=1.5, pch=20, cex=1.5,
     lwd=3)
title(xlab='Date', line=3, cex.lab=1.5)
dev.off()

Data <- Data[order(Data$Date),]
pdf('figs/report_figure5.pdf', width=14)
par(mfcol=c(1,2), oma=c(0,0,2,1), mar=c(4,5,1,1))
plot(TN~Date, data=Data, type='l',
     ylab='Total Nitrogen, mg/L', xlab='', cex.lab=2, cex.axis=2,pch=20, cex=1.5,
     lwd=3)
title(main='Nitrogen Concentration Over Time',  outer=TRUE, cex.main=3)
title(xlab='Date', line=3, cex.lab=2)
legend(x='topleft', legend=c('Raw Data','Modeled Data'),
       lty=1:2, pch=NA, bty="n", cex=c(1.25,1.25), x.intersp = .25,lwd=3)

Data <- Data[order(Data$AverageFlow),]
n <- length(Data$AverageFlow)
i <- 1:n
p <- (i - 0.4)/(n + 0.2)
plot(TN~AverageFlow,data=Data,
     cex.axis=1.5, cex.lab=1.5, pch=20, cex=1.5, xlab='Average Flowrate, MGD')
lines(sort(Data$AverageFlow, decreasing = TRUE),sort(Data$TN), lwd=2)

par(mar=c(5,1,0,1), oma=c(1,4,3,1))
# Pick out an average flow
p_find = runif(1)
q <- approx(p, Data$AverageFlow, p_find)
q$y
# Find quantile value of that 
q$x
# Find correspond N value
approx(p,Data$TN,p_find)

approx()
?approx
plot(Data$AverageFlow, p, xlab='Flowrate, MGD', ylab=NA, pch=20, cex=1.5,
     cex.axis=2, cex.lab=2)


flow.TN.lindate<-lm(TN~AverageFlow+
                      as.numeric(format(Data$Date,"%m"))+
                      as.numeric(format(Data$Date,"%y")),data=Data)
lines(Data$Date, flow.TN.lindate$fitted.values, lty=2, lwd=3)
plot(Data$Date, flow.TN.lindate$residuals, ylab='', xlab='', cex.axis=2, pch=20, cex=1.5)
title(ylab='Nitrogen-Time Model Residuals', line=3, cex.lab=2)
title(xlab='Date', line=3, cex.lab=2)
lines(c(0,max(Data$Date)),c(0,0), lty=2, lwd=3)
dev.off()



#Conclusion, BOD can be predicted by flowrate, but TN cannot (at least not well enough for our purposes)
# Use Cunnane Plotting Position to generate PDFs for TN and Average Flow, 
par(opar)
pdf('figs/figure5.pdf', width=9)
layout(matrix(c(1,2,
                3,3),nrow=2, ncol=2, byrow=TRUE))
n <- length(Data$AverageFlow)
i <- 1:n
p <- (i - 0.4)/(n + 0.2)
par(mar=c(5,1,0,1), oma=c(1,4,3,1))
plot(sort(Data$AverageFlow), p, xlab='Flowrate, MGD', ylab=NA, pch=20, cex=1.5,
     cex.axis=2, cex.lab=2)
title(ylab='Cumulative Probability', line=2, cex.lab=2, outer=TRUE, adj=0.95)
plot(sort(Data$TN), p, xlab='Nitrogen, mg/L', ylab=NA, yaxt='n', pch=20, cex=1.5,
     cex.axis=2,cex.lab=2)
title(main='How to Generate Synthetic Input Data', outer=TRUE, cex.main=3)
plot(Data$AverageFlow, Data$AverageBOD, main=NA, ylab=NA, cex.axis=2,
     pch=20, cex=1.5,xlab='Flowrate, MGD', cex.lab=2)
title(ylab='Carbon, mg/L', line=2, cex.lab=2, outer=TRUE, adj=0.25)
# Add the trendline
conf_interval <- predict(flow.bod, interval="confidence",
                         level = 0.95)
lines(Data$AverageFlow, conf_interval[,1], main=NA, ylab=NA)
lines(Data$AverageFlow, conf_interval[,2], main=NA, ylab=NA, col=2)
lines(Data$AverageFlow, conf_interval[,3], main=NA, ylab=NA, col=2)
dev.off()

par(opar)

# Plot all data
pdf('figs/fig0.pdf',width=7*1.5, height=1.15*7)
layout(matrix(c(1)))
par(mar=c(5,6,6,1))
Data <- Data[order(Data$Date),]
matplot(Data[,1],Data[,2:4],
        type='l', lty=1, lwd=3, 
        xlab='Date', xaxt='n', 
        ylab='MGD or mg/L',
        cex.lab=2, cex.axis=2)

title(main=c('King County South','', 'Wastewater Treatment Plant Data'), cex.main=2.5, line=2)
axvals <-quantile(Data$Date,seq(0,1,0.2))
axlabels <- format(axvals, '%Y')
axis(1,at=axvals,labels=axlabels, cex.axis=2)
legend('topleft',
       legend=c('Flowrate, MGD','Carbon Concentration, mg/L', 'Nitrogen Concentration, mg/L'),
       col=1:3,
       lty=1,
       lwd=3,
       bty='n',
       cex=c(1.75,1.75), x.intersp = .3, inset=c(-0.02, -0.02))
dev.off()
?matplot

# Plot with date as color to make sure this isn't working
plotclr <- c(colorRampPalette(c("black", "blue"))(50),
             colorRampPalette(c("blue", "purple", "orange"))(55),
             colorRampPalette(c("orange", "red", "darkred"))(70))

# By Date
plot(Data$AverageFlow,Data$TN, type = "n", xlab='Flowrate',
     ylab='Total Nitrogen, mg/L', main='Nitrogen Concentration Over Time') # create new plot

z_scl <- (unclass(Data$Date)-unclass(min(Data$Date)))/(unclass(max(Data$Date))-unclass(min(Data$Date)))
color_scl = round(z_scl*length(plotclr))
color_scl[color_scl == 0] = 1
for(i in 1:length(Data$Date)){ 
  points(Data$AverageFlow[i], Data$TN[i], pch = 20, col = plotclr[color_scl[i]], cex = 2.5)
}

# By month
layout(matrix(c(1, 1, 1, 1, 2),nrow=1))
par(mar=c(4, 5, 0, 1), oma=c(0,0,3,0))
plot(Data$AverageFlow,Data$TN, type = "n", xlab='Flowrate',
     ylab='Total Nitrogen, mg/L', cex.lab=1.5, cex.axis=1.5) # create new plot
title(main='Nitrogen Concentration Over Time', cex.main=2, line=1,outer=TRUE)
z_scl <- as.numeric(format(Data$Date,"%m"))/12
color_scl = round(z_scl*length(plotclr))
color_scl[color_scl == 0] = 1
for(i in 1:length(Data$Date)){ 
  points(Data$AverageFlow[i], Data$TN[i], pch = 20, col = plotclr[color_scl[i]], cex = 2.5)
}
par(mar=c(2, 4, 0, 1))
color.bar <- function(lut, min, max=-min, nticks=11, 
                      ticks=seq(min, max, len=nticks), title='',
                      lbls=NA) {
  scale = (length(lut)-1)/(max-min)
  plot(c(0,10), c(min,max), type='n', bty='n', xaxt='n', xlab='', yaxt='n', ylab='', main=title)
  if (is.na(lbls)){
    axis(2, ticks, las=1, cex.axis=1.5
    )
  }
  else{
    axis(2, ticks, labels = lbls, las=1, cex.axis=1.5)
  }
  for (i in 1:(length(lut)-1)) {
    y = (i-1)/scale + min
    rect(0,y,10,y+1/scale, col=lut[i], border=NA)
  }
}
tcks =c(1,4,8,12)
color.bar(plotclr, min=1, max=12, ticks=tcks, lbls=month.abb[tcks])
?axis
seq(1,12,2)
par(opar)




# For plotting lots of heat maps

library(latticeExtra)
library(gridExtra)
library(ggplot2)
library(grid)

col.l.o2 <- colorRampPalette(c('blue', 'white', 'red'))
p <-list()
for (i in 1:(length(result)-1)) {
  d <- select(result[[i+1]], Nitrogen, Carbon, O2.demand, sludge.out)
  p[[i]] <- levelplot(O2.demand ~ Nitrogen * Carbon, data=d, 
                      at=seq(-1,1,by=.01),col.regions=col.l.o2, main=paste('O2',i))
  p[[(i+3)]] <- levelplot(sludge.out ~ Nitrogen * Carbon, data=d, 
                          at=seq(-1,1,by=.01),col.regions=col.l.o2,main=paste('sludge',i))
}


grid_arrange_shared_legend <- function(..., ncol = length(list(...)), nrow = 1, position = c("bottom", "right")) {
  
  plots <- list(...)
  position <- match.arg(position)
  g <- ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  lwidth <- sum(legend$width)
  gl <- lapply(plots, function(x) x + theme(legend.position="none"))
  gl <- c(gl, ncol = ncol, nrow = nrow)
  
  combined <- switch(position,
                     "bottom" = arrangeGrob(do.call(arrangeGrob, gl),
                                            legend,
                                            ncol = 1,
                                            heights = unit.c(unit(1, "npc") - lheight, lheight)),
                     "right" = arrangeGrob(do.call(arrangeGrob, gl),
                                           legend,
                                           ncol = 2,
                                           widths = unit.c(unit(1, "npc") - lwidth, lwidth)))
  
  grid.newpage()
  grid.draw(combined)
  
  # return gtable invisibly
  invisible(combined)
  
}

do.call(grid.arrange, c(p, ncol=3))


library(ggplot2)
library(gridExtra)
library(grid)
my_fun <- function(id){
  plot(1,1) + ggtitle(paste(id, "hours-feed"))
}

pl <- lapply(seq_len(10), my_fun)

lg <- tableGrob(c("", "26ppm", "39ppm"), theme= ttheme_minimal())
rg <- arrangeGrob(grobs = p, ncol=3,
                  top = textGrob('Medium',gp=gpar(fontsize=18)))


n.grobs <- 12
gs <- lapply(1:n.grobs, function(ii) 
  grobTree(textGrob(ii)))

gs[1:6] <- p
#gs[9] <- textGrob('Oxygen Demand')
lay <- rbind(c(7,7,7,7,7,7,7,7,7,7,7,7),
             c(11,12,9,9,9,9,9,9,9,9,9,8),
             c(11,12,1,1,1,2,2,2,3,3,3,8),
             c(11,12,1,1,1,2,2,2,3,3,3,8),
             c(11,12,1,1,1,2,2,2,3,3,3,8),
             c(11,12,10,10,10,10,10,10,10,10,10,8),
             c(11,12,4,4,4,5,5,5,6,6,6,8),
             c(11,12,4,4,4,5,5,5,6,6,6,8),
             c(11,12,4,4,4,5,5,5,6,6,6,8))
grid.arrange(grobs = gs, layout_matrix = lay)

grid.newpage()
grid.draw(cbind(lg, rg, size = "last"))

# result.process <- function(scenario.names, Q, cCODin,cNin) {
#   result <- scenarios(Q,cCODin,cNin)
#   for (i in length(scenario.names)) {
#     df.O2 <- result[[1]] %>%
#       select(Nitrogen, Carbon, O2.demand) %>%
#       spread(Nitrogen,O2.demand)
#     df <- df$Flowrate
#   }
# }


# GG Plot Arranagements
gs <- lapply(1:8, function(ii) 
  grobTree(rectGrob(gp=gpar(fill=ii, alpha=0.5)), textGrob(ii)))
gs[1:3] <- p[1:3]
gs[4] <- grob(textGrob(seq(50,CODmax, by=50), y=seq(0.15, 1, length.out = 8), x=.75, just = 'right'))
gs[5] <- grob(textGrob('Oxygen Demand, kgO2/day'))
gs[6] <- grob(textGrob('COD Concentration in Influent, mg/L', rot=90))
gs[7] <- grob(textGrob('Total Nitrogen Concentration in Influent, mg/L'))
gs[8] <- leg
grid.arrange(grobs = gs, layout_matrix = lay)
