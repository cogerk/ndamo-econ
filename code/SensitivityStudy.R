#=== main.R
#== Load necessary files 
source('code/masterrun.R')
require(lattice)
require(gridExtra)
require(ggplot2)
require(grid)
require(extrafont)

# Graph Font
font <- 'serif'

# Labels
labB.name <- 'HRAS/anammox vs. MLE'
labC.name <- 'AnMBR/anammox vs. MLE'
labD.name <- 'HRAS/anammox/n-damo vs. MLE'
labE.name <- 'AnMBR/anammox/n-damo vs. MLE'
y.ax.GHG <- 'GHG reduced per day compared to base case, kg equivalent CO2/d'
y.ax.cost <- 'Cost saved per day compared to base case, USD/d'

# Helper function to clean resulting dataframes
SS_to_DF <- function(SS) {
  SS <- data.frame(SS) %>% 
    gather(key = ratio, value=kgCO2.d, -e_base) %>%
    separate(ratio, into=c(NA, 'ratio')) %>%
    mutate(ratio=factor(as.numeric(ratio)))
  return(SS)
}
plot_ss <- function(df, lab.name, y.ax.scale){
  ggplot(df, aes(y=kgCO2.d, x=e_base/90, 
                 col=ratio, lty=ratio))  +
    geom_line() + geom_hline(aes(yintercept = 0)) +
    scale_y_continuous(lim=y.ax.scale) +
    theme_bw() +
    scale_x_continuous(limits=c(-0.5,5), label=scales::percent, 
                       breaks=c(-0.5, 0, 1, 2.5, 5)) +
    scale_color_manual(name='COD/N Ratio', values=c(1, 2, 3, 4)) +
    scale_linetype_manual(name='COD/N Ratio',values=c(4, 2, 3, 1)) +
    theme(text=element_text(family=font),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.title=element_blank(),
          plot.title = element_text(hjust = 0.5), 
          legend.position = 'bottom') +
    labs(title=lab.name)
}
g_legend <- function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

# Model parameters
Nmin <- .6
Nmax <- 60
CODmax <- 400
CODmin <- 4
cNin <- seq(Nmin, Nmax, length=100) # Nitrogen concentration varies
cCODin <- seq(CODmin, CODmax, length=100) # Carbon concentration varies
Q <- 60 # constant flow rate
e_base = c(90*5,90*2, 90, 0,-90/2)

# Run Model
result_SS_all <- list()
result_SS <- matrix(nrow=length(e_base), ncol=5, dimnames=list(1:length(e_base), c('e_base', 'r_10', 'r_5', 'r_2.5', 'r_1')))
result_SS_CO2_B <- result_SS
result_SS_CO2_C <- result_SS
result_SS_CO2_D <- result_SS
result_SS_CO2_E <- result_SS
result_SS_cost_B <- result_SS
result_SS_cost_C <- result_SS
result_SS_cost_D <- result_SS
result_SS_cost_E <- result_SS

#c(50, 40, 20, 10), c(100)
#c(40), c(400, 200, 100, 40)
for (i in 1:length(e_base)) {
  #result_SS_all[i] <- scenarios(Q, cNin, cCODin, e_Base= e_base[i], compare=TRUE)
  result <- scenarios(Q, c(50, 40, 20, 10), c(100),  e_Base= e_base[i], compare=TRUE)
  
  result_SS_CO2_B[i,] <- c(e_base[i], result$B$CO2.equivs)
  result_SS_CO2_C[i,] <- c(e_base[i], result$C$CO2.equivs)
  result_SS_CO2_D[i,] <- c(e_base[i], result$D$CO2.equivs)
  result_SS_CO2_E[i,] <- c(e_base[i], result$E$CO2.equivs)
  result_SS_cost_B[i,] <- c(e_base[i], result$B$cost)
  result_SS_cost_C[i,] <- c(e_base[i], result$C$cost)
  result_SS_cost_D[i,] <- c(e_base[i], result$D$cost)
  result_SS_cost_E[i,] <- c(e_base[i], result$E$cost)
  
  }

# Data Clean
result_SS_CO2_B <- SS_to_DF(result_SS_CO2_B)
result_SS_CO2_C <- SS_to_DF(result_SS_CO2_C)
result_SS_CO2_D <- SS_to_DF(result_SS_CO2_D)
result_SS_CO2_E <- SS_to_DF(result_SS_CO2_E)
result_SS_cost_B <- SS_to_DF(result_SS_cost_B)
result_SS_cost_C <- SS_to_DF(result_SS_cost_C)
result_SS_cost_D <- SS_to_DF(result_SS_cost_D)
result_SS_cost_E <- SS_to_DF(result_SS_cost_E)

# Plot

b_plot_GHG <- plot_ss(result_SS_CO2_B, labB.name, c(-50000, 20000))
c_plot_GHG <- plot_ss(result_SS_CO2_C, labC.name, c(-50000, 20000))
d_plot_GHG <- plot_ss(result_SS_CO2_D, labD.name, c(-50000, 20000))
e_plot_GHG <- plot_ss(result_SS_CO2_E, labE.name, c(-50000, 20000))
legend <- g_legend(b_plot_GHG)

grid.arrange(b_plot_GHG + theme(legend.position="none"), 
             c_plot_GHG + theme(legend.position="none"), 
             d_plot_GHG + theme(legend.position="none"), 
             e_plot_GHG + theme(legend.position="none"),
             legend,
             top=textGrob('GHG Emission Sensitivity to Pumping Demand', gp=gpar(fontfamily=font)),
             left=textGrob(y.ax.GHG, gp=gpar(fontfamily=font), rot=90),
             bottom=textGrob('Approx. % Change from Conventional System', 
                             gp=gpar(fontfamily=font)),
             layout_matrix=matrix(c(1,2,1,2,1,2,1,2,1,2,1,2,
                                    3,4,3,4,3,4,3,4,3,4,3,4,5,5), ncol=2, byrow = TRUE))

ggsave('code/figures/ss_GHG.png', grid.arrange(b_plot_GHG + theme(legend.position="none"), 
                                  c_plot_GHG + theme(legend.position="none"), 
                                  d_plot_GHG + theme(legend.position="none"), 
                                  e_plot_GHG + theme(legend.position="none"),
                                  legend,
                                  top=textGrob('GHG Emission Sensitivity to Pumping Demand', gp=gpar(fontfamily=font)),
                                  left=textGrob(y.ax.GHG, gp=gpar(fontfamily=font), rot=90),
                                  bottom=textGrob('Approx. % Change from Conventional System', 
                                                  gp=gpar(fontfamily=font)),
                                  layout_matrix=matrix(c(1,2,1,2,1,2,1,2,1,2,1,2,
                                                         3,4,3,4,3,4,3,4,3,4,3,4,5,5), ncol=2, byrow = TRUE)))


#=== main.R
#== Load necessary files 
source('code/masterrun.R')

# Graph Font
font <- 'serif'

# Labels
labB.name <- 'HRAS/anammox vs. MLE'
labC.name <- 'AnMBR/anammox vs. MLE'
labD.name <- 'HRAS/anammox/n-damo vs. MLE'
labE.name <- 'AnMBR/anammox/n-damo vs. MLE'
y.ax.GHG <- 'GHG reduced per day compared to base case, kg equivalent CO2/d'
y.ax.cost <- 'Cost saved per day compared to base case, USD/d'

# Helper function to clean resulting dataframes
SS_to_DF <- function(SS) {
  SS <- data.frame(SS) %>% 
    gather(key = ratio, value=kgCO2.d, -e_base) %>%
    separate(ratio, into=c(NA, 'ratio')) %>%
    mutate(ratio=factor(as.numeric(ratio)))
  return(SS)
}
plot_ss <- function(df, lab.name, y.ax.scale){
  ggplot(df, aes(y=kgCO2.d, x=e_base/90, 
                 col=ratio, lty=ratio))  +
    geom_line() + geom_hline(aes(yintercept = 0)) +
    scale_y_continuous(lim=y.ax.scale) +
    theme_bw() +
    scale_x_continuous(limits=c(-0.5,5), label=scales::percent, 
                       breaks=c(-0.5, 0, 1, 2.5, 5)) +
    scale_color_manual(name='COD/N Ratio', values=c(1, 2, 3, 4)) +
    scale_linetype_manual(name='COD/N Ratio',values=c(4, 2, 3, 1)) +
    theme(text=element_text(family=font),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.title=element_blank(),
          plot.title = element_text(hjust = 0.5), 
          legend.position = 'bottom') +
    labs(title=lab.name)
}
g_legend <- function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

# Model parameters
Nmin <- .6
Nmax <- 60
CODmax <- 400
CODmin <- 4
cNin <- seq(Nmin, Nmax, length=100) # Nitrogen concentration varies
cCODin <- seq(CODmin, CODmax, length=100) # Carbon concentration varies
Q <- 60 # constant flow rate
e_base = c(90*5,90*2, 90, 0,-90/2)

# Run Model
result_SS_all <- list()
result_SS <- matrix(nrow=length(e_base), ncol=5, dimnames=list(1:length(e_base), c('e_base', 'r_10', 'r_5', 'r_2.5', 'r_1')))
result_SS_CO2_B <- result_SS
result_SS_CO2_C <- result_SS
result_SS_CO2_D <- result_SS
result_SS_CO2_E <- result_SS
result_SS_cost_B <- result_SS
result_SS_cost_C <- result_SS
result_SS_cost_D <- result_SS
result_SS_cost_E <- result_SS

#c(50, 40, 20, 10), c(100)
#c(40), c(400, 200, 100, 40)
for (i in 1:length(e_base)) {
  #result_SS_all[i] <- scenarios(Q, cNin, cCODin, e_Base= e_base[i], compare=TRUE)
  result <- scenarios(Q, c(50, 40, 20, 10), c(100),  e_Base= e_base[i], compare=TRUE)
  
  result_SS_CO2_B[i,] <- c(e_base[i], result$B$CO2.equivs)
  result_SS_CO2_C[i,] <- c(e_base[i], result$C$CO2.equivs)
  result_SS_CO2_D[i,] <- c(e_base[i], result$D$CO2.equivs)
  result_SS_CO2_E[i,] <- c(e_base[i], result$E$CO2.equivs)
  result_SS_cost_B[i,] <- c(e_base[i], result$B$cost)
  result_SS_cost_C[i,] <- c(e_base[i], result$C$cost)
  result_SS_cost_D[i,] <- c(e_base[i], result$D$cost)
  result_SS_cost_E[i,] <- c(e_base[i], result$E$cost)
  
}

# Data Clean
result_SS_CO2_B <- SS_to_DF(result_SS_CO2_B)
result_SS_CO2_C <- SS_to_DF(result_SS_CO2_C)
result_SS_CO2_D <- SS_to_DF(result_SS_CO2_D)
result_SS_CO2_E <- SS_to_DF(result_SS_CO2_E)
result_SS_cost_B <- SS_to_DF(result_SS_cost_B)
result_SS_cost_C <- SS_to_DF(result_SS_cost_C)
result_SS_cost_D <- SS_to_DF(result_SS_cost_D)
result_SS_cost_E <- SS_to_DF(result_SS_cost_E)

# Plot
b_plot_GHG <- plot_ss(result_SS_CO2_B, labB.name, c(-50000, 20000))
c_plot_GHG <- plot_ss(result_SS_CO2_C, labC.name, c(-50000, 20000))
d_plot_GHG <- plot_ss(result_SS_CO2_D, labD.name, c(-50000, 20000))
e_plot_GHG <- plot_ss(result_SS_CO2_E, labE.name, c(-50000, 20000))
legend <- g_legend(b_plot_GHG)

grid.arrange(b_plot_GHG + theme(legend.position="none"), 
             c_plot_GHG + theme(legend.position="none"), 
             d_plot_GHG + theme(legend.position="none"), 
             e_plot_GHG + theme(legend.position="none"),
             legend,
             top=textGrob('GHG Emission Sensitivity to Pumping Demand', gp=gpar(fontfamily=font)),
             left=textGrob(y.ax.GHG, gp=gpar(fontfamily=font), rot=90),
             bottom=textGrob('Approx. % Change from Conventional System', 
                             gp=gpar(fontfamily=font)),
             layout_matrix=matrix(c(1,2,1,2,1,2,1,2,1,2,1,2,
                                    3,4,3,4,3,4,3,4,3,4,3,4,5,5), ncol=2, byrow = TRUE))

ggsave('code/figures/ss_cost.png', grid.arrange(b_plot_GHG + theme(legend.position="none"), 
                                               c_plot_GHG + theme(legend.position="none"), 
                                               d_plot_GHG + theme(legend.position="none"), 
                                               e_plot_GHG + theme(legend.position="none"),
                                               legend,
                                               top=textGrob('GHG Emission Sensitivity to Pumping Demand', gp=gpar(fontfamily=font)),
                                               left=textGrob(y.ax.GHG, gp=gpar(fontfamily=font), rot=90),
                                               bottom=textGrob('Approx. % Change from Conventional System', 
                                                               gp=gpar(fontfamily=font)),
                                               layout_matrix=matrix(c(1,2,1,2,1,2,1,2,1,2,1,2,
                                                                      3,4,3,4,3,4,3,4,3,4,3,4,5,5), ncol=2, byrow = TRUE)))

b_plot_cost <- plot_ss(result_SS_cost_B, labB.name, c(-50000, 20000))
c_plot_cost <- plot_ss(result_SS_cost_C, labC.name, c(-50000, 20000))
d_plot_cost <- plot_ss(result_SS_cost_D, labD.name, c(-50000, 20000))
e_plot_cost <- plot_ss(result_SS_cost_E, labE.name, c(-50000, 20000))
legend <- g_legend(b_plot_cost)

grid.arrange(b_plot_cost + theme(legend.position="none"), 
             c_plot_cost + theme(legend.position="none"), 
             d_plot_cost + theme(legend.position="none"), 
             e_plot_cost + theme(legend.position="none"),
             legend,
             top=textGrob('Operational Cost Sensitivity to Pumping Demand', gp=gpar(fontfamily=font)),
             left=textGrob(y.ax.cost, gp=gpar(fontfamily=font), rot=90),
             bottom=textGrob('Approx. % Change from Conventional System', 
                             gp=gpar(fontfamily=font)),
             layout_matrix=matrix(c(1,2,1,2,1,2,1,2,1,2,1,2,
                                    3,4,3,4,3,4,3,4,3,4,3,4,5,5), ncol=2, byrow = TRUE))

ggsave('code/figures/ss_cost.png', grid.arrange(b_plot_cost + theme(legend.position="none"), 
                                                c_plot_cost + theme(legend.position="none"), 
                                                d_plot_cost + theme(legend.position="none"), 
                                                e_plot_cost + theme(legend.position="none"),
                                                legend,
                                                top=textGrob('Operational Cost Sensitivity to Pumping Demand', gp=gpar(fontfamily=font)),
                                                left=textGrob(y.ax.cost, gp=gpar(fontfamily=font), rot=90),
                                                bottom=textGrob('Approx. % Change from Conventional System', 
                                                                gp=gpar(fontfamily=font)),
                                                layout_matrix=matrix(c(1,2,1,2,1,2,1,2,1,2,1,2,
                                                                       3,4,3,4,3,4,3,4,3,4,3,4,5,5), ncol=2, byrow = TRUE)))

