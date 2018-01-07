# Sources & Libraries
source(file = 'quantileregression.R')

# Read in data & data clean
SouthPlant <- read.csv('data/SouthPlantPermitData.csv', header=TRUE)
SouthPlant <- SouthPlant[, -grep("X", colnames(SouthPlant))]
SouthPlant$Date <- as.POSIXct(as.Date(SouthPlant$Date, '%d-%b-%y'))
SouthPlant$TN <- SouthPlant$AverageNH4+SouthPlant$AverageTON
Data <- SouthPlant[, -grep("Max", colnames(SouthPlant))]
Data <- Data[, -grep("NH4", colnames(Data))]
Data <- Data[, -grep("TON", colnames(Data))]
Data <- Data[order(Data$AverageFlow),]

# Regression Models
flow.bod<-lm(AverageBOD~AverageFlow,data=Data)
flow.TN<-lm(TN~AverageFlow,data=Data)
bod.regress <- quantile.regression(Data$AverageFlow, Data$AverageBOD, Data$AverageFlow)
QR.bod.residuals <- Data$AverageBOD-bod.regress$y
TN.regress <- quantile.regression(Data$AverageFlow, Data$TN, Data$AverageFlow)
QR.TN.residuals <-  Data$TN-TN.regress$y
# Conclusion: We like Linear for BOD and Quantile for Nitrogen (see figs X-X in figure maker)
Data$BODfit <- flow.bod$fitted.values
Data$BODresids <- flow.bod$residuals
Data$TNfit <- TN.regress$y
Data$TNresids <- QR.TN.residuals

# Store Data Generation Params
sd.bod <- sd(Data$BODresids)
sd.TN.lowflow <- sd(Data$TNresids[which(Data$AverageFlow<=85)])
sd.TN.highflow <- sd(Data$TNresids[which(Data$AverageFlow>85)])
flow.model <- generate.cdf(Data$AverageFlow)
bod.model <- flow.bod$coefficients

#' Title: Generating synthetic data for monte carlo simulation
#'
#' @param n - number of fake data points you want to generate
#'
#' @return syndata - a dataframe of length n with flowrates, nitrogen and carbon conc.
generate.data <- function(n) {
    # Use empirical CDF to choose flowrates
    p.flow <- runif(n = n)
    flow <- approx(x = flow.model$p, y=flow.model$x, xout=p.flow, rule=2)
    syn.data <- data.frame(flow=flow$y)
    
    # Use linear model + variance based on standard deviation to choose BOD
    BOD.modeled <- bod.model[2]*syn.data$flow + bod.model[1]
    BOD.var <- rnorm(n = n, sd = sd.bod)
    syn.data$BOD <- BOD.modeled + BOD.var
    
    # Use quantile regression + variance based on sd (dependent on flow range) to choose TN
    TN.modeled <- quantile.regression(Data$AverageFlow, Data$TN, syn.data$flow)
    TN.modeled <- TN.modeled$y
    i.l <- which(syn.data$flow<=85)
    i.h <- which(syn.data$flow>85)
    syn.data$TN <- rep(x = NA, times=n)
    syn.data$TN[i.l] = TN.modeled[i.l] + rnorm(n = length(i.l), sd = sd.TN.lowflow)
    syn.data$TN[i.h] = TN.modeled[i.h] + rnorm(n = length(i.h), sd = sd.TN.highflow)
    # Can't be less than 0
    syn.data$TN[syn.data$TN<0] <- 0
    
    
    return(syn.data)
}

