quantile.regression <- function(x, y, xout, rule=2) {
  cdf.x <- generate.cdf(x)
  cdf.y <- generate.cdf(y)
  
  pout <- approx(x, cdf.x$p, xout, rule=rule)
  yout <- approx(cdf.y$p, y, pout$y)
  return(list(x=xout, px=pout$x, y=yout$y))
}


#' Title Generate Empircal CDF w/ Cunnane Plotting Position
#'
#' @param x - What to generate CDF from
#'
#' @return list(x, p) - the x & p to plot the empirical CDF
generate.cdf <- function(x){
  n <- length(x)
  i <- 1:n
  p <- (i - 0.4)/(n + 0.2)
  return(list(x=x, p=p))
}