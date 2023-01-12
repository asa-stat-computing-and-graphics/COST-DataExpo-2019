###############################
##
## This code calculate the Margin of Error
##  i.e., the +/- part of the simultaneous
##  confidence/prediction intervals for predicted
##  values from a multivariate regression model
##
## Code is adapted from the bivariate ellipse code at
##   https://data.library.virginia.edu/getting-started-with-multivariate-multiple-regression/
##

predictionIntervals <- function(mod, newdata, type=c("confidence", "prediction"),
                                level = 0.95, ggplot = TRUE){

  if(type=="confidence")
    add <- 0
  else add <- 1
  
  # shape of ellipse
  Z <- model.matrix(mod)
  Y <- mod$model[[1]]
  n <- nrow(Y)
  m <- ncol(Y)
  r <- ncol(Z) - 1
  S <- crossprod(resid(mod))/(n-r-1)
  
  # radius of circle generating the ellipse
  tt <- terms(mod)
  Terms <- delete.response(tt)
  mf <- model.frame(Terms, newdata, na.action = na.pass, 
                    xlev = mod$xlevels)
  z0 <- model.matrix(Terms, mf, contrasts.arg = mod$contrasts)
  rad <- sqrt((m*(n-r-1)/(n-r-m))*qf(level,m,n-r-m))*sqrt(add+z0%*%solve(t(Z)%*%Z) %*% t(z0))
  moe <- matrix(rep(diag(rad),m), ncol=m)*matrix(rep(sqrt(diag(S)), dim(rad)[1]), ncol=m, byrow=TRUE)
  moe
}

