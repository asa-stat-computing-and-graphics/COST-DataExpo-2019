#########################
## The Corrected AIC criterion
##
## Bedrick & Tsai (1994), Model Selection for Multivariate
##   Regression in Small Samples, Biometrics, 50 (1) 226-231.

aic_mlm <- function(mod, correct=TRUE) {
  p <- mod$coefficients[2]
  m <- mod$coefficients[1]
  n <- mod$df.residual + m
  
  sig <- cov(mod$residuals)
  
  if(correct) {
    d <- n/(n - (m + p + 1) )
  } else d <- 1
  n*(determinant(sig, logarithm=TRUE)$modulus[1] + p) + 2*d*(p*m + 0.5*p*(p+1) )
}

