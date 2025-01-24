#This code allows you to compute estimates of the AR
#(1) coefficient and the variance using the arima function in R. 
#The first case uses the conditional sum of squares, the second and third cases 
#use the full likelihood with different starting points for the numerical optimization 
#required to compute the MLE with the full likelihood. 

par(mfrow=c(1,1))
# Obtaining parameter estimates using the arima function in R

set.seed(2021)
phi=0.9 # ar coefficient
v=1
sd=sqrt(v) # innovation standard deviation
T=500 # number of time points
yt=arima.sim(n = T, model = list(ar = phi), sd = sd) 

#Using conditional sum of squares, equivalent to conditional likelihood 
arima_CSS=arima(yt,order=c(1,0,0),method="CSS",n.cond=1,include.mean=FALSE)
cat("AR estimates with conditional sum of squares (CSS) for phi and v:", arima_CSS$coef,arima_CSS$sigma2,
    "\n")

#Uses ML with full likelihood 
arima_ML=arima(yt,order=c(1,0,0),method="ML",include.mean=FALSE)
cat("AR estimates with full likelihood for phi and v:", arima_ML$coef,arima_ML$sigma2,
    "\n")

#Default: uses conditional sum of squares to find the starting point for ML and 
#         then uses ML 
arima_CSS_ML=arima(yt,order=c(1,0,0),method="CSS-ML",n.cond=1,include.mean=FALSE)
cat("AR estimates with CSS to find starting point for ML for phi and v:", 
    arima_CSS_ML$coef,arima_CSS_ML$sigma2,"\n")



#This code shows you how to compute the MLE for 
#phi using the full likelihood and the function optimize in R
set.seed(2021)
phi=0.9 # ar coefficient
v=1
sd=sqrt(v) # innovation standard deviation
T=500 # number of time points
yt=arima.sim(n = T, model = list(ar = phi), sd = sd) 

## MLE, full likelihood AR(1) with v=1 assumed known 
# log likelihood function
log_p <- function(phi, yt){
  0.5*(log(1-phi^2) - sum((yt[2:T] - phi*yt[1:(T-1)])^2) - yt[1]^2*(1-phi^2))
}

# Use a built-in optimization method to obtain maximum likelihood estimates
result =optimize(log_p, c(-1, 1), tol = 0.0001, maximum = TRUE, yt = yt)
cat("\n MLE of full likelihood for phi: ", result$maximum)

