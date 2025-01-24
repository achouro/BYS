###############################
#####    sample ar(1)     #####
###############################
#
# sample data from 2 ar(1) processes and plot their ACF and PACF functions
#
set.seed(2021)
T=500 # number of time points
#
# sample data from an ar(1) with ar coefficient phi = 0.9 and variance 1
#
v=2.0 # innovation variance
sd=sqrt(v) #innovation stantard deviation
phi1=0.6 # ar coefficient
yt1=arima.sim(n = T, model = list(ar = phi1), sd = sd)

#
# sample data from an ar(1) with ar coefficient phi = -0.9 and variance 1
#
phi2=-0.5 # ar coefficient
yt2=arima.sim(n = T, model = list(ar = phi2), sd = sd)

par(mfrow = c(2, 1), cex.lab = 1.3)
plot(yt1,main=expression(phi==0.8))
plot(yt2,main=expression(phi==-0.8))

par(mfrow = c(3, 2), cex.lab = 1.3)
lag.max=10 # max lag
#
## plot true ACFs for both processes
#

cov_0=sd^2/(1-phi1^2) # compute auto-covariance at h=0

cov_h=phi1^(0:lag.max)*cov_0 # compute auto-covariance at h
plot(0:lag.max, cov_h/cov_0, pch = 1, type = 'h', col = 'red',
     ylab = "true ACF", xlab = "Lag",ylim=c(-1,1), main=expression(phi==0.8))

cov_0=sd^2/(1-phi2^2) # compute auto-covariance at h=0
cov_h=phi2^(0:lag.max)*cov_0 # compute auto-covariance at h
# Plot autocorrelation function (ACF)
plot(0:lag.max, cov_h/cov_0, pch = 1, type = 'h', col = 'red',
     ylab = "true ACF", xlab = "Lag",ylim=c(-1,1),main=expression(phi==-0.8))

## plot sample ACFs for both processes
#
acf(yt1, lag.max = lag.max, type = "correlation", ylab = "sample ACF",
    lty = 1, ylim = c(-1, 1), main = " ")
acf(yt2, lag.max = lag.max, type = "correlation", ylab = "sample ACF",
    lty = 1, ylim = c(-1, 1), main = " ")
## plot sample PACFs for both processes
#
pacf(yt1, lag.max = lag.max, type = "sample PACF", ylab = "sample PACF",
     lty = 1, ylim = c(-1, 1), main = " ")
pacf(yt2, lag.max = lag.max, type = "sample PACF", ylab = "sample PACF",
     lty = 1, ylim = c(-1, 1), main = " ")