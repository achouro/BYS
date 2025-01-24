#
# Simulate data with no temporal structure (white noise)
#
set.seed(2021)
T=200
t =1:T
y_white_noise=rnorm(T, mean=0, sd=1)
#
# Define a time series object in R: 
# Assume the data correspond to annual observations starting in January 1960 
#
yt=ts(y_white_noise, start=c(1960), frequency=1)
#
# plot the simulated time series, their sample ACF and their sample PACF
#
par(mfrow = c(1, 3), cex.lab = 1.3, cex.main = 1.3)
yt=ts(y_white_noise, start=c(1960), frequency=1)
plot(yt, type = 'l', col='red', xlab = 'time (t)', ylab = "Y(t)")
acf(yt, lag.max = 20, xlab = "lag",
    ylab = "Sample ACF",ylim=c(-1,1),main="")
pacf(yt, lag.max = 20,xlab = "lag",
     ylab = "Sample PACF",ylim=c(-1,1),main="")