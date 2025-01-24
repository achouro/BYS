###########################################################
##### Compute AR reciprocal roots given the AR coefficients
###########################################################
# Assume the folloing AR coefficients for an AR(8)
phi=c(0.27, 0.07, -0.13, -0.15, -0.11, -0.15, -0.23, -0.14)
roots=1/polyroot(c(1, -phi)) # compute reciprocal characteristic roots
r=Mod(roots) # compute moduli of reciprocal roots
lambda=2*pi/Arg(roots) # compute periods of reciprocal roots
# print results modulus and frequency by decreasing order
print(cbind(r, abs(lambda))[order(r, decreasing=TRUE), ][c(2,4,6,8),]) 


## simulate data from an AR(2)
set.seed(2021)
## AR(2) with a pair of complex-valued roots with modulus 0.95 and period 12 
r=0.95
lambda=12 
phi=numeric(2) 
phi[1]<- 2*r*cos(2*pi/lambda) 
phi[2] <- -r^2
phi
T=300 # number of time points
sd=1 # innovation standard deviation
yt=arima.sim(n=T, model = list(ar = phi), sd=sd)

par(mfrow = c(3, 1), cex.lab = 1.5)
## plot simulated data 
ts.plot(yt)
## draw sample autocorrelation function
acf(yt, lag.max = 50,
    type = "correlation", ylab = "sample ACF", 
    lty = 1, ylim = c(-1, 1), main = " ")

## draw sample partial autocorrelation function
pacf(yt, lag.ma = 50, main = "sample PACF")

### Simulate from AR(2) with two real reciprocal roots (e.g., 0.95 and 0.5)
set.seed(2021)
recip_roots=c(0.95, 0.5) ## two different real reciprocal roots
phi=c(sum(recip_roots), -prod(recip_roots)) ## compute ar coefficients
phi
T=300 ## set up number of time points
sd=1 ## set up standard deviation
yt=arima.sim(n=T,model = list(ar=phi),sd=sd) # generate ar(2)

par(mfrow = c(3, 1), cex.lab = 1.5, cex.main = 1.5)
### plot simulated data 
ts.plot(yt)
### plot sample ACF
acf(yt, lag.max = 50, type = "correlation",  main = "sample ACF")
### plot sample PACF
pacf(yt, lag.max = 50, main = "sample PACF")

### Simulate from AR(3) with one real root 
### and a pair of complex roots (e.g., r=0.95 and lambda = 12 and real root with
### 0.8 modulus)
set.seed(2021)
r= c(0.95, 0.95, 0.8) ## modulus
lambda=c(-12, 12) ## lambda
recip_roots=c(r[1:2]*exp(2*pi/lambda*1i), r[3]) ## reciprocal roots
phi <- numeric(3) # placeholder for phi
phi[1]=Re(sum(recip_roots)) # ar coefficients at lag 1
phi[2]=-Re(recip_roots[1]*recip_roots[2] + recip_roots[1]*recip_roots[3] + recip_roots[2]*recip_roots[3]) # ar coefficients at lag 2
phi[3]=Re(prod(recip_roots))
phi
T=300 # number of time points
sd=1 # standard deviation
yt=arima.sim(n=T,model = list(ar=phi), sd = sd) # generate ar(3)

par(mfrow = c(3,1), cex.lab = 1.5, cex.main = 1.5)
### plot simulated data 
ts.plot(yt)
### plot sample ACF
acf(yt, lag.max = 50, type = "correlation",  main = "sample ACF")
### plot sample PACF
pacf(yt, lag.max = 50, main = "sample PACF")

