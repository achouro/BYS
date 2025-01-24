### Simulate 300 observations from an AR(2) prcess with a pair of complex-valued roots 
set.seed(2021)
r=0.95
lambda=12 
phi=numeric(2) 
phi[1]<- 2*r*cos(2*pi/lambda) 
phi[2] <- -r^2
sd=sqrt(0.5) # innovation standard deviation
T=300 # number of time points
# sample from the AR(2) process
yt=arima.sim(n = T, model = list(ar = phi), sd = sd) 

# Compute the MLE of phi and the unbiased estimator of v using the conditional likelihood 
p=2
y=rev(yt[(p+1):T])
X=t(matrix(yt[rev(rep((1:p),T-p)+rep((0:(T-p-1)),rep(p,T-p)))],p,T-p));
XtX=t(X)%*%X
XtX_inv=solve(XtX)
phi_MLE=XtX_inv%*%t(X)%*%y # MLE for phi
s2=sum((y - X%*%phi_MLE)^2)/(length(y) - p) #unbiased estimate for v

# Obtain 200 samples from the posterior distribution under the conditional likelihood and the reference prior 
n_sample=200 # posterior sample size
library(MASS)

## step 1: sample v from inverse gamma distribution
v_sample=1/rgamma(n_sample, (T-2*p)/2, sum((y-X%*%phi_MLE)^2)/2)

## step 2: sample phi conditional on v from normal distribution
phi_sample=matrix(0, nrow = n_sample, ncol = p)
for(i in 1:n_sample){
  phi_sample[i,]=mvrnorm(1,phi_MLE,Sigma=v_sample[i]*XtX_inv)
}


### using spec.ar to draw spectral density based on the data assuming an AR(2)
spec.ar(yt, order = 2, main = "yt")