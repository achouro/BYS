set.seed(2021)
# Simulate 300 observations from an AR(2) with one pair of complex-valued reciprocal roots 
r=0.95
lambda=12 
phi=numeric(2) 
phi[1]=2*r*cos(2*pi/lambda) 
phi[2]=-r^2
sd=1 # innovation standard deviation
T=300 # number of time points
# generate stationary AR(2) process
yt=arima.sim(n = T, model = list(ar = phi), sd = sd) 

## Compute the MLE for phi and the unbiased estimator for v using the conditional likelihood
p=2
y=rev(yt[(p+1):T]) # response
X=t(matrix(yt[rev(rep((1:p),T-p)+rep((0:(T-p-1)),rep(p,T-p)))],p,T-p));
XtX=t(X)%*%X
XtX_inv=solve(XtX)
phi_MLE=XtX_inv%*%t(X)%*%y # MLE for phi
s2=sum((y - X%*%phi_MLE)^2)/(length(y) - p) #unbiased estimate for v

cat("\n MLE of conditional likelihood for phi: ", phi_MLE, "\n",
    "Estimate for v: ", s2, "\n")

# Simulate 300 observations from an AR(2) with one pair of complex-valued roots 
set.seed(2021)
r=0.95
lambda=12 
phi=numeric(2) 
phi[1]=2*r*cos(2*pi/lambda) 
phi[2]=-r^2
sd=1 # innovation standard deviation
T=300 # number of time points
# generate stationary AR(2) process
yt=arima.sim(n = T, model = list(ar = phi), sd = sd) 
par(mfrow=c(1,1))
plot(yt)

## Compute the MLE of phi and the unbiased estimator of v using the conditional likelihood
p=2
y=rev(yt[(p+1):T]) # response
X=t(matrix(yt[rev(rep((1:p),T-p)+rep((0:(T-p-1)),rep(p,T-p)))],p,T-p));
XtX=t(X)%*%X
XtX_inv=solve(XtX)
phi_MLE=XtX_inv%*%t(X)%*%y # MLE for phi
s2=sum((y - X%*%phi_MLE)^2)/(length(y) - p) #unbiased estimate for v

#####################################################################################
### Posterior inference, conditional likelihood + reference prior via 
### direct sampling                 
#####################################################################################

n_sample=1000 # posterior sample size
library(MASS)

## step 1: sample v from inverse gamma distribution
v_sample=1/rgamma(n_sample, (T-2*p)/2, sum((y-X%*%phi_MLE)^2)/2)

## step 2: sample phi conditional on v from normal distribution
phi_sample=matrix(0, nrow = n_sample, ncol = p)
for(i in 1:n_sample){
  phi_sample[i, ]=mvrnorm(1,phi_MLE,Sigma=v_sample[i]*XtX_inv)
}

par(mfrow = c(2, 3), cex.lab = 1.3)
## plot histogram of posterior samples of phi and v

for(i in 1:2){
  hist(phi_sample[, i], xlab = bquote(phi), 
       main = bquote("Histogram of "~phi[.(i)]),col='lightblue')
  abline(v = phi[i], col = 'red')
}

hist(v_sample, xlab = bquote(nu), main = bquote("Histogram of "~v),col='lightblue')
abline(v = sd, col = 'red')

#####################################################
# Graph posterior for modulus and period 
#####################################################
r_sample=sqrt(-phi_sample[,2])
lambda_sample=2*pi/acos(phi_sample[,1]/(2*r_sample))
hist(r_sample,xlab="modulus",main="",col='lightblue')
abline(v=0.95,col='red')
hist(lambda_sample,xlab="period",main="",col='lightblue')
abline(v=12,col='red')

###################################################
# Simulate data from an AR(2)
###################################################
set.seed(2021)
r=0.95
lambda=12 
phi=numeric(2) 
phi[1]=2*r*cos(2*pi/lambda) 
phi[2]=-r^2
sd=1 # innovation standard deviation
T=300 # number of time points
# generate stationary AR(2) process
yt=arima.sim(n = T, model = list(ar = phi), sd = sd) 

#############################################################################
######   compute AIC and BIC for different AR(p)s based on simulated data ###
#############################################################################
pmax=10 # the maximum of model order
Xall=t(matrix(yt[rev(rep((1:pmax),T-pmax)+rep((0:(T-pmax-1)),
                                              rep(pmax,T-pmax)))], pmax, T-pmax));
y=rev(yt[(pmax+1):T])
n_cond=length(y) # (number of total time points - the maximum of model order)

## compute MLE
my_MLE <- function(y, Xall, p){
  n=length(y)
  x=Xall[,1:p]
  a=solve(t(x) %*%x)
  a=(a + t(a))/2 # for numerical stability 
  b=a%*%t(x)%*%y # mle for ar coefficients
  r=y - x%*%b # residuals 
  nu=n - p # degrees freedom
  R=sum(r*r) # SSE
  s=R/nu #MSE
  return(list(b = b, s = s, R = R, nu = nu))
}


## function for AIC and BIC computation 
AIC_BIC <- function(y, Xall, p){
  ## number of time points
  n <- length(y)
  
  ## compute MLE
  tmp=my_MLE(y, Xall, p)
  
  ## retrieve results
  R=tmp$R
  
  ## compute likelihood
  likl= n*log(R)
  
  ## compute AIC and BIC
  aic =likl + 2*(p)
  bic =likl + log(n)*(p)
  return(list(aic = aic, bic = bic))
}
# Compute AIC, BIC 
aic =numeric(pmax)
bic =numeric(pmax)

for(p in 1:pmax){
  tmp =AIC_BIC(y,Xall, p)
  aic[p] =tmp$aic
  bic[p] =tmp$bic
  print(c(p, aic[p], bic[p])) # print AIC and BIC by model order
}

## compute difference between the value and its minimum
aic =aic-min(aic) 
bic =bic-min(bic) 

## draw plot of AIC, BIC, and the marginal likelihood
par(mfrow = c(1, 1))
matplot(1:pmax,matrix(c(aic,bic),pmax,2),ylab='value',
        xlab='AR order p',pch="ab", col = 'black', main = "AIC and BIC")
# highlight the model order selected by AIC
text(which.min(aic), aic[which.min(aic)], "a", col = 'red') 
# highlight the model order selected by BIC
text(which.min(bic), bic[which.min(bic)], "b", col = 'red') 

########################################################
p <- which.min(bic) # We set up the moder order
print(paste0("The chosen model order by BIC: ", p))


