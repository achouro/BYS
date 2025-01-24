#The following code allows you to compute the MLE of the AR coefficient 
#phi the unbiased estimator of v, s_sq, and the MLE of 
#v based on a dataset simulated from an AR(1)process  
#and using the conditional likelihood. 

####################################################
#####             MLE for AR(1)               ######
####################################################
set.seed(2021)
phi=0.9 # ar coefficient
v=1
sd=sqrt(v) # innovation standard deviation
T=500 # number of time points
yt=arima.sim(n = T, model = list(ar = phi), sd = sd) 

## Case 1: Conditional likelihood
y=as.matrix(yt[2:T]) # response
X=as.matrix(yt[1:(T-1)]) # design matrix
phi_MLE=as.numeric((t(X)%*%y)/sum(X^2)) # MLE for phi
s2=sum((y - phi_MLE*X)^2)/(length(y) - 1) # Unbiased estimate for v 
v_MLE=s2*(length(y)-1)/(length(y)) # MLE for v

cat("\n MLE of conditional likelihood for phi: ", phi_MLE, "\n",
    "MLE for the variance v: ", v_MLE, "\n", 
    "Estimate s2 for the variance v: ", s2, "\n")

#Posterior inference AR(1)
#Conditional Likelihood, reference prior
#Direct sampling

#Step1
n_sample=3000
v_sample=1/rgamma(n_sample, (T-2)/2, sum((yt[2:T]-phi_MLE*yt[1:(T-1)])^2))

phi_sample=rep(0, n_sample)

for(i in 1:n_sample){
  phi_sample[i]=rnorm(1, mean= phi_MLE, sd= sqrt(v_sample[i]/sum(yt[1:(T-1)]^2)))
}

#plot of posterior samples
par(mfrow=c(1,2), cex.lab=1.3)

hist(phi_sample, xlab=bquote(phi), main= bquote("Posterior for "~phi))
abline(v=phi, col="red")

hist(v_sample, xlab=bquote(v), main= bquote("Posterior for "~v))
abline(v=sd, col="lightblue")

