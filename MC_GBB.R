#Gibbs Sampler

#Y_i |mu ~ N(mu, sigma_sq) likelihood
#mu~N(mu_0, sigma_sq_0) conjugate prior for mu
#sigma_sq~IG(nu_0, beta_0) conjugate prior for sigma_sq

#using the joint distribution (multiply the cdfs) 
#we simplify the posteriors by removing conditional constants
#we have our conditional posteriors ceteris paribus the other variables
#values are then updated using the last update from the other variable and so on

#Define posterior distributions for mu and sigma_sq variables
update_mu= function(n, ybar, sig2, mu_0, sig2_0){

  sig2_1=1.0/(n/sig2 +1/sig2_0)
  mu_1= (1.0/(n/sig2 +1/sig2_0))*(n*ybar/sig2 + mu_0/sig2_0)
  
  rnorm(n=1,mean=mu_1, sd=sqrt(sig2_1))
}

update_sig2=function(n, y, mu, nu_0, beta_0){
  
  nu_1= nu_0 + n/2.0
  beta_1= beta_0 + sum((y-mu)^2)/2.0
  
  out_gamma=rgamma(n=1, shape=nu_1, rate=beta_1 )
  1.0/out_gamma
}

#Define Gibbs sampler
gibbs=function(y,n_iter, init, prior){
  ybar=mean(y)
  n=length(y)
  
  mu_out=numeric(n_iter)
  sig2_out=numeric(n_iter)
  
  mu_current=init$mu
  
  for(i in 1:n_iter){

    sig2_current= update_sig2(n, y, mu=mu_current, nu_0=prior$nu_0, beta_0= prior$beta_0)

    mu_current=update_mu(n,ybar, sig2=sig2_current, mu_0 = prior$mu_0, sig2_0 = prior$sig2_0)
    
    sig2_out[i]=sig2_current
    mu_out[i]=mu_current
  }
  cbind(mu=mu_out, sig2= sig2_out)
}

#Set-up problem
#Example
y= c(1.2, 1.4, -0.5, 0.3, 0.9, 2.3, 1.0, 0.1, 1.3, 1.9)
ybar=mean(y)
n=length(y)

#Prior
prior=list()

prior$mu_0=0.0
prior$sig2_0=1.0

prior$n_0=2.0
prior$s2_0=1.0

prior$nu=prior$n_0/2.0
prior$beta_0=prior$n_0 +prior$s2_0/2

hist(y, freq=FALSE, xlim=c(-1, 3))
curve(dnorm(x=x, mean=prior$mu_0, sd=sqrt(prior$sig2_0)), lty=2, add=TRUE)
points(y, rep(0,n))
points(ybar, 0.0, pch=19)

#Run Gibbs sampler
set.seed(53)

init=list()
init$mu= 0.0

post=gibbs(y=y,n_iter=1000, init=init, prior=prior)






