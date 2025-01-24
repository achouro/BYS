##Metropolis-Hastings

#Y_i |mu ~ N(mu, 1) likelihood
#mu~t(0,1,1) non-conjugate prior

#we want to simulate posterior p(mu|Y_i's) 
#proportional to g evaluated by Bayes theorem

#g(mu)= exp(n*(ybar*mu - (mu^2)/2.0) -log(1.0 + (mu^2))

#for convenience we will use its log 

lg=function(mu, n, ybar){
  n*(ybar*mu - (mu^2)/2.0) -log(1.0 + (mu^2))
}

#Metropolis-Hastings Algorithm

mh=function(n, ybar, n_iter, mu_init, cand_sd){

    mu_output= numeric(n_iter)
    accepted=0
    
    mu_current= mu_init
    lg_current= lg(mu_current, n, ybar)
    
#Draw candidates theta*    
    for(i in 1:n_iter){
      mu_cand= rnorm(1, mean=mu_current, sd= cand_sd)
      lg_cand=lg(mu_cand, n, ybar)
      
#Compute acceptance ratio (in logs)      
      lg_alpha= lg_cand - lg_current
      alpha= exp(lg_alpha)

#Evaluate against 1      
      u= runif(1)
      if(u<alpha){
        mu_current= mu_cand
        accepted= accepted+1
        lg_current= lg_cand
      }
      mu_output[i]=mu_current
    }
    
    list(mu=mu_output, acceptance= accepted/n_iter)
}

#Example

y= c(1.2, 1.4, -0.5, 0.3, 0.9, 2.3, 1.0, 0.1, 1.3, 1.9)
ybar=mean(y)
n=length(y)
hist(y, freq=FALSE, xlim=c(-1, 3))
points(y, rep(0,0,n))
points(ybar, 0.0, pch=19)
curve(dt(x, df=1), lty=2, add=TRUE)

install.packages("coda")
library("coda")

#Posterior distribution sampling
set.seed(43)
post=mh(n=n, ybar = ybar, n_iter= 1000, mu_init=3, cand_sd=.9)
str(post)
traceplot(as.mcmc(post$mu))

#Posterior analysis
post$mu_kept=post$mu[-c(1:100)]
plot(density((post$mu_kept), xlim=c(-1,3) ))



