install.packages("rjags")
cancel
#Specify the model
library("rjags")

mod_str= "model{
  for(i in 1:n){
    y[i]~dnorm(mu, 1.0/sig2) #precision 1/stdev instead of stdev
  }
  mu ~ dt(0, 1/1.0, 1)
  sig2=1.0
  
}"

#Set-up model
set.seed(50)
y= c(1.2, 1.4, -0.5, 0.3, 0.9, 2.3, 1.0, 0.1, 1.3, 1.9)
n=length(y)

data_jags=list(y=y, n=n)
params=c("mu")

init=function() {
  init=list("mu"=0.0)}

mod=jags.model(textConnection(mod_str), data = data_jags, inits=init)

#Run MCMC sampler

update(mod, 500)

mod_sim= coda.samples(model=mod, variable.names = params, n.iter=1000)

#Post processing

library("coda")
plot(mod_sim)
summary(mod_sim)




