library("car")  # load the 'car' package
data("Anscombe")  # load the data set
 ?Anscombe  # read a description of the data
head(Anscombe)  # look at the first few lines of the data

pairs(Anscombe)
lmoda=lm(education~income+ young+ urban, data= Anscombe)
summary(lmoda)

library(rjags)
#education likelihood normally dist with mean and sd
#we specify priors on mean and sd
#normal priors on linear model predictors of mu
#vague priors on their distributions non-iformative
#~so that information is picked from the linear model~
#inverse gamma prior on sd also vague scale

## Initial guess of variance based on overall
## variance of education variable. Uses low prior
## effective sample size. Technically, this is not
## a true 'prior', but it is not very informative.
library("rjags")

#write model function for lm
#likelihood and prior

moda_string = " model {

    for (i in 1:length(education)) {
        education[i] ~ dnorm(mu[i], prec)
        mu[i] = b[1] + b[2]*income[i] + b[3]*young[i] + b[4]*urban[i]
    }
    
    for (i in 1:4) {
        b[i] ~ dnorm(0.0, 1.0/1.0e6)
    }
    
    prec ~ dgamma(1.0/2.0, 1.0*1500.0/2.0)
    sig2 = 1.0 / prec
    sigma = sqrt(sig2)
} "

data_jagsa = as.list(Anscombe)


#specifiy MCMC sampler parameters
set.seed(72)

paramsa=c( "b", "sigma")

initsa=function(){
  inits=list(  "b"=rnorm(4,0.0,100.0), "prec"=rgamma(1,1.0,1.0))
}

moda_lm= jags.model(textConnection(moda_string), data=data_jagsa, inits=initsa, n.chains=3)

update(moda_lm, 1000)

moda_lm_sim=coda.samples(model=moda_lm, variable.names = paramsa, n.iter=5000)

moda_csim=do.call(rbind,moda_lm_sim)

plot(moda_lm_sim)

dic.samples(moda_lm, n.iter = 100000)

lmoda2=lm(education~income+ young income*youth, data= Anscombe)
summary(lmoda)
summary(moda_lm_sim)
summary(moda_csim)

moda2_string = " model {
    for (i in 1:length(education)) {
        education[i] ~ dnorm(mu[i], prec)
        mu[i] = b[1] + b[2]*income[i] + b[3]*young[i] 
    }
    
    for (i in 1:3) {
        b[i] ~ dnorm(0.0, 1.0/1.0e6)
    }
    
    prec ~ dgamma(1.0/2.0, 1.0*1500.0/2.0)
    sig2 = 1.0 / prec
    sigma = sqrt(sig2)
} "

data_jagsa2 = as.list(Anscombe)


set.seed(72)

paramsa2=c( "b", "sigma")

initsa2=function(){
  inits=list(  "b"=rnorm(3,0.0,100.0), "prec"=rgamma(1,1.0,1.0))
}

moda2_lm= jags.model(textConnection(moda2_string), data=data_jagsa2, inits=initsa2, n.chains=3)

update(moda2_lm, 1000)

moda2_lm_sim=coda.samples(model=moda2_lm, variable.names = paramsa2, n.iter=5000)

moda_csim=do.call(rbind,moda2_lm_sim)

plot(moda_lm_sim)

dic.samples(moda2_lm, n.iter = 100000)

moda3_string = " model {
    for (i in 1:length(education)) {
        education[i] ~ dnorm(mu[i], prec)
        mu[i] = b[1] + b[2]*income[i] + b[3]*young[i] + b[4]*income[i]*young[i]
    }
    
    for (i in 1:4) {
        b[i] ~ dnorm(0.0, 1.0/1.0e6)
    }
    
    prec ~ dgamma(1.0/2.0, 1.0*1500.0/2.0)
    sig2 = 1.0 / prec
    sigma = sqrt(sig2)
} "

data_jagsa3 = as.list(Anscombe)


set.seed(72)

paramsa3=c( "b", "sigma")

initsa3=function(){
  inits=list(  "b"=rnorm(4,0.0,100.0), "prec"=rgamma(1,1.0,1.0))
}

moda3_lm= jags.model(textConnection(moda3_string), data=data_jagsa3, inits=initsa3, n.chains=3)

update(moda3_lm, 1000)

#moda2_lm_sim=coda.samples(model=moda2_lm, variable.names = paramsa2, n.iter=5000)

#moda_csim=do.call(rbind,moda2_lm_sim)

#plot(moda_lm_sim)

dic.samples(moda3_lm, n.iter = 100000)

dic.samples(moda_lm, n.iter = 100000)
dic.samples(moda2_lm, n.iter = 100000)
dic.samples(moda3_lm, n.iter = 100000)

(0.07809-0)/0.01101
