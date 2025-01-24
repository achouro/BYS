install.packages("COUNT")
library("COUNT")
data(badhealth)
head(badhealth)
any(is.na(badhealth))
sum(badhealth$badh)/

  hist(badhealth$numvisit, breaks=20)
min(badhealth$numvisit)

plot(jitter(log(numvisit)) ~ jitter(age), 
     data=badhealth, 
     subset=(badh==0 & numvisit>0),
     xlab="age", ylab="log(visits)")
points(jitter(log(numvisit)) ~ jitter(age), 
  data=badhealth, 
  subset=(badh==0 & numvisit>0),
  xlab="age", ylab="log(visits)",col="red")

library("rjags")

#log mean lambda
mod7_string = " model {

    for (i in 1:length(numvisit)) {

        numvisit[i] ~ dpois(lambda[i])
    
        log(lambda[i])=int + b_badh*badh[i] + b_age*age[i] + b_intx*badh[i]*age[i]
    }
    
    int~dnorm(0.0, 1.0/1e6)
    b_badh~dnorm(0.0, 1.0/1e4)
    b_age~dnorm(0.0, 1.0/1e4)
    b_intx~dnorm(0.0, 1.0/1e4)
    
} "
#Set-up
set.seed(102)

data7_jags = as.list(badhealth)

params7 = c("int","b_badh", "b_age", "b_intx")

#Run MCMC
mod7 = jags.model(textConnection(mod7_string), 
                  data=data7_jags,
                  n.chains=3)

update(mod7, 1000) # burn-in

mod7_sim = coda.samples(model=mod7,
                        variable.names=params7,
                        n.iter=5000)

mod7_csim = as.mcmc(do.call(rbind, mod7_sim))

summary(mod7_sim)

#Convergence diagnostics
plot(mod7_sim)
gelman.diag(mod7_sim)
autocorr.diag(mod7_sim)
effectiveSize(mod7_sim)

dic7=dic.samples(mod7, n.iter=1000)
dic7
#Residuals
X=as.matrix(badhealth[,-1])
X=cbind(X, with(badhealth, badh*age))

#Posterior moedian this time
pmed_coef= apply(mod7_csim, 2, median)
pmed_coef

log_lambda_hat = pmed_coef["int"] + X %*% pmed_coef[c("b_badh", "b_age", "b_intx")]
lambda_hat = exp(log_lambda_hat)

hist(lambda_hat)


resid = badhealth$numvisit - lambda_hat
plot(resid)

plot(lambda_hat, badhealth$numvisit)
abline(0.0, 1.0)

plot(lambda_hat[which(badhealth$badh==0)], resid[which(badhealth$badh==0)], xlim=c(0, 8), ylab="residuals", xlab=expression(hat(lambda)), ylim=range(resid))
points(lambda_hat[which(badhealth$badh==1)], resid[which(badhealth$badh==1)], col="red")

var(resid[which(badhealth$badh==0)])

var(resid[which(badhealth$badh==1)])

summary(mod7_sim)
#model has poor fit

#We look for the posterior prob that indiv with poor health have more doctor visits
mean(y2>y1)
#healthy person
x1=c(0, 35, 0)

#unhealthy pers
x2=c(1,35, 0)

head(mod7_csim)
log_lambda1= mod7_csim[,"int"]+ mod7_csim[, c(2,1,3)]%*%x1
log_lambda2= mod7_csim[,"int"]+ mod7_csim[, c(2,1,3)]%*%x2

head(log_lambda1)

lambda1=exp(log_lambda1)
lambda2=exp(log_lambda2)

plot(density(lambda1))
points(density(lambda2), add=TRUE)

n_sim=length(lambda1)
n_sim
y1= rpois(n_sim, lambda1)
y2= rpois(n_sim, lambda2)

plot(table(factor(y1, levels =0:18))/n_sim)
points(table((y2 +0.1))/n_sim, col="red")

mean(y2>y1)


#

