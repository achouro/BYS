data("PlantGrowth")
?PlantGrowth
head(PlantGrowth)
par(mfrow=c(1,1))
boxplot(weight~group , data=PlantGrowth)

lmod= lm(weight~group, data=PlantGrowth)
summary(lmod)
anova(lmod)
#Define
mod4_string = " model {

    for (i in 1:length(y)) {
        y[i] ~ dnorm(mu[grp[i]], prec)
    }
    
    for (i in 1:3) {
        mu[i] ~ dnorm(0.0, 1.0/1.0e6)
    }
#effective prior sample size of 5 and prior guess of variace 1    
    prec ~ dgamma(5.0/2.0, 5.0*1.0/2.0)
    sigma = sqrt(1.0 / prec)
} "
#Set-up
set.seed(82)
str(PlantGrowth)
data4_jags = list(y=PlantGrowth$weight, 
                  grp=as.numeric(PlantGrowth$group))

params4 = c("mu", "sigma")

inits4 = function() {
  inits = list("mu"=rnorm(3,0.0,100.0),
               "prec"=rgamma(1,1.0,1.0))
}
#Run MCMC
mod4 = jags.model(textConnection(mod4_string), 
                  data=data4_jags, 
                  inits=inits4, 
                  n.chains=3)

update(mod4, 1000) # burn-in

mod4_sim = coda.samples(model=mod4,
                        variable.names=params4,
                        n.iter=5000)

mod4_csim = as.mcmc(do.call(rbind, mod3_sim))

summary(mod4_sim)

#Diagnostics
plot(mod4_sim)
gelman.diag(mod4_sim)
autocorr.diag(mod4_sim)
effectiveSize(mod4_sim)

#Posterior mean
pm_params4= colMeans(mod4_csim)
pm_params4
#(comparison with lmod)
coefficients(lmod)

#Residuals
yhat4= pm_params4[1:3][data4_jags$grp]
data4_jags$grp
yhat4

resid4= data4_jags$grp-yhat4
plot(yhat4, resid4)

summary(mod4_sim)

HPDinterval(mod4_csim, 0.95)

#Posterior probability
pp= mean(mod4_csim[,3]>mod4_csim[,1])
pp
#
ppa= mean(mod4_csim[,3]>1.1*mod4_csim[,1])                
ppa                
                
                