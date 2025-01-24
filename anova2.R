data("PlantGrowth")
?PlantGrowth
head(PlantGrowth)
par(mfrow=c(1,1))
boxplot(weight~group , data=PlantGrowth)

lmod= lm(weight~group, data=PlantGrowth)
summary(lmod)
anova(lmod)
#Define
mod5_string = " model {

    for (i in 1:length(y)) {
        y[i] ~ dnorm(mu[grp[i]], prec[grp[i]])
    }
    
    for (i in 1:3) {
        mu[i] ~ dnorm(0.0, 1.0/1.0e6)
    }
    for (i in 1:3) {
        prec[i] ~ dgamma(5.0/2.0, 5.0*1.0/2.0)
        sigma[i] = sqrt(1.0 / prec[i])
    }
#effective prior sample size of 5 and prior guess of variace 1    

} "
#Set-up
set.seed(82)
str(PlantGrowth)
data5_jags = list(y=PlantGrowth$weight, 
                  grp=as.numeric(PlantGrowth$group))

params5 = c("mu", "sigma")

inits5 = function() {
  inits = list("mu"=rnorm(3,0.0,100.0),
               "prec"=rgamma(3,1.0,1.0))
}
#Run MCMC
mod5 = jags.model(textConnection(mod5_string), 
                  data=data5_jags, 
                  inits=inits5, 
                  n.chains=3)

update(mod5, 1000) # burn-in

mod5_sim = coda.samples(model=mod5,
                        variable.names=params5,
                        n.iter=5000)

mod5_csim = as.mcmc(do.call(rbind, mod5_sim))

summary(mod5_sim)
summary(mod4_sim)
#Diagnostics
plot(mod5_sim)
gelman.diag(mod5_sim)
autocorr.diag(mod5_sim)
effectiveSize(mod5_sim)

#Posterior mean
pm_params5= colMeans(mod5_csim)
pm_params5
pm_params4= colMeans(mod4_csim)
pm_params4


dic1=dic.samples(mod4, n.iter=1000)
dic1
dic2=dic.samples(mod5, n.iter=1000)
dic2
dic1-dic2
#(comparison with lmod)
coefficients(lmod)

#Residuals
yhat5= pm_params5[1:3][data5_jags$grp]
data5_jags$grp
yhat5

resid5= data5_jags$grp-yhat5
plot(yhat5, resid5)

summary(mod5_sim)


hpd1=HPDinterval(mod4_csim[,3]-mod4_csim[,1], 0.95)
hpd1

hpd2=HPDinterval(mod5_csim, 0.95)
hpd2


mod_cm = lm(weight ~ -1 + group, data=PlantGrowth)
summary(mod_cm)
#Posterior probability
pp= mean(mod5_csim[,3]>mod5_csim[,1])
pp
#
ppa= mean(mod5_csim[,3]>1.1*mod5_csim[,1])                
ppa                

