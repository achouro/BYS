#Modified model of mod6
mod6_string = " model {

    for (i in 1:length(y)) {
        y[i] ~ dbern(p[i])
    
        logit(p[i])=int + b[1]*gravity[i] + b[2]*cond[i] + b[3]*calc[i] 
    }
    
    int~dnorm(0.0, 1.0/25)
    
    for (i in 1:3) {
        b[i] ~ ddexp(0.0, sqrt(2.0))
    }
} "
#Set-up
set.seed(82)
str(PlantGrowth)
data6_jags = list(y=dat$r, 
                  gravity=X[,"gravity"],
                  cond=X[,"cond"],
                  calc=X[,"calc"])

params6 = c("int", "b")

#Run MCMC
mod6 = jags.model(textConnection(mod6_string), 
                  data=data6_jags, 
                  n.chains=3)

update(mod6, 1000) # burn-in

mod6_sim = coda.samples(model=mod6,
                        variable.names=params6,
                        n.iter=5000)

mod6_csim = as.mcmc(do.call(rbind, mod6_sim))

summary(mod6_sim)

dic2=dic.samples(mod6, n.iter=1000)
dic1
dic2

#Posterior mean
pm_params6= colMeans(mod6_csim)
pm_params6

#Calculate probabilities
1.0/(1.0+exp(-(-0.21)))

1.0/(1.0+exp(-(-0.21 -0.97*0.0 -0.83*(-1.0) +1.88*(1.0))))

pm_Xb=pm_params6["int"]+ X[,c(1,4,6)%*%pm_params6[1:3]]
pm_Xb

p_hat=1.0/(1.0+ exp(-(pm_Xb)))
head(p_hat) 
par(mfrow=c(1,1))
plot(p_hat, jitter(dat$r))

tab05=table(p_hat>0.5, dat$r)
tab05
#success rate
sum(diag(tab05))/sum((tab05))

tab03=table(p_hat>0.3, dat$r)
tab03
#lower accuracy
sum(diag(tab03))/sum((tab03))

