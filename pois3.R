dat = read.csv(file="callers.csv", header=TRUE)
head(dat)
pairs(dat)

plot(calls/days_active~isgroup2 ,data=dat)

plot(calls~isgroup2 ,data=dat)


#Define
mod8_string = " model {

    for (i in 1:length(calls)) {

        calls[i] ~ dpois(lambda[i])
    
        log(lambda[i])=int + b[1]*age[i] + b[2]*isgroup2[i] 
    }
    
    int~dnorm(0.0, 1.0/1e2)
    
    b[1]~dnorm(0.0, 1.0/1e2)
    b[2]~dnorm(0.0, 1.0/1e2)
    
} "
#Set-up
set.seed(102)

data8_jags = as.list(dat)

params8 = c("int","b")

#Run MCMC
mod8 = jags.model(textConnection(mod8_string), 
                  data=data8_jags,
                  n.chains=3)

update(mod8, 1000) # burn-in

mod8_sim = coda.samples(model=mod8,
                        variable.names=params8,
                        n.iter=5000)

mod8_csim = as.mcmc(do.call(rbind, mod8_sim))

summary(mod8_sim)
#Posterior probability that beta_2 is greter than 1
pp= mean(mod8_csim[,2]>1)
pp

#Convergence diagnostics
dic8=dic.samples(mod8, n.iter=1000)
plot(mod7_sim)
gelman.diag(mod7_sim)
autocorr.diag(mod7_sim)
effectiveSize(mod7_sim)




#ex