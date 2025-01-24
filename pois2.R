mod7_string = " model {

    for (i in 1:length(numvisit)) {

        numvisit[i] ~ dpois(lambda[i])
    
        log(lambda[i])=int + b_badh*badh[i] + b_age*age[i] 
    }
    
    int~dnorm(0.0, 1.0/1e6)
    b_badh~dnorm(0.0, 1.0/1e4)
    b_age~dnorm(0.0, 1.0/1e4)
    
} "
#Set-up
set.seed(102)

data7_jags = as.list(badhealth)

params7 = c("int","b_badh", "b_age")

#Run MCMC
mod7 = jags.model(textConnection(mod7_string), 
                  data=data7_jags,
                  n.chains=3)

update(mod7, 1000) # burn-in

mod7_sim = coda.samples(model=mod7,
                        variable.names=params7,
                        n.iter=5000)

mod7_csim = as.mcmc(do.call(rbind, mod7_sim))

dic_mod_1=5633
dic7_2=dic.samples(mod7, n.iter=1000)
dic7_2

summary(mod7_sim)

#Convergence diagnostics
plot(mod7_sim)
gelman.diag(mod7_sim)
autocorr.diag(mod7_sim)
effectiveSize(mod7_sim)

#ex

y1=rpois(100000, 2*15)
y2=rpois(100000, 22)

mean(y1<22)


dat = read.csv(file="callers.csv", header=TRUE)
head(dat)
pairs(dat)

plot(calls/days_active~isgroup2 ,data=dat)

plot(calls~isgroup2 ,data=dat)
