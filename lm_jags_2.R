library("rjags")

mod2_string = " model {
    for (i in 1:length(y)) {
        y[i] ~ dnorm(mu[i], prec)
        mu[i] = b[1] + b[2]*log_income[i] + b[3]*is_oil[i]
    }
    
    for (i in 1:3) {
        b[i] ~ dnorm(0.0, 1.0/1.0e6)
    }
    
    prec ~ dgamma(5/2.0, 5*10.0/2.0)
    sig = sqrt(1.0 / prec)
} "

set.seed(75)
data2_jags = list(y=dat$loginfant, 
                  log_income=dat$logincome,
                  is_oil=as.numeric(dat$oil=="yes"))

params2 = c("b", "sig")

inits2 = function() {
  inits = list("b"=rnorm(3,0.0,100.0), 
               "prec"=rgamma(1,1.0,1.0))
}

mod2 = jags.model(textConnection(mod2_string), 
                  data=data2_jags, 
                  inits=inits2, 
                  n.chains=3)

update(mod2, 1000) # burn-in

mod2_sim = coda.samples(model=mod2,
                        variable.names=params2,
                        n.iter=5000)

mod2_csim = as.mcmc(do.call(rbind, mod2_sim))

summary(mod2_csim)

#Design matrix
n2=length(data2_jags$y)
X2=cbind(rep(1.0, n2),
        data2_jags$log_income,
        data2_jags$is_oil)

head(X2)
pm_params2= colMeans(mod2_csim)

yhat2= drop(X2 %*% pm_params2[1:3])

resid2=data2_jags$y - yhat2
par(mfrow=c(2,1))

plot(yhat2, resid2)
plot(yhat1, resid1)
sd(resid2)

par(mfrow=c(1,1))
curve(dnorm(x), from=-5, to=5)
curve(dt(x,1), from=-5, to=5, col="red", add=TRUE)


