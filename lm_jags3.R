library("rjags")

mod3_string = " model {
    for (i in 1:length(y)) {
        y[i] ~ dt(mu[i], tau, df)
        mu[i] = b[1] + b[2]*log_income[i] + b[3]*is_oil[i]
    }
    
    for (i in 1:3) {
        b[i] ~ dnorm(0.0, 1.0/1.0e6)
    }
    
    nu~dexp(1.0)
    df= nu+2
    
    tau ~ dgamma(5/2.0, 5*10.0/2.0)
    sig = sqrt(1.0 / tau*df / (df-2.0))
} "

set.seed(75)
data3_jags = list(y=dat$loginfant, 
                  log_income=dat$logincome,
                  is_oil=as.numeric(dat$oil=="yes"))

params3 = c("b","df", "sig")

inits3 = function() {
  inits = list("b"=rnorm(3,0.0,100.0),
               "nu"=rexp(1, 1.0) ,
               "tau"=rgamma(1,1.0,1.0))
}

mod3 = jags.model(textConnection(mod3_string), 
                  data=data3_jags, 
                  inits=inits3, 
                  n.chains=3)

update(mod3, 1000) # burn-in

mod3_sim = coda.samples(model=mod3,
                        variable.names=params3,
                        n.iter=5000)

mod3_csim = as.mcmc(do.call(rbind, mod3_sim))

summary(mod3_csim)

#Design matrix
n3=length(data3_jags$y)
X3=cbind(rep(1.0, n3),
         data3_jags$log_income,
         data3_jags$is_oil)

head(X3)
pm_params3= colMeans(mod3_csim)

yhat3= drop(X3 %*% pm_params3[1:3])

resid3=data3_jags$y - yhat3
par(mfrow=c(3,1))

plot(yhat3, resid3)
plot(yhat2, resid2)
plot(yhat1, resid1)





