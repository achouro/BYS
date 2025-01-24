library("car")
data("Leinhardt")
?Leinhardt

head(Leinhardt)
str(Leinhardt)

#Positive valued and very strongly right skewed
#log would be more approps

Leinhardt$loginfant=log(Leinhardt$infant)
Leinhardt$logincome=log(Leinhardt$income)

dat= na.omit(Leinhardt)
str(dat)

#Modelling

lmod=lm(Leinhardt$loginfant~Leinhardt$logincome)
summary(lmod)

library("rjags")

#Define
mod_string11 = " model {
    for (i in 1:length(y)) {
        y[i] ~ dnorm(mu[i], inv_sigma_sq)
        mu[i] = a[region[i]] + b[1]*log_income[i] +b[2]*is_oil[i]
    }
    
    for(i in 1:max(region)){
      a[i]~dnorm(a0, inv_tau_sq)
    }
    
    a0~dnorm(0.0, 1.0/1.0e6)
    inv_tau_sq ~ dgamma(1/2.0, 1*10.0/2.0)
    tau=sqrt(1/inv_tau_sq)
    
    for (i in 1:2) {
        b[i] ~ dnorm(0.0, 1.0/1.0e6)
    }
    
    inv_sigma_sq ~ dgamma(5/2.0, 5*10.0/2.0)
    sigma = sqrt(1.0 / inv_sigma_sq)
} "

set.seed(116)
data_jags11 = list(y=dat$loginfant,  
                  log_income=dat$logincome,
                  is_oil=as.numeric(dat$oil=="yes"),
                  region=as.numeric(dat$region))

params11 = c("a0","a", "b","tau", "sigma")

inits11 = function() {
  inits = list("b"=rnorm(2,0.0,100.0), 
               "prec"=rgamma(1,1.0,1.0))
}

mod11 = jags.model(textConnection(mod_string11), 
                   data=data_jags11, 
                   n.chains=3)

update(mod11,1000)

mod_sim11=coda.samples(model=mod11, variable.names = params11, n.iter=5000)

mod_csim11=do.call(rbind,mod_sim11)

summary(mod_sim11)


#Convergence diagnostics

plot(mod_sim11, ask=TRUE)
gelman.diag(mod_sim11)
autocorr.diag(mod_sim11)
effectiveSize(mod_sim11)


dic11=dic.samples(mod11, n.iter = 1000)
dic11
summary(mod_sim11)

#Residuals
lmod0=lm(infant~income, data=Leinhardt)
plot(resid(lmod0))
plot(predict(lmod0), resid(lmod0))
(qqnorm(resid(lmod0)))


#Design matrix

X=cbind(rep(1.0, data1_jags$n),data1_jags$log_income)

pm_params1= colMeans(mod1_csim)

yhat1= drop(X %*% pm_params1[1:2])

resid1=data1_jags$y - yhat1

plot(resid1)
plot(yhat1, resid1)
qqnorm(resid1)

head(row.names(dat)[order(resid1, decreasing=TRUE)])

