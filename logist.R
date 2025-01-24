library("boot")
data("urine")
head(urine)
tail(urine)
dat=na.omit(urine)
dim(dat)
pairs(dat)

X=scale(dat[, -1], center=TRUE, scale=TRUE)
head(X)
colMeans(X)
apply(X, 2, sd)

#Laplace prior double exponential
ddexp=function(x,mu, tau){
  0.5*tau*exp(-tau*abs(x-mu))
}
curve(ddexp(x,mu=0.0, tau=1.0), from=-5, to=5, ylab="density", 
      main="Double Exponential Distribution")
curve(dnorm(x,mean=0.0, sd=1.0), from=-5, to=5,lty=2, add=TRUE)

mod6_string = " model {

    for (i in 1:length(y)) {
        y[i] ~ dbern(p[i])
    
        logit(p[i])=int + b[1]*gravity[i] + b[2]*ph[i] + b[3]*osmo[i]+ 
                          b[4]*cond[i] + b[5]*urea[i] + b[6]*calc[i] 
    }
    
    int~dnorm(0.0, 1.0/25)
    
    for (i in 1:6) {
        b[i] ~ ddexp(0.0, sqrt(2.0))
    }
} "
#Set-up
set.seed(82)
str(PlantGrowth)
data6_jags = list(y=dat$r, 
                  gravity=X[,"gravity"],
                  ph=X[,"ph"],
                  osmo=X[,"osmo"],
                  cond=X[,"cond"],
                  urea=X[,"urea"],
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

plot(mod6_sim, ask=TRUE)

dic1=dic.samples(mod6, n.iter=1000)
#Marginal Posterior density plot
par(mfrow=c(1,1))
par(mfrow=c(3,2))
densplot(mod6_csim[,1:6], xlim=c(-3.0,3.0))
colnames(X)
#B[2], B[3], plot identical to ddexp no effect


