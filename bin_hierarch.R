#Hierarchical model for auditory perception in children
library("MASS")
data("OME")
?OME # background on the data


dat = subset(OME, OME != "N/A")
dat$OME = factor(dat$OME) # relabel OME
dat$ID = as.numeric(factor(dat$ID)) # relabel ID so there are no gaps in numbers (they now go from 1 to 63)

head(OME)
## Original reference model and covariate matrix
mod_glm = glm(Correct/Trials ~ Age + OME + Loud + Noise, data=dat, weights=Trials, family="binomial")
X = model.matrix(mod_glm)[,-1]

## Original model (that needs to be extended)
mod_string12 = " model {
	for (i in 1:length(y)) {
		y[i] ~ dbin(phi[i], n[i])
		logit(phi[i]) = alpha[ID[i]] + b[1]*Age[i] + b[2]*OMElow[i] + b[3]*Loud[i] + b[4]*Noiseincoherent[i]
	}
	
	for(i in 1:max(ID)){
	  alpha[i] ~ dnorm(mu, inv_tau_sq)
	}
	
	mu ~ dnorm(0, 1.0/10^2)
	inv_tau_sq ~ dgamma(1.0/2.0, 1.0/2.0)
	tau=sqrt(1/inv_tau_sq)
	
	for (j in 1:4) {
		b[j] ~ dnorm(0.0, 1.0/4.0^2)
	}
	
	
	
} "

data_jags12 = as.list(as.data.frame(X))
data_jags12$y = dat$Correct
data_jags12$n = dat$Trials
data_jags12$ID = dat$ID

params12 = c("alpha","b","mu","tau" )

#Run MCMC
mod12 = jags.model(textConnection(mod_string12), 
                  data=data_jags12,
                  n.chains=3)

update(mod12, 1000) # burn-in

mod_sim12 = coda.samples(model=mod12,
                        variable.names=params12,
                        n.iter=5000)

mod_csim12 = as.mcmc(do.call(rbind, mod_sim12))

summary(mod_sim12)


#Convergence diagnostics
plot(mod_sim12, ask=TRUE)
gelman.diag(mod_sim12)
autocorr.diag(mod_sim12)
effectiveSize(mod_sim12)
sum(effectiveSize(mod_sim12))

dic12=dic.samples(mod12, n.iter=1000)
dic12
