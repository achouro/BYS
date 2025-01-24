#Hierarchical model for mean personnel growth of 53 companies from 5 different idustries

dat=read.csv(file="/Users/mac/Downloads/New Folder With Items 6/BYS/MCM/pctgrowth.csv", header=TRUE)
head(dat)

#Y_i|theta_g(i),sigma^2 ~N(theta_g(j), sigma^2) i=1,..53, g(i)=1,..5
#theta_i|mu,tau^2 ~ N(mu, tau^2) g(i)=1,..5

#mu~N(0,1e6)
#tau_^2~IG(1/2, 1*3/2)
#sigma^2~IG(2/2, 2*1/2) 


mod_string10="model{
  for(i in 1:length(y)){
    y[i]~dnorm( theta[grp[i]], inv_sigma_sq )
  }
  for(i in 1:max(grp)){
    theta[i]~dnorm(mu, inv_tau_sq)
  }
  mu~dnorm(0,1.0e6)
  
  inv_tau_sq~dgamma(1/2, 1*3/2)
  tau=sqrt(1.0/inv_tau_sq)
  
  inv_sigma_sq~dgamma(2/2, 2*1/2)
  sigma=sqrt(1.0/inv_sigma_sq)
}"

set.seed(123)

data_jags10=as.list(dat)

params10=c("theta", "mu", "tau","sigma" )

mod10=jags.model(textConnection(mod_string10),
                data=data_jags10,
                n.chains=3)
update(mod10, 1000)

mod_sim10=coda.samples(model=mod10, 
                      variable.names=params10, 
                      n.iter = 1000)
mod_csim10=as.mcmc(do.call(rbind, mod_sim10))

summary(mod_sim10)

#Posterior mean for theta_g(i)

pm_params10= colMeans(mod_csim10)
pm_params10


means_anova = tapply(dat$y, INDEX=dat$grp, FUN=mean)
means_anova

pm_params10[4:8]-means_anova

plot(means_anova)
points(pm_params10, col="red")
