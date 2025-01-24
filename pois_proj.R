#Hierarchical model of chips per cookies i from different factories j
head(cookies)
dat=cookies
table(dat$location)
boxplot(chips~ location, data=dat)

set.seed(112)
n_sim=500

#Y_i,j |lambda_j ~Pois(lambda_j) i=1,..30 j=1,..5
#lambda_j|alpha,beta ~ Gamma(alpha, beta) j=1,..5
#alpha~p(alpha), beta~p(beta) 

#mu=E(Gamma(alpha, beta)= alpha/beta ~Gamma(2, 1/5);
#sigma^2= Var(Gamma(alpha, beta)=sqrt(alpha)/beta~ Exp(1) 
                  

#we fist set p()=Exp()
alpha_prior=rexp(n_sim, 1.0/2.0)
beta_prior=rexp(n_sim, 5.0)

mu_prior=alpha_prior/beta_prior
sigma_prior=sqrt(alpha_prior)/beta_prior

summary(mu_prior)
summary(sigma_prior)

lambda_prior= rgamma(n_sim, alpha_prior, beta_prior)
summary(lambda_prior)

y_prior=rpois(n_sim,lambda_prior)
summary(y_prior)

lambda_prior=lambda_prior[1:5]
y_prior=rpois(150, lambda = rep(lambda_prior, each=30))
summary(y_prior)

mod_string9="model{
  for(i in 1:length(chips)){
    chips[i]~dpois(lambda[location[i]])
  }
  for(i in 1:max(location)){
    lambda[i]~dgamma(alpha, beta)
  }
#mu and sigma mean and var of gamma dist for lambdas
  mu~dgamma(2.0, 1.0/5.0) #prior mean 10
  sigma~dexp(1.0)
#alpha/beta=mu, sqrt(alpha)/beta=sigma
  
  alpha=mu^2/sigma^2
  beta=mu/alpha^2
}"

set.seed(113)

data_jags9=as.list(dat)

params9=c("lambda", "mu","sigma" )

mod9=jags.model(textConnection(mod_string9),
                data=data_jags9,
                n.chains=3)
update(mod9, 1000)

mod_sim9=coda.samples(model=mod9, 
                      variable.names=params9, 
                      n.iter = 5000)
mod_csim9=as.mcmc(do.call(rbind, mod_sim9))

plot(mod_sim9)
dic9=dic.samples(mod9, n.iter=1000)
dic9

summary(mod_sim9)

#Convergence diagnostics
plot(mod_sim9)
gelman.diag(mod_sim9)
autocorr.diag(mod_sim9)
effectiveSize(mod_sim9)

#Residuals for posterior means

pm_params9= colMeans(mod_csim9)

yhat9= rep(pm_params9[1:5], each=30)

resid9= dat$chips -yhat9

plot(resid9)
plot(jitter(yhat9), resid9)
plot(jitter(yhat9), resid9)
par(mfrow=c(1,1))

#Location mean/ lambda residuals
lambda_resid=pm_params9[1:5]-pm_params9["mu"]
plot(lambda_resid)



summary(mod_sim9)


#Using posterior dist for mu and sigma 
#we can draw predictive dist for lambda

n_sim=nrow(mod_csim9)

#Posterior samples of beta and alpha
#alpha=mu^2/sigma^2
#alpha=mu/sigma^2
#lambda~gamma(alpha,beta)

post_alpha= mod_csim9[,"mu"]^2/mod_csim9[,"sigma"]^2
post_beta= mod_csim9[,"mu"]/mod_csim9[,"sigma"]^2


lambda_predictive=rgamma(n_sim, post_alpha, post_beta)

hist(lambda_predictive)
mean(lambda_predictive>15)

#Posterior predictive dist of nb of cookies Y_i,j 
#from a new location 

y_predictive=rpois(n_sim, lambda_predictive)
hist(y_predictive)
mean(y_predictive>4)
mean(y_predictive>10)

#Posterior prob that cookie produced at location_1
#have less than 7 chips

y_pred2= rpois(n_sim, lambda= mod_csim9[,"lambda[1]"])
hist(y_pred2)
mean(y_pred2<7)



#Posterior probabilities
y_pred1= rpois(n_sim, lambda= mod_csim9[,"lambda[1]"])
mean(y_pred1<16)
hist(y_pred1)
y_pred2= rpois(n_sim, lambda= mod_csim9[,"lambda[2]"])
mean(y_pred2<11)
hist(y_pred2)
y_pred3= rpois(n_sim, lambda= mod_csim9[,"lambda[3]"])
mean(y_pred3<16)
y_pred4= rpois(n_sim, lambda= mod_csim9[,"lambda[4]"])
mean(y_pred4<15)
y_pred5= rpois(n_sim, lambda= mod_csim9[,"lambda[5]"])
mean(y_pred5<19)

mean(y_pred5)








