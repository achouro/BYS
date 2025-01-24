set.seed(61)
post0 = mh(n=n, ybar=ybar, n_iter=10e3, mu_init=0.0, cand_sd=0.9)
coda::traceplot(as.mcmc(post0$mu[-c(1:500)]))

set.seed(61)
post1 = mh(n=n, ybar=ybar, n_iter=1e3, mu_init=0.0, cand_sd=0.04)
coda::traceplot(as.mcmc(post1$mu[-c(1:500)]))

set.seed(61)
post2 = mh(n=n, ybar=ybar, n_iter=100e3, mu_init=0.0, cand_sd=0.04)
coda::traceplot(as.mcmc(post2$mu))

coda::autocorr.plot(as.mcmc(post0$mu))

coda::autocorr.diag(as.mcmc(post0$mu))


coda::autocorr.plot(as.mcmc(post1$mu))

coda::autocorr.diag(as.mcmc(post1$mu))

str(post2)
# contains 100,000 iterations
coda::effectiveSize(as.mcmc(post2$mu))
#effective sample size of ~350


## thin out the samples until autocorrelation is essentially 0. This will leave you with approximately independent samples. The number of samples remaining is similar to the effective sample size.
coda::autocorr.plot(as.mcmc(post2$mu), lag.max=500)

thin_interval = 400 # how far apart the iterations are for autocorrelation to be essentially 0.
thin_indx = seq(from=thin_interval, to=length(post2$mu), by=thin_interval)
head(thin_indx)

coda::raftery.diag(as.mcmc(post0$mu))
?raftery.diag

post2mu_thin = post2$mu[thin_indx]
traceplot(as.mcmc(post2$mu))

traceplot(as.mcmc(post2mu_thin))

coda::autocorr.plot(as.mcmc(post2mu_thin), lag.max=10)

effectiveSize(as.mcmc(post2mu_thin))
#250
length(post2mu_thin)
#250


str(post0) # contains 10,000 iterations



