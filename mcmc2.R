#Independent Metroplis-Hasting with normal draws

# draw candidate
theta_cand = rnorm(n=1, mean=0.0, sd=10.0)

# evaluate log of g with the candidate
lg_cand = lg(theta=theta_cand)

# evaluate log of g at the current value
lg_now = lg(theta=theta_now)

# evaluate log of q at candidate
lq_cand = dnorm(theta_cand, mean=0.0, sd=10.0, log=TRUE)

# evaluate log of q at the current value
lq_now = dnorm(theta_now, mean=0.0, sd=10.0, log=TRUE)

# calculate the acceptance ratio
lalpha = lg_cand + lq_now - lg_now - lq_cand 
alpha = exp(lalpha)

# draw a uniform variable which will be less than alpha with probability min(1, alpha)
u = runif(1)

if (u < alpha) { # then accept the candidate
  theta_now = theta_cand
  accpt = accpt + 1 # to keep track of acceptance
}

