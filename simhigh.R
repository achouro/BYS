#y_i ~ Binom(10, phi_i)
#phi_i ~ Beta(2,2)

#simulate phi_i from Beta(2,2)
m=1e5

y=numeric(m)
phi=numeric(m)

#populate y and phi from beta and binom simulations
for ( i in 1:m){
   phi[i]=rbeta(1, 2, 2)
   y[i]= rbinom(1, size=10, prob=phi[i])
}

#vectorized

phi=rbeta(m, 2, 2)
y= rbinom(m, 10, phi)

tab=table(y)

tab/m
#marginal dist of y beta-binomial
plot(tab)

#marginal exp val of y

mean(y)

