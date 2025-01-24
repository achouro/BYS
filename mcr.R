set.seed(32)
m=10000
a=2
b=1/3.0
#m random generations from gamma of shape a and rate b
theta= rgamma(m, a, b)

#we try to approximate MC estimates using CLT

#standard error sd/sqrt(m)
se=sd(theta)/sqrt(m)

2*se
#edges of confidence interval
mean(theta)-2*se
mean(theta)+2*se


#indicator for prob on theta*
ind=theta<5
mean(ind) 
pgamma(5.0,a,b)

se=sd(theta)/sqrt(m)
se
