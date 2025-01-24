#Generate n observations from a mixture of two Gaussian dist
n=50
w=c(0.6,0.4)
mu=c(0,5)
sigma=c(1,2)
cc=sample(1:2, n, replace=TRUE, prob=w)

x=rnorm(n, mean=mu[cc], sd=sigma[cc])

#Plot G(x) marginal desity of x 

x5= seq(-5, 12, length=200)
y5=w[1]*dnorm(x5,mu[1], sigma[1]) + w[2]*dnorm(x5,mu[2], sigma[2])

par(mar=c(4,4,1,1)+0.1)
plot(x5,y5, type="l", las=1, xlab="X", ylab=" Density Function")
points(x,y=rep(0,n), pch=1)
points(x,y=rep(0,n), pch=1, col=cc)