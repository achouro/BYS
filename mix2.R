#Zero-inflated  Count Models

#Negative Binomial
x3= seq(0, 15)
y3=dnbinom(x,8,0.6)

par(mfrow=c(2,1))
par(mar=c(4,4,1,1)+0.1)
barplot(y3, names.arg=x, las=1, xlab="X", ylab="Probability Mass", ylim=c(0,0.22))

#Mixture of Point mass at zero and Negative Binomial
z3=0.2*c(1,rep(0,length(x3)-1))+ (1-0.2)*y3
par(mar=c(4,4,1,1)+0.1)
barplot(z3, names.arg=x, las=1, xlab="X", ylab="Probability Mass", ylim=c(0,0.22))

#Zero-inflated continuous dist
x4= seq(-2, 15, length=200)
y4=plnorm(x4,1.5,0.5)
z4=0.3*as.numeric(x4>=0)+ (1-0.3)*y4

par(mfrow=c(1,1))
par(mar=c(4,4,1,1)+0.1)

plot(x4,y4, type="l", las=1, lty=2, xlab="X", ylab="Cumulative Distribution Function")
lines(x4,z4, lty=1)

legend(4, 0.45, c("Zero-inf log-Gaussian", "log-Gaussian"), lty=c(1,2), bty="n")



