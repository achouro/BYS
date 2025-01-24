par(mfrow=c(3,1))
plot(yhat3, resid3)
plot(yhat2, resid2)
plot(yhat1, resid1)

dic.samples(mod1, n.iter=1000)
dic.samples(mod2, n.iter=1000)
dic.samples(mod3, n.iter=1000)

?dic.samples
