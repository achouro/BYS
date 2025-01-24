#EM algorithm for 2 Gaussian Mixture

rm(list = ls())
set.seed(81196)

#Generate Sim Data from Mixture of 2 Gaussians
KK=2
w.true=0.6
mu.true=rep(0,KK)
mu.true[1]=0
mu.true[2]=5
sigma.true=1
n=120
cc=sample(x=1:KK, size=n, replace=TRUE, prob=c(w.true, 1-w.true))

x=rep(0,n)
for(i in 1:n){
  x[i]=rnorm(1, mean=mu.true[cc[i]], sd=sigma.true)
}
#X is now the generated data

#Visualisation
par(mfrow=c(1,1))
xx.true=seq(-8,11, length=200)
#Mixture 
yy.true=w.true*dnorm(xx.true, mu.true[1], sigma.true)+
        (1-w.true)*dnorm(xx.true, mu.true[2], sigma.true)

plot(xx.true, yy.true, type="l", xlab="X", ylab="True Density", lwd=2)
points(x,rep(0,n), col=cc)


#Expectation-maximisation algorithm

#Initiallise params
w=1/2 #equal weights
mu=rnorm(KK, mean(x), sd(x)) #initial mu drawn from normal with mean and var of data 
sigma=sd(x)

#Plot for visalusation
xx=seq(-8,11, length=200)
#Mixture 
yy=w*dnorm(xx, mu[1], sigma)+
  (1-w)*dnorm(xx, mu[2], sigma)

plot(xx, yy, type="l", xlab="X", ylab="Initial Density", lwd=2)
points(x,rep(0,n), col=cc)

s=0
sw=FALSE #switch variable to end algorithm

#Qfunction
QQ= -Inf
QQ.out=NULL
epsilon=10^(-5) #error stopping criterion

#Algorithm

while(!sw){
  #E: Expectation calc
  v=array(0, dim=c(n,KK)) #120x2
  #P(c_i=K|)=w_hat*g_norm(X_i|theta)/sum(w_hat*g_norm(X_i|theta)) Bayes theorem 
  #v_i, log of the weights
  v[,1]=log(w) + dnorm(x, mu[1], sigma, log=TRUE) 
  v[,2]=log(1-w) + dnorm(x, mu[2], sigma, log=TRUE) 
  for(i in 1:n){
    v[i,]=exp(v[i,] - max(v[i,]))/ sum(exp(v[i,] - max(v[i,])))
  }
  
  #M: Maxiximisation
  w=mean(v[,1])
  #Values found through maximisation derivation i.e first derivative kernel
  mu=rep(0,KK)
  for(k in 1:KK){
    for(i in 1:n){
      mu[k]=mu[k] +v[i,k]*x[i]
    }
    mu[k]=mu[k]/sum(v[,k])
  }
  
  sigma=0
  for(k in 1:KK){
    for(i in 1:n){
      sigma=sigma +v[i,k]*(x[i]-mu[k])^2
    }
    sigma=sqrt(sigma/sum(v))
  }
  
  #Convergence
  #Q function
  QQn=0
  for(i in 1:n){
    QQn= QQn + v[i,1]*(log(w) + dnorm(x[i], mu[1], sigma, log = TRUE))+ 
      v[i,2]*(log(1-w) + dnorm(x[i], mu[2], sigma, log = TRUE))
  }
  
  if((abs(QQn-QQ)/abs(QQn)) < epsilon){
    sw=TRUE
  }
  
  QQ=QQn
  QQ.out=c(QQ.out, QQ)
  s=s+1
  print(paste(s, QQn))
  
  #Plot current estimate over data
  layout(matrix(c(1,2),2,1), widths=c(1,1), heights=c(1.3,3))
  par(mar=c(3.1,4.1,0.5,0.5))
  
  plot(QQ.out[1:s],type="l", xlim=c(1,max(10,s)), las=1, ylab="Q", lwd=2)
  
  par(mar=c(5,4,1.5,0.5))
  xx = seq(-8,11,length=200)
  yy = w*dnorm(xx, mu[1], sigma) + (1-w)*dnorm(xx, mu[2], sigma)
  plot(xx, yy, type="l", ylim=c(0, max(c(yy,yy.true))), main=paste("s =",s,"   Q =", round(QQ.out[s],4)), lwd=2, col="red", lty=2, xlab="x", ylab="Density")
  lines(xx.true, yy.true, lwd=2)
  points(x, rep(0,n), col=cc)
  legend(6,0.22,c("Truth","Estimate"),col=c("black","red"), lty=c(1,2))
  

}

#Plot final estimate over data
layout(matrix(c(1,2),2,1), widths=c(1,1), heights=c(1.3,3))
par(mar=c(3.1,4.1,0.5,0.5))

plot(QQ.out[1:s],type="l", xlim=c(1,max(10,s)), las=1, ylab="Q", lwd=2)

par(mar=c(5,4,1.5,0.5))
xx = seq(-8,11,length=200)
yy = w*dnorm(xx, mu[1], sigma) + (1-w)*dnorm(xx, mu[2], sigma)

plot(xx, yy, type="l", ylim=c(0, max(c(yy,yy.true))), main=paste("s =",s,"   Q =", round(QQ.out[s],4)), lwd=2, col="red", lty=2, xlab="x", ylab="Density")
lines(xx.true, yy.true, lwd=2)
points(x, rep(0,n), col=cc)

legend(6,0.22,c("Truth","Estimate"),col=c("black","red"), lty=c(1,2), bty="n")


















