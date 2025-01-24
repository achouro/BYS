y=c(-1,0,1)


for (i in 4:200) {
  set.seed(i)
  U=runif(1)
  if(U<=0.5){
    y.new=rnorm(1,0.1*y[i-1]+0.1*y[i-2],0.25)
  }else if(U>0.8){
    y.new=rnorm(1,0.3*y[i-1]+0.5*y[i-2],0.25)
  }else{
    y.new=rnorm(1,0.4*y[i-1]-0.5*y[i-2],0.25)
  }
  y=c(y,y.new)
}


plot(y,type='l',xlab='Time',ylab='Simulated Time Series')


library(MCMCpack)
library(mvtnorm)


##setting prior
p=2 ## order of AR process
K=3 ## number of mixing component
Y=matrix(y[3:200],ncol=1) ## y_{p+1:T}
Fmtx=matrix(c(y[2:199],y[1:198]),nrow=2,byrow=TRUE) ## design matrix F
n=length(Y) ## T-p


## prior hyperparameters
m0=matrix(rep(0,p),ncol=1)
C0=10*diag(p)
C0.inv=0.1*diag(p)
n0=1
d0=1
a=rep(1,K)

#sampling function
sample_omega=function(L.cur){
  n.vec=sapply(1:K, function(k){sum(L.cur==k)})
  rdirichlet(1,a+n.vec)
}


sample_L_one=function(beta.cur,omega.cur,nu.cur,y.cur,Fmtx.cur){
  prob_k=function(k){
    beta.use=beta.cur[((k-1)*p+1):(k*p)]
    omega.cur[k]*dnorm(y.cur,mean=sum(beta.use*Fmtx.cur),sd=sqrt(nu.cur[k]))
  }
  prob.vec=sapply(1:K, prob_k)
  L.sample=sample(1:K,1,prob=prob.vec/sum(prob.vec))
  return(L.sample)
}


sample_L=function(y,x,beta.cur,omega.cur,nu.cur){
  L.new=sapply(1:n, function(j){sample_L_one(beta.cur,omega.cur,nu.cur,y.cur=y[j,],Fmtx.cur=x[,j])})
  return(L.new)
}


sample_nu=function(k,L.cur){
  idx.select=(L.cur==k)
  n.k=sum(idx.select)
  if(n.k==0){
    d.k.star=d0
    n.k.star=n0
  }else{
    y.tilde.k=Y[idx.select,]
    Fmtx.tilde.k=Fmtx[,idx.select]
    e.k=y.tilde.k-t(Fmtx.tilde.k)%*%m0
    Q.k=t(Fmtx.tilde.k)%*%C0%*%Fmtx.tilde.k+diag(n.k)
    Q.k.inv=chol2inv(chol(Q.k))
    d.k.star=d0+t(e.k)%*%Q.k.inv%*%e.k
    n.k.star=n0+n.k
  }
  
  1/rgamma(1,shape=n.k.star/2,rate=d.k.star/2)
}




sample_beta=function(k,L.cur,nu.cur){
  nu.use=nu.cur[k]
  idx.select=(L.cur==k)
  n.k=sum(idx.select)
  if(n.k==0){
    m.k=m0
    C.k=C0
  }else{
    y.tilde.k=Y[idx.select,]
    Fmtx.tilde.k=Fmtx[,idx.select]
    e.k=y.tilde.k-t(Fmtx.tilde.k)%*%m0
    Q.k=t(Fmtx.tilde.k)%*%C0%*%Fmtx.tilde.k+diag(n.k)
    Q.k.inv=chol2inv(chol(Q.k))
    A.k=C0%*%Fmtx.tilde.k%*%Q.k.inv
    m.k=m0+A.k%*%e.k
    C.k=C0-A.k%*%Q.k%*%t(A.k)
  }
  
  rmvnorm(1,m.k,nu.use*C.k)
}

#gibbs sampler

## number of iterations
nsim=20000


## store parameters


beta.mtx=matrix(0,nrow=p*K,ncol=nsim)
L.mtx=matrix(0,nrow=n,ncol=nsim)
omega.mtx=matrix(0,nrow=K,ncol=nsim)
nu.mtx=matrix(0,nrow=K,ncol=nsim)


## initial value


beta.cur=rep(0,p*K)
L.cur=rep(1,n)
omega.cur=rep(1/K,K)
nu.cur=rep(1,K)

## Gibbs Sampler
for (i in 1:nsim) {
  set.seed(i)
  
  ## sample omega
  omega.cur=sample_omega(L.cur)
  omega.mtx[,i]=omega.cur
  
  ## sample L
  L.cur=sample_L(Y,Fmtx,beta.cur,omega.cur,nu.cur)
  L.mtx[,i]=L.cur
  
  ## sample nu
  nu.cur=sapply(1:K,function(k){sample_nu(k,L.cur)})
  nu.mtx[,i]=nu.cur
  
  ## sample beta
  beta.cur=as.vector(sapply(1:K, function(k){sample_beta(k,L.cur,nu.cur)}))
  beta.mtx[,i]=beta.cur
  
  ## show the numer of iterations 
  if(i%%1000==0){
    print(paste("Number of iterations:",i))
  }
  
}

#Cheking posterioir inference result

sample.select.idx=seq(10001,20000,by=1)


post.pred.y.mix=function(idx){
  
  k.vec.use=L.mtx[,idx]
  beta.use=beta.mtx[,idx]
  nu.use=nu.mtx[,idx]
  
  
  get.mean=function(s){
    k.use=k.vec.use[s]
    sum(Fmtx[,s]*beta.use[((k.use-1)*p+1):(k.use*p)])
  }
  get.sd=function(s){
    k.use=k.vec.use[s]
    sqrt(nu.use[k.use])
  }
  mu.y=sapply(1:n, get.mean)
  sd.y=sapply(1:n, get.sd)
  sapply(1:length(mu.y), function(k){rnorm(1,mu.y[k],sd.y[k])})
  
}  




y.post.pred.sample=sapply(sample.select.idx, post.pred.y.mix)


summary.vec95=function(vec){
  c(unname(quantile(vec,0.025)),mean(vec),unname(quantile(vec,0.975)))
}


summary.y=apply(y.post.pred.sample,MARGIN=1,summary.vec95)


plot(Y,type='b',xlab='Time',ylab='',pch=16,ylim=c(-1.2,1.5))
lines(summary.y[2,],type='b',col='grey',lty=2,pch=4)
lines(summary.y[1,],type='l',col='purple',lty=3)
lines(summary.y[3,],type='l',col='purple',lty=3)
legend("topright",legend=c('Truth','Mean','95% C.I.'),lty=1:3,
       col=c('black','grey','purple'),horiz = T,pch=c(16,4,NA))

