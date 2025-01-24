library(zoo)
dat=read.csv.zoo('/Users/mac/Downloads/multiTimeline (2).csv')
head(dat)
max(dat$interest)
length(dat$Month)
data=as.data.frame(dat)
head(data)

plot(data$interest, xlab="Monthly Time Increments",ylab="Interest", 
     main = "Google Search Interests for Texas 2004-2024", pch='.',)
lines(dat$interest, type='l', lty=1)
abline(v=164, col="red")


interest=zoo(dat$interest, seq(from=as.Date("2004-01-01"), to=as.Date("2024-09-01"), by="1 month"))

head(interest)

plot(interest)

data$yt=data$interest

####################### Google Search Interest Data ######################
plot(interest) # 249 observations total 
k=9
T=length(interest)-k # We take the first 
# 240 observations only as our data
ts_data=interest[1:T]
ts_validation_data <- interest[(T+1):249]

data <- list(yt = ts_data)

## set up dlm matrices
GG <- as.matrix(1)
FF <- as.matrix(1)
VV <- as.matrix(1)
WW <- as.matrix(1)
m0 <- as.matrix(570)
C0 <- as.matrix(1e4)

## wrap up all matrices and initial values
matrices <- set_up_dlm_matrices(FF, GG, VV, WW)
initial_states <- set_up_initial_states(m0, C0)

## filtering and smoothing 
results_filtered <- forward_filter(data, matrices, 
                                   initial_states)
results_smoothed <- backward_smoothing(data, matrices, 
                                       results_filtered)

index=seq(2004, 2024, length.out = length(interest))
index_filt=index[1:T]


par(mfrow=c(1,1))
plot(index, interest, main = "Google Searches for Texas ",type='l',
     xlab="Time",ylab="Interest",lty=3,ylim=c(50,100))
points(index,interest,pch=1)
lines(index_filt, results_filtered$mt, type='l', 
      col='red',lwd=2)
lines(index_filt, results_smoothed$mnt, type='l', 
      col='blue',lwd=2)


# Now let's look at the DLM package 
library(dlm)
model=dlmModPoly(order=1,dV=1,dW=1,m0=570,C0=1e4)
results_filtered_dlm=dlmFilter(interest[1:T],model)
results_smoothed_dlm=dlmSmooth(results_filtered_dlm)

plot(index_filt, interest[1:T], ylab = "Interest", 
     main = "Google Searches for Texas ARIMA(1)",
     type='l', xlab="Time",lty=3,ylim=c(50,100))
#points(index_filt,interest[1:T],pch=20)
lines(index_filt,results_filtered_dlm$m[-1],col='red',lwd=2)
lines(index_filt,results_smoothed_dlm$s[-1],col='blue',lwd=2)

# Similarly, for the second order polynomial and the Interest data:
T=length(interest)
data=list(yt = interest)

FF <- (as.matrix(c(1,0)))
GG <- matrix(c(1,1,0,1),ncol=2,byrow=T)
VV <- as.matrix(10)
WW <- 0.0001*diag(2)
m0 <- t(as.matrix(c(315,0)))
C0 <- 10*diag(2)

## wrap up all matrices and initial values
matrices <- set_up_dlm_matrices(FF,GG, VV, WW)
initial_states <- set_up_initial_states(m0, C0)

## filtering and smoothing 
results_filtered_2 <- forward_filter(data, matrices, 
                                     initial_states)
results_smoothed2 <- backward_smoothing(data, matrices, 
                                        results_filtered)

#### Now, using the DLM package: 
model2=dlmModPoly(order=2,dV=200,dW=0.01*rep(1,2),
                  m0=c(320,0),C0=10*diag(2))
# filtering and smoothing 
results_filtered_dlm2=dlmFilter(data$yt,model2)
results_smoothed_dlm2=dlmSmooth(results_filtered_dlm2)

index=seq(2004, 2024, length.out = length(interest))
index_filt=index[1:T]

par(mfrow=c(1,1))
plot(index, interest,main = "Google Searches for Texas ARIMA(2)", type='l',xlab="time",
     ylim=c(50,100))
lines(index, results_filtered_2$mt[,1],
      col='red',lwd=2)
lines(index,results_smoothed2$mnt[,1],
      col='blue',lwd=2)

plot(index, interest,main = "Google Searches for Texas ", type='l',xlab="time",
     ylim=c(50,100))
lines(index,results_filtered_dlm2$m[-1,1],
      col='red',lwd=2)
lines(index,results_smoothed_dlm2$s[-1,1],
      col='blue',lwd=2)





