####################### Google Search Interest Data ######################
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
data$yt=data$interest

plot(interest) # 249 observations total 
k=9
T=length(interest)-k # We take the first 
# 240 observations only as our data
ts_data=interest[1:T]
ts_validation_data <- interest[(T+1):249]

data <- list(yt = ts_data)


index=seq(2004, 2024.75, length.out = length(interest))
index_filt=index[1:T]
index_forecast=index[(T+1):(T+k)]

index=seq(1, 249, length.out = length(interest))
index_filt=index[1:T]
index_forecast=index[(T+1):(T+k)]


# Now let's look at the DLM package 
library(dlm)
model=dlmModPoly(order=1,dV=1,dW=1,m0=50,C0=10)
results_filtered_dlm=dlmFilter(interest[1:T],model)
results_smoothed_dlm=dlmSmooth(results_filtered_dlm)
results_forecast_dlm=dlmForecast(model, nAhead = k )


plot(index, interest, ylab = "Interest", 
     main = "Google Searches Forecast for Texas ",
     type='l', xlab="Time",lty=3,ylim=c(50,100), xlim=c(0, 250))
#points(index_filt,interest[1:T],pch=20)
lines(index_filt,results_filtered_dlm$m[-1],col='red',lwd=2)
lines(index_filt,results_smoothed_dlm$s[-1],col='blue',lwd=2)
lines(index_forecast,results_forecast_dlm$f,col='green',lwd=2)
points(index_forecast,interest[(T+1):(T+k)],lty=2)

length(results_forecast_dlm$f)
length(index_forecast)
points(T+k,results_forecast_dlm$newStates,col='green',lwd=2)



index3=seq(1, 249, length.out = length(interest))
index_all3=index[200: (T+k)]
index_filt3=index[200:T]
index_forecast3=index[(T+1):(T+k)]


plot(index_all3, interest[index_all3], ylab = "Interest", 
     main = "Google Searches Forecast for Texas ",
     type='l', xlab="Time",lty=3,ylim=c(50,100), xlim=c(200, 250))
#points(index_filt,interest[1:T],pch=20)
lines(index_filt3,results_filtered_dlm$m[-1][index_filt3],col='red',lwd=2)
lines(index_filt3,results_smoothed_dlm$s[-1][index_filt3],col='blue',lwd=2)
lines(index_forecast,results_forecast_dlm$f,col='green',lwd=2)

lines(index_forecast,results_forecast_dlm$f,col='green',lwd=2)


points(index_forecast,interest[(T+1):(T+k)],lty=1)


# Similarly, for the second order polynomial and the Interest data:
#using the DLM package: 
model2=dlmModPoly(order=2,dV=1,dW=1,m0=c(50,0),C0=(10,0,0,10)
results_filtered_dlm2=dlmFilter(interest[1:T],model)
results_smoothed_dlm2=dlmSmooth(results_filtered_dlm2)
results_forecast_dlm2=dlmForecast(model2, nAhead = k )
# filtering and smoothing 
results_filtered_dlm2=dlmFilter(data$yt,model2)
results_smoothed_dlm2=dlmSmooth(results_filtered_dlm2)

index=seq(2004, 2024, length.out = length(interest))
index_filt=index[1:T]

plot(index, interest,main = "Google Searches for Texas ", type='l',xlab="time",
     ylim=c(50,100))
lines(index,results_filtered_dlm2$m[-1,1],
      col='red',lwd=2)
lines(index,results_smoothed_dlm2$s[-1,1],
      col='blue',lwd=2)
lines(index_forecast,results_forecast_dlm$Q[-1],col='green',lwd=2)






