#################################################
##### Univariate DLM: Known, constant variances
#################################################
set_up_dlm_matrices <- function(FF, GG, VV, WW){
  return(list(FF=FF, GG=GG, VV=VV, WW=WW))
}

set_up_initial_states <- function(m0, C0){
  return(list(m0=m0, C0=C0))
}

### forward update equations ###
forward_filter <- function(data, matrices, initial_states){
  ## retrieve dataset
  yt <- data$yt
  T <- length(yt)
  
  ## retrieve a set of quadruples 
  # FF, GG, VV, WW are scalar
  FF <- matrices$FF  
  GG <- matrices$GG
  VV <- matrices$VV
  WW <- matrices$WW
  
  ## retrieve initial states
  m0 <- initial_states$m0
  C0 <- initial_states$C0
  
  ## create placeholder for results
  d <- dim(GG)[1]
  at <- matrix(NA, nrow=T, ncol=d)
  Rt <- array(NA, dim=c(d, d, T))
  ft <- numeric(T)
  Qt <- numeric(T)
  mt <- matrix(NA, nrow=T, ncol=d)
  Ct <- array(NA, dim=c(d, d, T))
  et <- numeric(T)
  
  for(i in 1:T){
    # moments of priors at t
    if(i == 1){
      at[i, ] <- GG %*% t(m0)
      Rt[, , i] <- GG %*% C0 %*% t(GG) + WW
      Rt[, , i] <- 0.5*Rt[, , i]+0.5*t(Rt[, , i])
    }else{
      at[i, ] <- GG %*% t(mt[i-1, , drop=FALSE])
      Rt[, , i] <- GG %*% Ct[, , i-1] %*% t(GG) + WW
      Rt[, , i] <- 0.5*Rt[, , i]+0.5*t(Rt[, , i])
    }
    
    # moments of one-step forecast:
    ft[i] <- t(FF) %*% (at[i, ]) 
    Qt[i] <- t(FF) %*% Rt[, , i] %*% FF + VV
    
    # moments of posterior at t:
    At <- Rt[, , i] %*% FF / Qt[i]
    et[i] <- yt[i] - ft[i]
    mt[i, ] <- at[i, ] + t(At) * et[i]
    Ct[, , i] <- Rt[, , i] - Qt[i] * At %*% t(At)
    Ct[, , i] <- 0.5*Ct[, , i]+ 0.5*t(Ct[, , i])
  }
  cat("Forward filtering is completed!") # indicator of completion
  return(list(mt = mt, Ct = Ct, at = at, Rt = 
                Rt, ft = ft, Qt = Qt))
}

forecast_function <- function(posterior_states, k, matrices){
  
  ## retrieve matrices
  FF <- matrices$FF
  GG <- matrices$GG
  WW <- matrices$WW
  VV <- matrices$VV
  mt <- posterior_states$mt
  Ct <- posterior_states$Ct
  
  ## set up matrices
  T <- dim(mt)[1] # time points
  d <- dim(mt)[2] # dimension of state-space parameter vector
  
  ## placeholder for results
  at <- matrix(NA, nrow = k, ncol = d)
  Rt <- array(NA, dim=c(d, d, k))
  ft <- numeric(k)
  Qt <- numeric(k)
  
  
  for(i in 1:k){
    ## moments of state distribution
    if(i == 1){
      at[i, ] <- GG %*% t(mt[T, , drop=FALSE])
      Rt[, , i] <- GG %*% Ct[, , T] %*% t(GG) + WW
      Rt[, , i] <- 0.5*Rt[, , i]+0.5*t(Rt[, , i])
    }else{
      at[i, ] <- GG %*% t(at[i-1, , drop=FALSE])
      Rt[, , i] <- GG %*% Rt[, , i-1] %*% t(GG) + WW
      Rt[, , i] <- 0.5*Rt[, , i]+0.5*t(Rt[, , i])
    }
    
    ## moments of forecast distribution
    ft[i] <- t(FF) %*% t(at[i, , drop=FALSE])
    Qt[i] <- t(FF) %*% Rt[, , i] %*% FF + VV
  }
  cat("Forecasting is completed!") # indicator of completion
  return(list(at=at, Rt=Rt, ft=ft, Qt=Qt))
}

## obtain 95% credible interval
get_credible_interval <- function(mu, sigma2, 
                                  quantile = c(0.025, 0.975)){
  z_quantile <- qnorm(quantile)
  bound <- matrix(0, nrow=length(mu), ncol=2)
  bound[, 1] <- mu + 
    z_quantile[1]*sqrt(as.numeric(sigma2)) # lower bound
  bound[, 2] <- mu + 
    z_quantile[2]*sqrt(as.numeric(sigma2)) # upper bound
  return(bound)
}

####################### Example: Lake Huron Data ######################
plot(LakeHuron,main="Lake Huron Data",
     ylab="level in feet") # Total of 98 observations 
k=4
T=length(LakeHuron)-k # We take the first 94 observations 
# only as our data
ts_data=LakeHuron[1:T]
ts_validation_data <- LakeHuron[(T+1):98]

data <- list(yt = ts_data)

# First order polynomial model 

## set up the DLM matrices 
FF <- as.matrix(1)
GG <- as.matrix(1)
VV <- as.matrix(1)
WW <- as.matrix(1)
m0 <- as.matrix(570)
C0 <- as.matrix(1e4)

## wrap up all matrices and initial values
matrices <- set_up_dlm_matrices(FF, GG, VV, WW)
initial_states <- set_up_initial_states(m0, C0)

## filtering
results_filtered <- forward_filter(data, matrices, 
                                   initial_states)
names(results_filtered)

ci_filtered <- get_credible_interval(results_filtered$mt, 
                                     results_filtered$Ct)

## forecasting 
results_forecast <- forecast_function(results_filtered,k, 
                                      matrices)
ci_forecast <- get_credible_interval(results_forecast$ft, 
                                     results_forecast$Qt)

index=seq(1875, 1972, length.out = length(LakeHuron))
index_filt=index[1:T]
index_forecast=index[(T+1):98]

plot(index, LakeHuron, ylab = "level", 
     main = "Lake Huron Level",type='l',
     xlab="time",lty=3,ylim=c(574,584))
points(index,LakeHuron,pch=20)

lines(index_filt, results_filtered$mt, type='l',
      col='red',lwd=2)
lines(index_filt, ci_filtered[, 1], type='l', 
      col='red', lty=2)
lines(index_filt, ci_filtered[, 2], type='l', col='red', lty=2)


lines(index_forecast, results_forecast$ft, type='l',
      col='green',lwd=2)
lines(index_forecast, ci_forecast[, 1], type='l',
      col='green', lty=2)
lines(index_forecast, ci_forecast[, 2], type='l',
      col='green', lty=2)

legend('bottomleft', legend=c("filtered","forecast"),
       col = c("red", "green"), lty=c(1, 1))

#Now consider a 100 times smaller signal to noise ratio 
VV <- as.matrix(1)
WW <- as.matrix(0.01)
matrices_2 <- set_up_dlm_matrices(FF,GG, VV, WW)

## filtering
results_filtered_2 <- forward_filter(data, matrices_2, 
                                     initial_states)
ci_filtered_2 <- get_credible_interval(results_filtered_2$mt, 
                                       results_filtered_2$Ct)

results_forecast_2 <- forecast_function(results_filtered_2, 
                                        length(ts_validation_data), 
                                        matrices_2)
ci_forecast_2 <- get_credible_interval(results_forecast_2$ft, 
                                       results_forecast_2$Qt)


plot(index, LakeHuron, ylab = "level", 
     main = "Lake Huron Level",type='l',
     xlab="time",lty=3,ylim=c(574,584))
points(index,LakeHuron,pch=20)

lines(index_filt, results_filtered_2$mt, type='l', 
      col='magenta',lwd=2)
lines(index_filt, ci_filtered_2[, 1], type='l', 
      col='magenta', lty=2)
lines(index_filt, ci_filtered_2[, 2], type='l', 
      col='magenta', lty=2)

lines(index_forecast, results_forecast_2$ft, type='l', 
      col='green',lwd=2)
lines(index_forecast, ci_forecast_2[, 1], type='l', 
      col='green', lty=2)
lines(index_forecast, ci_forecast_2[, 2], type='l', 
      col='green', lty=2)

legend('bottomleft', legend=c("filtered","forecast"),
       col = c("magenta", "green"), lty=c(1, 1))

plot(index_filt,results_filtered$mt,type='l',col='red',lwd=2,
     ylim=c(574,584),ylab="level")
lines(index_filt,results_filtered_2$mt,col='magenta',lwd=2)
points(index,LakeHuron,pch=20)
lines(index,LakeHuron,lty=2)

