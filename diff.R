# Load the CO2 dataset in R
data(co2) 

# Take first differences to remove the trend 
co2_1stdiff=diff(co2,differences=1)

# Filter via moving averages to remove the seasonality 
co2_ma=filter(co2,filter=c(1/24,rep(1/12,11),1/24),sides=2)

par(mfrow=c(3,1), cex.lab=1.2,cex.main=1.2)
plot(co2) # plot the original data 
plot(co2_1stdiff) # plot the first differences (removes trend, highlights seasonality)
plot(co2_ma) # plot the filtered series via moving averages (removes the seasonality, highlights the trend)
