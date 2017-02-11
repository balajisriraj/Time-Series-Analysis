###########################
# Electricty Consumption Solution
# Authors: Gautham, Bharat, Balaji
###########################

install.packages("astsa")
library(astsa)
install.packages("tseries")
library(tseries)

#Read Data
rm(list=ls())
readLines("eletricity.dat")
sample = read.table("eletricity.dat",skip = 1, fill = T)
e = read.table("eletricity.dat",skip = 1, fill = T)
e = as.vector(t(e))
e = e[!is.na(e)]

#Change to time series and explore
elect = ts(e,start = 1920, end = 1970, frequency = 1)
elect_bckup = elect
plot(elect_bckup, xlab = "Time", ylab = "Total Electricity Consumption", main = "Time Series Plot")
elect = elect[1:39]
elect = ts(elect,start = 1920, frequency = 1)


#Dickey Fuller Test to determine stationarity
adf.test(elect, alternative = "stationary", k=0)
elect_diff = diff(elect)
adf.test(elect_diff, alternative = "stationary", k=0)
plot(elect_diff, xlab = "Time", ylab = "Total Electricity Consumption", main = "Differenced Data - 1st Order")
plot(diff(elect_diff), xlab = "Time", ylab = "Total Electricity Consumption", main = "Differenced Data - 2nd Order")


#ACF and PACF plots
acf2(elect)
acf2(elect_diff)

#ARIMA Models
elect_model = arima(elect,order = c(1,1,0))
tsdiag(elect_model)
elect_model1 = arima(elect,order = c(1,1,1))
tsdiag(elect_model1)
elect_model2 = arima(elect,order = c(1,2,0))
tsdiag(elect_model2)

#AR1 Model
forecast = predict(elect_model, n.ahead = 12)
plot(elect_bckup, xlab = "Time", ylab = "Total Electricity Consumption", main = "Forecast - ARIMA (1,1,0)")
lines(forecast$pred,type = "o",col = "red")
lines(forecast$pred-1.96*forecast$se, col="blue")
lines(forecast$pred+1.96*forecast$se, col="blue")

#ARMA Model
forecast1 = predict(elect_model1, n.ahead = 12)
plot(elect_bckup, xlab = "Time", ylab = "Total Electricity Consumption", main = "Forecast - ARIMA (1,1,1)")
lines(forecast1$pred,type = "o",col = "red")
lines(forecast1$pred-1.96*forecast1$se, col="blue")
lines(forecast1$pred+1.96*forecast1$se, col="blue")

#AR1 with lag 2
#ARMA Model
forecast2 = predict(elect_model2, n.ahead = 12)
forecast2$pred
plot(elect_bckup, xlab = "Time", ylab = "Total Electricity Consumption", main = "Forecast - ARIMA (1,2,0)")
lines(forecast2$pred,type = "o",col = "red")
lines(forecast2$pred-1.96*forecast2$se, col="blue")
lines(forecast2$pred+1.96*forecast2$se, col="blue")
