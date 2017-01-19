library(forecast)
library(tseries)
##a
ts1<- read.csv("D:/Dropbox/16summer/Macroeconometrics/Problem Set2/41.csv")
adf.test(ts1[,2])
auto.arima(ts1[,2])
Box.test(auto.arima(ts1[,2])$residuals,type="Ljung-Box")
##b
ts2<- read.csv("D:/Dropbox/16summer/Macroeconometrics/Problem Set2/42.csv")
adf.test(ts2[,2])
auto.arima(ts2[,2])
Box.test(auto.arima(ts2[,2])$residuals,type="Ljung-Box")
##c
ts3<- read.csv("D:/Dropbox/16summer/Macroeconometrics/Problem Set2/43.csv")
adf.test(ts3[,2])
auto.arima(ts3[,2])
Box.test(auto.arima(ts3[,2])$residuals,type="Ljung-Box")
