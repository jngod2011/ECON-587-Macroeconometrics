library(forecast)
ts<- read.csv("D:/Dropbox/16summer/Macroeconometrics/Problem Set2/ps2prob5_data.csv")
aic <- rep(NA,10)
for(i in 1:10){
  aic[i] <- arima(ts[,2],order = c(i,0,0),include.mean = 0)$aic
}
aic
arima(ts[,2],c(3,0,0),include.mean = 0)
Box.test(arima(ts[,2],c(3,0,0),include.mean = 0)$residuals,type="Ljung-Box")
loglik <- rep(NA,10)
for(i in 1:10){
  loglik[i] <- arima(ts[,2],order = c(i,0,0),include.mean = 0)$loglik
}
MyIC <- rep(NA,10)
step <- seq(0,2,1e-5)
parameter <- rep(NA,length(step))
l <- c(1:10)
for (j in 1:length(step)) {
    MyIC<- -2*loglik/145+step[j]*l/145
  if(MyIC[8]==min(MyIC)){
    parameter [j] <- step[j]
  }
}
min(subset(parameter,!is.na(parameter)))
max(subset(parameter,!is.na(parameter)))
arima(ts[,2],c(8,0,0),include.mean = 0)
Box.test(arima(ts[,2],c(8,0,0),include.mean = 0)$residuals,type="Ljung-Box")
