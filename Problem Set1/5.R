library(forecast)
library(tseries)
library(car)
GDPDEF <- read.csv("C:/Users/wang_/Desktop/GDPDEF-2.csv")
UNRATE.2 <- read.csv("C:/Users/wang_/Desktop/UNRATE-2.csv")
inflation <- rep(NA,239)
for(i in 1:239){
  inflation[i] <- ((1+(GDPDEF[i+1,2]-GDPDEF[i,2])/GDPDEF[i,2])^4)-1
}
inflation.date <- cbind.data.frame(GDPDEF$DATE[-1],inflation)
colnames(inflation.date)[1] <- "DATE"
inflation.date$DATE<- as.Date(inflation.date$DATE,format="%m/%d/%Y")
##a
adf.test(inflation)
acf(inflation)
pacf(inflation)
ar(inflation)
Box.test(ar(inflation)$resid,type="Ljung-Box")
##b
plot(inflation.date$DATE,inflation.date$inflation,type="l")
ar.inflation1 <- ar(inflation[1:143],aic = FALSE,order.max = 8)
ar.inflation2 <- ar(inflation[144:239],aic = FALSE,order.max = 8)
SSRp <- sum(ar(inflation)$resid[-(1:8)]^2)
SSR1 <- sum(ar.inflation1$resid[-(1:8)]^2)
SSR2 <- sum(ar.inflation2$resid[-(1:8)]^2)
chow <- ((SSRp-SSR1-SSR2)/(SSR1+SSR2))*((239-2*8)/8)
print(chow)
qf(0.95,8,(239-8*2))
##c
unrate <- UNRATE.2[,2]
L.unrate <- c(unrate[-240])
L2.unrate <- c(NA,L.unrate[-239])
L3.unrate <- c(NA,L2.unrate[-239])
L4.unrate <- c(NA,L3.unrate[-239])
L5.unrate <- c(NA,L4.unrate[-239])
arima(inflation[-(1:4)],order = c(1,0,0),xreg =cbind(L.unrate)[-c(1:4),])$aic
arima(inflation[-(2:4)],order = c(2,0,0),xreg =cbind(L.unrate,L2.unrate)[-c(2:4),])$aic
arima(inflation[-(3:4)],order = c(3,0,0),xreg =cbind(L.unrate,L2.unrate,L3.unrate)[-c(3:4),])$aic
arima(inflation[-4],order = c(4,0,0),xreg =cbind(L.unrate,L2.unrate,L3.unrate,L4.unrate)[-4,])$aic
arima(inflation,order = c(5,0,0),xreg =cbind(L.unrate,L2.unrate,L3.unrate,L4.unrate,L5.unrate))$aic
arima(inflation,order = c(3,0,0),xreg =cbind(L.unrate,L2.unrate,L3.unrate))
Box.test(arima(inflation,order = c(3,0,0),xreg =cbind(L.unrate,L2.unrate,L3.unrate))$resid,type="Ljung-Box")
##d
philps1 <- arima(inflation[c(1:167)],order = c(3,0,0),xreg =cbind(L.unrate,L2.unrate,L3.unrate)[c(1:167),])
linearHypothesis(philps1,c(0,0,0,0,1,1,1))
philps2 <- arima(inflation[c(168:239)],order = c(3,0,0),xreg =cbind(L.unrate,L2.unrate,L3.unrate)[c(168:239),])
linearHypothesis(philps2,c(0,0,0,0,1,1,1))
