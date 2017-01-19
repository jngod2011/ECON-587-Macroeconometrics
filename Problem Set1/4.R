library(forecast)
library(tseries)
medicare <- read.csv("D:/Dropbox/16summer/Macroeconometrics/Problem Set1/medicare.csv")
adf.test(medicare$Net.Medicare)
medi.d <- diff(medicare[,2])
adf.test(medi.d)
plot(2016:2090,medi.d,type = "l",xlab = "year",ylab = "d_medicare",
     main = "CBO's Projection of Net Medicare Spending(differenced)",col="blue")
acf(medi.d)
pacf(medi.d)
auto.arima(medi.d,ic="aic")
res.medi <- arima(medi.d,order = c(0,0,2))
Box.test(res.medi$residuals,type="Ljung-Box")


sum(arma(medi.d,lag=list(ar=2))$residuals[-(1:2)]^2)
sum(auto.arima(medi.d,ic="aic")$residuals^2)
arma(medi.d,lag=list(ar=2))$css