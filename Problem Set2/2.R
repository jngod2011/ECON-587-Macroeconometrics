library(forecast)
masim <- arima.sim(list(ma=c(-0.3,0.17)),n=200)
# a <- rnorm(202)
# masim <- rep(NA,200)
# for(i in 3:202){
#   masim[i-2] <- a[i]-0.3*a[i-1]+0.17*a[i-2]
# }
acf(masim,8)$acf
auto.arima(masim)
Box.test(auto.arima(masim)$residuals[1:8],type="Ljung-Box")