a <- arima.sim(n = 200, list(ma = c(-.3,.17)))
acf(a, lag.max=8)

arima(a,order=c(0,0,2))
