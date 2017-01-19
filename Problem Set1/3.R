sometime <- read.csv("D:/Dropbox/16summer/Macroeconometrics/Problem Set1/sometimeseriesdata.csv")
attach(sometime)
acf(rebekah,10)
pacf(rebekah,10)
##MA(1),beta<0
acf(daniel,10)
pacf(daniel,10)
##ARMA(1,1),a1>0
acf(zhuoxiansheng,10)
pacf(zhuoxiansheng,10)
##AR(1),a1>0
acf(sylvia,10)
pacf(sylvia,10)
##AR(1),a1<0
acf(zhirui,10)
pacf(zhirui,10)
##MA(1),beta>0
acf(hao,10)
pacf(hao,10)
##AR(1),a1>0
acf(joanne,10)
pacf(joanne,10)
##white noise
acf(sebastian,10)
pacf(sebastian,10)
##ARMA(1,1),a1>0
