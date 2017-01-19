library(lmtest)
library(car)
ts <- read.csv("D:/Dropbox/16summer/Macroeconometrics/Problem Set2/1.csv")
##i
GDP <- ts[,3]
L.GDP <- c(NA,GDP[-239])
L2.GDP <- c(NA,L.GDP[-239])
L3.GDP <- c(NA,L2.GDP[-239])
arima(ts[,4],order = c(2,0,0),xreg =cbind(GDP,L.GDP,L2.GDP))

##ii
arima(ts[-(2:3),4],order = c(2,0,0),xreg =cbind(GDP,L.GDP,L2.GDP)[-c(2:3),])$aic
arima(ts[-(2:3),4],order = c(3,0,0),xreg =cbind(GDP,L.GDP,L2.GDP,L3.GDP)[-c(2:3),])$aic
arima(ts[-(2:3),4],order = c(3,0,0),xreg =cbind(GDP,L.GDP,L2.GDP)[-c(2:3),])$aic
arima(ts[-(2:3),4],order = c(2,0,0),xreg =cbind(GDP,L.GDP,L2.GDP,L3.GDP)[-c(2:3),])$aic
arima(ts[-(2:3),4],order = c(1,0,0),xreg =cbind(GDP,L.GDP)[-c(2:3),])$aic
arima(ts[-(2:3),4],order = c(2,0,0),xreg =cbind(GDP,L.GDP)[-c(2:3),])$aic
arima(ts[-(2:3),4],order = c(1,0,0),xreg =cbind(GDP,L.GDP,L2.GDP)[-c(2:3),])$aic
##2 unrate lag and 2 GDP lag is the best

##iii
SSRp <- sum(arima(ts[,4],order = c(2,0,0),xreg =cbind(GDP,L.GDP,L2.GDP))$residuals[-(1:2)]^2)
SSR1 <- sum(arima(ts[1:143,4],order = c(2,0,0),xreg =cbind(GDP,L.GDP,L2.GDP)[1:143,])$residuals[-(1:2)]^2)
SSR2 <- sum(arima(ts[144:239,4],order = c(2,0,0),xreg =cbind(GDP,L.GDP,L2.GDP)[144:239,])$residuals^2)
chow <- ((SSRp-SSR1-SSR2)/(SSR1+SSR2))*((239-2*6)/6)
print(chow)
qf(0.95,6,(239-6*2))

##iv
lm(ts[,4]~GDP)
D <- c(rep(0,143),rep(1,(239-143)))
reg <- lm(ts[,4]~D+GDP*D)
coeftest(reg)
linearHypothesis(reg,c("D=0","D:GDP=0"))
reg1 <- lm(ts[1:143,4]~GDP[1:143])
reg2 <- lm(ts[144:239,4]~GDP[144:239])
-reg1$coefficients[1]/reg1$coefficients[2]
-reg2$coefficients[1]/reg2$coefficients[2]

