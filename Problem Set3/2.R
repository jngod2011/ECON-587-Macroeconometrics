suppressMessages(library(lmtest))
arima(data$ms5)
Box.test(arima(data$ms5)$resid,type="Ljung-Box")
plot(data$date,data$ms5)
a <- rep(NA,194)
b <- rep(NA,194)
for(i in 10:194){
  D1 <- c(rep(0,i),rep(1,204-i))
  if(coeftest(arima(data$ms5,xreg = D1))[2,4]<=0.05){
    a[i] <- 1
  }
  if(Box.test(arima(data$ms5,xreg = D1)$resid,type="Ljung-Box")[3]>=0.70){
    b[i] <- 1
  }
}
intersect(which(a==1),which(b==1))
data[intersect(which(a==1),which(b==1)),1]
D1 <- c(rep(0,63),rep(1,204-63))
coeftest(arima(data$ms5,xreg = D1))
arima(data$ms5,xreg = D1)
Box.test(arima(data$ms5,xreg = D1)$resid,type="Ljung-Box")


arima(data$ms6,xreg = data$ms7)
Box.test(arima(data$ms6,xreg = data$ms7)$resid,type="Ljung-Box")
plot(data$ms7,data$ms6)
plot(data$date,data$ms7)
plot(data$date,data$ms6)
a <- rep(NA,194)
b <- rep(NA,194)
for(i in 10:194){
  D2<- c(rep(0,i),rep(1,204-i))
  if(linearHypothesis(lm(data$ms6 ~ data$ms7+D2*data$ms7),c("D2=0","data$ms7:D2=0"))[2,6]<=0.05){
    a[i] <- 1
  }
  if(Box.test(lm(data$ms6 ~ data$ms7+D2*data$ms7)$resid,type="Ljung-Box")[3]>=0.70){
    b[i] <- 1
  }
}
intersect(which(a==1),which(b==1))
data[intersect(which(a==1),which(b==1)),1]
D2<- c(rep(0,105),rep(1,204-105))
linearHypothesis(lm(data$ms6 ~ data$ms7+D2*data$ms7),c("D2=0","data$ms7:D2=0"))
lm(data$ms6 ~ data$ms7+D2*data$ms7)
Box.test(lm(data$ms6 ~ data$ms7+D2*data$ms7)$resid,type="Ljung-Box")
