suppressMessages(library(car))
Inflation <- read.csv("D:/Dropbox/16summer/Macroeconometrics/Problem Set3/CPILFESL.csv")
GDPC1 <- read.csv("D:/Dropbox/16summer/Macroeconometrics/Problem Set3/GDPC1.csv")
GDPPOT <- read.csv("D:/Dropbox/16summer/Macroeconometrics/Problem Set3/GDPPOT.csv")
GAP <- 100*(log(GDPC1[,2])-log(GDPPOT[,2]))
L.GAP <- c(NA,GAP[-208])
L2.GAP <- c(NA,L.GAP[-208])
L3.GAP <- c(NA,L2.GAP[-208])
L4.GAP <- c(NA,L3.GAP[-208])
armax <- arima(Inflation[,2],order = c(4,0,0),xreg =cbind(L.GAP,L2.GAP,L3.GAP,L4.GAP))
Box.test(armax$resid,type="Ljung-Box")
# gama1=rep(NA,208-19)
# gama2=rep(NA,208-19)
# gama3=rep(NA,208-19)
# gama4=rep(NA,208-19)
# for (i in 20:208) {
#   armax <- arima(Inflation[1:i,2],order = c(4,0,0),xreg =cbind(L.GAP,L2.GAP,L3.GAP,L4.GAP)[1:i,])
#   gama1[i-19] <- armax$coef[6]
#   gama2[i-19] <- armax$coef[7]
#   gama3[i-19] <- armax$coef[8]
#   gama4[i-19] <- armax$coef[9]
# }
# 
# plot(20:208,gama1,type="l",main="Rolling window estimation for gama1", 
#      xlab="End period")
# plot(20:208,gama2,type="l",main="Rolling window estimation for gama2", 
#      xlab="End period")
# plot(20:208,gama3,type="l",main="Rolling window estimation for gama3", 
#      xlab="End period")
# plot(20:208,gama4,type="l",main="Rolling window estimation for gama4", 
#      xlab="End period")
a <- rep(NA,207)
b <- rep(NA,207)
for(i in 10:197){
  D <- c(rep(0,i),rep(1,208-i))
  DLGAP <- D*L.GAP
  DL2GAP <- D*L2.GAP
  DL3GAP <- D*L3.GAP
  DL4GAP <- D*L4.GAP
  armax.test <- arima(Inflation[,2],order = c(4,0,0),xreg =cbind(D,L.GAP,L2.GAP,L3.GAP,L4.GAP,DLGAP,DL2GAP,DL3GAP,DL4GAP))
  if(linearHypothesis(armax.test,c("DLGAP=0","DL2GAP=0","DL3GAP=0","DL4GAP=0"))[2,3]<=0.05){
    a[i] <- 1
  }
  if(Box.test(armax.test$resid,type="Ljung-Box")[3]>=0.70){
    b[i] <- 1
  }
}
intersect(which(a==1),which(b==1))
data[intersect(which(a==1),which(b==1)),1]
D <- c(rep(0,68),rep(1,208-68))
DLGAP <- D*L.GAP
DL2GAP <- D*L2.GAP
DL3GAP <- D*L3.GAP
DL4GAP <- D*L4.GAP
arima(Inflation[,2],order = c(4,0,0),xreg =cbind(D,L.GAP,L2.GAP,L3.GAP,L4.GAP,DLGAP,DL2GAP,DL3GAP,DL4GAP))
armax.test <- arima(Inflation[,2],order = c(4,0,0),xreg =cbind(D,L.GAP,L2.GAP,L3.GAP,L4.GAP,DLGAP,DL2GAP,DL3GAP,DL4GAP))
coeftest(armax.test)
linearHypothesis(armax.test,c("DLGAP=0","DL2GAP=0","DL3GAP=0","DL4GAP=0"))
linearHypothesis(armax.test,c("DLGAP+DL2GAP+DL3GAP+DL4GAP=0"))
