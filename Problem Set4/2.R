suppressMessages(library(vars))
OILPRICE <- read.csv("D:/Dropbox/16summer/Macroeconometrics/Problem Set4/OILPRICE.csv")
GDPDEF <- read.csv("D:/Dropbox/16summer/Macroeconometrics/Problem Set4/GDPDEF.csv")
GDPC1 <- read.csv("D:/Dropbox/16summer/Macroeconometrics/Problem Set4/GDPC1.csv")
OILPRICE[,2] <- OILPRICE[,2]/GDPDEF[,2]
oilprice <- rep(NA,243)
for (i in 1:243) {
  oilprice[i] <- (OILPRICE[i+1,2]-OILPRICE[i,2])/OILPRICE[i,2]
}
oilprice <- (1+oilprice)^4-1
rGDP <- GDPC1[,2]
##a
y=cbind(oilprice,rGDP)
VARselect(y)
oil.var <- VAR(y,ic="aic")
coef(oil.var)
##b
plot(irf(VAR(y,ic="aic")))
fevd(oil.var,n.ahead=10)
