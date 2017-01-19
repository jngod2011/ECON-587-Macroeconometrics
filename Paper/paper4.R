library(car)
library(lmtest)
CPI_core <- read.csv("D:/Dropbox/16summer/Macroeconometrics/Paper/CPI_core_graph.csv")
Industrial.Production.Index <- read.csv("D:/Dropbox/16summer/Macroeconometrics/Paper/Industrial Production Index_graph.csv")
a <- c("USA","MEX","CAN","GBR","DEU","AUT","BEL","FRA","LUX","NLD","ESP","FIN","GRC","IRL","ITA","PRT")
table <- matrix(rep(NA,9296),581,16)
colnames(table) <- a
year <- rep(NA,16)

for (k in c(1:16)) {
  pi <- CPI_core[CPI_core[,1]==a[k],c(6,7),drop=F]
  pi1 <- data.frame(rep(NA,(nrow(pi)-1)))
  for (i in 1:(nrow(pi)-1)) {
    pi1[i,1] <- (pi[i+1,2,drop=F]-pi[i,2,drop=F])/pi[i,2,drop=F]
  }
  pi[2:nrow(pi),2]<- ((1+pi1[,1,drop=F])^4-1)*100
  pi <- pi[-1,]
  
  y <- Industrial.Production.Index[Industrial.Production.Index[,1]==a[k],c(6,7),drop=F]
  y1 <- data.frame(rep(NA,(nrow(y)-1)))
  for (i in 1:(nrow(y)-1)) {
    y1[i,1] <- (y[i+1,2,drop=F]-y[i,2,drop=F])/y[i,2,drop=F]
  }
  y[2:nrow(y),2] <- ((1+y1[,1,drop=F])^4-1)*100
  y <- y[-1,]
  
  if(k!=12){
    b <- which(y[,1]==as.character(pi[24,1]))
    year[k] <- as.character(pi[24,1])
    
    for(i in 1:(nrow(pi)-24-119)){
      depen <- pi[(24+i):(24+i+119),2]
      indust <- y[(b+i):(b+i+119),2]
      indepen <- NA
      indepen <- pi[(24+i-1):(24+i+119-1),2,drop=F]
      for (j in 1:23) {
        indepen <- cbind(indepen,pi[(24+i-1-j):(24+i+119-1-j),2,drop=F])
      }
      (formula=depen ~ alpha+indepen[,1]*gamma+indepen[,2]*gamma*(1-gamma)+indepen[,3]*gamma*(1-gamma)^2
      +indepen[,4]*gamma*(1-gamma)^3+indepen[,5]*gamma*(1-gamma)^4+indepen[,6]*gamma*(1-gamma)^5
      +indepen[,7]*gamma*(1-gamma)^6+indepen[,7]*gamma*(1-gamma)^7+indepen[,9]*gamma*(1-gamma)^8
      +indepen[,10]*gamma*(1-gamma)^9+indepen[,11]*gamma*(1-gamma)^10+indepen[,12]*gamma*(1-gamma)^11
      +indepen[,13]*gamma*(1-gamma)^12+indepen[,14]*gamma*(1-gamma)^13+indepen[,15]*gamma*(1-gamma)^14
      +indepen[,16]*gamma*(1-gamma)^15+indepen[,17]*gamma*(1-gamma)^16+indepen[,18]*gamma*(1-gamma)^17
      +indepen[,19]*gamma*(1-gamma)^18+indepen[,20]*gamma*(1-gamma)^19+indepen[,21]*gamma*(1-gamma)^20
      +indepen[,22]*gamma*(1-gamma)^21+indepen[,23]*gamma*(1-gamma)^22+indepen[,24]*gamma*(1-gamma)^23
      +lamda*indust)
      m1 <- nls(formula,start=list(alpha = 0, gamma = 0.4, lamda = 0),control = list(maxiter=500000,warnOnly = TRUE))
      table[i,k]<- coeftest(m1)[2,1]
    }
  }
  else {
    b <- which(pi[,1]==as.character(y[1,1]))
    year[k] <- as.character(y[1,1])
    
    for(i in 1:(nrow(pi)-b-119)){
      depen <- pi[(b+i):(b+i+119),2]
      indust <- y[(i):(i+119),2]
      indepen <- NA
      indepen <- pi[(b+i-1):(b+i+119-1),2,drop=F]
      for (j in 1:23) {
        indepen <- cbind(indepen,pi[(b+i-1-j):(b+i+119-1-j),2,drop=F])
      }
      (formula=depen ~ alpha+indepen[,1]*gamma+indepen[,2]*gamma*(1-gamma)+indepen[,3]*gamma*(1-gamma)^2
      +indepen[,4]*gamma*(1-gamma)^3+indepen[,5]*gamma*(1-gamma)^4+indepen[,6]*gamma*(1-gamma)^5
      +indepen[,7]*gamma*(1-gamma)^6+indepen[,7]*gamma*(1-gamma)^7+indepen[,9]*gamma*(1-gamma)^8
      +indepen[,10]*gamma*(1-gamma)^9+indepen[,11]*gamma*(1-gamma)^10+indepen[,12]*gamma*(1-gamma)^11
      +indepen[,13]*gamma*(1-gamma)^12+indepen[,14]*gamma*(1-gamma)^13+indepen[,15]*gamma*(1-gamma)^14
      +indepen[,16]*gamma*(1-gamma)^15+indepen[,17]*gamma*(1-gamma)^16+indepen[,18]*gamma*(1-gamma)^17
      +indepen[,19]*gamma*(1-gamma)^18+indepen[,20]*gamma*(1-gamma)^19+indepen[,21]*gamma*(1-gamma)^20
      +indepen[,22]*gamma*(1-gamma)^21+indepen[,23]*gamma*(1-gamma)^22+indepen[,24]*gamma*(1-gamma)^23
      +lamda*indust)
      m1 <- nls(formula,start=list(alpha = 0, gamma = 0.4, lamda = 0),control = list(maxiter=500000,warnOnly = TRUE))
      table[i,k]<- coeftest(m1)[2,1]
    }
  }
}

year

opar <- par(no.readonly = TRUE)
par(mfrow=c(2,2),ps=18)
USA <- ts(data=subset(table[,1],!is.na(table[,1])),start = c(1969,1),frequency = 12)
plot(USA,main = "U.S.",ylab = "γ Parameter",ylim=c(-0.1,0.5),col="Blue")
MEX <- ts(data=subset(table[,2],!is.na(table[,2])),start = c(1992,1),frequency = 12)
plot(MEX,main = "Mexico",ylab = "γ Parameter",col="Blue")
CAN <- ts(data=subset(table[,3],!is.na(table[,3])),start = c(1973,1),frequency = 12)
plot(CAN,main = "Canada.",ylab = "γ Parameter",ylim=c(-0.1,0.3),col="Blue")
GBR <- ts(data=subset(table[,4],!is.na(table[,4])),start = c(1982,1),frequency = 12)
plot(GBR,main = "U.K.",ylab = "γ Parameter",ylim=c(-0.1,0.3),col="Blue")
par(opar)
par(mfrow=c(3,2),ps=22)
DEU <- ts(data=subset(table[,5],!is.na(table[,5])),start = c(1974,1),frequency = 12)
plot(DEU,main = "Germany",ylab = "γ Parameter",ylim=c(-0.1,0.3),col="Blue")
AUT <- ts(data=subset(table[,6],!is.na(table[,6])),start = c(1978,1),frequency = 12)
plot(AUT,main = "Austria",ylab = "γ Parameter",ylim=c(-0.1,0.3),col="Blue")
BEL <- ts(data=subset(table[,7],!is.na(table[,7])),start = c(1988,6),frequency = 12)
plot(BEL,main = "Belgium",ylab = "γ Parameter",ylim=c(-0.1,0.3),col="Blue")
FRA <- ts(data=subset(table[,8],!is.na(table[,8])),start = c(1982,1),frequency = 12)
plot(FRA,main = "France",ylab = "γ Parameter",ylim=c(0,0.6),col="Blue")
LUX <- ts(data=subset(table[,9],!is.na(table[,9])),start = c(1979,1),frequency = 12)
plot(LUX,main = "Luxembourg",ylab = "γ Parameter",ylim=c(-0.1,0.3),col="Blue")
NLD <- ts(data=subset(table[,10],!is.na(table[,10])),start = c(1972,4),frequency = 12)
plot(NLD,main = "The Netherlands",ylab = "γ Parameter",ylim=c(-0.1,0.3),col="Blue")
par(opar)
par(mfrow=c(3,2),ps=22)
ESP <- ts(data=subset(table[,11],!is.na(table[,11])),start = c(1988,1),frequency = 12)
plot(ESP,main = "Spain",ylab = "γ Parameter",ylim=c(-0.1,0.3),col="Blue")
FIN <- ts(data=subset(table[,12],!is.na(table[,12])),start = c(1968,2),frequency = 12)
plot(FIN,main = "Finland",ylab = "γ Parameter",ylim=c(-0.1,0.3),col="Blue")
GBC <- ts(data=subset(table[,13],!is.na(table[,13])),start = c(1982,1),frequency = 12)
plot(GBC,main = "Greece",ylab = "γ Parameter",ylim=c(-0.1,0.3),col="Blue")
IRL <- ts(data=subset(table[,14],!is.na(table[,14])),start = c(1987,11),frequency = 12)
plot(IRL,main = "Ireland",ylab = "γ Parameter",ylim=c(-0.1,0.3),col="Blue")
ITA <- ts(data=subset(table[,15],!is.na(table[,15])),start = c(1972,1),frequency = 12)
plot(ITA,main = "Italy",ylab = "γ Parameter",ylim=c(-0.1,0.5),col="Blue")
PRT <- ts(data=subset(table[,16],!is.na(table[,16])),start = c(2000,1),frequency = 12)
plot(PRT,main = "Portugal",ylab = "γ Parameter",ylim=c(-0.1,0.3),col="Blue")
par(opar)
