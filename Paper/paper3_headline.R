library(car)
library(lmtest)
CPI_headline <- read.csv("D:/Dropbox/16summer/Macroeconometrics/Paper/CPI_headline.csv")
Industrial.Production.Index <- read.csv("D:/Dropbox/16summer/Macroeconometrics/Paper/Industrial Production Index.csv")
a <- as.character(unique(Industrial.Production.Index[,1]))
##1996-2013
table <- matrix(rep(NA,164),41,4)
rownames(table) <- a
colnames(table) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
for(j in c(1:41)){
  b <- a[j]
  pi <- CPI_headline[CPI_headline[,1]==b,7,drop=F]
  pi1 <- data.frame(rep(NA,270))
  for (i in 1:270) {
    pi1[i,1] <- (pi[i+1,1,drop=F]-pi[i,1,drop=F])/pi[i,1,drop=F]
  }
  pi<- ((1+pi1[,1,drop=F])^4-1)*100
  y <- Industrial.Production.Index[Industrial.Production.Index[,1]==b,7,drop=F]
  y1 <- rep(NA,216)
  for (i in 1:216) {
    y1[i] <- (y[i+1,1]-y[i,1])/y[i,1]
  }
  y <- ((1+y1)^4-1)*100
  depen <- pi[25:240,1]
  indust <- y[1:216]
  indepen <- NA
  indepen <- pi[24:239,1,drop=F]
  for (i in 1:23) {
    indepen <- cbind(indepen,pi[(24-i):(239-i),1,drop=F])
  }
  (formula=depen ~ alpha+indepen[,1]*gamma+indepen[,2]*gamma*(1-gamma)+indepen[,3]*gamma*(1-gamma)^2
  +indepen[,4]*gamma*(1-gamma)^3+indepen[,5]*gamma*(1-gamma)^4+indepen[,6]*gamma*(1-gamma)^5
  +indepen[,7]*gamma*(1-gamma)^6+indepen[,8]*gamma*(1-gamma)^7+indepen[,9]*gamma*(1-gamma)^8
  +indepen[,10]*gamma*(1-gamma)^9+indepen[,11]*gamma*(1-gamma)^10+indepen[,12]*gamma*(1-gamma)^11
  +indepen[,13]*gamma*(1-gamma)^12+indepen[,14]*gamma*(1-gamma)^13+indepen[,15]*gamma*(1-gamma)^14
  +indepen[,16]*gamma*(1-gamma)^15+indepen[,17]*gamma*(1-gamma)^16+indepen[,18]*gamma*(1-gamma)^17
  +indepen[,19]*gamma*(1-gamma)^18+indepen[,20]*gamma*(1-gamma)^19+indepen[,21]*gamma*(1-gamma)^20
  +indepen[,22]*gamma*(1-gamma)^21+indepen[,23]*gamma*(1-gamma)^22+indepen[,24]*gamma*(1-gamma)^23
  +lamda*indust)
  m <- nls(formula,start=list(alpha = 0, gamma = 0.2, lamda = 0),control = list(maxiter=500))
  table[j,] <- coeftest(m)[2,]
}
table[order(table[,1],decreasing = TRUE),]
write.csv(table[order(table[,1],decreasing = TRUE),],file="C:/Users/wang_/Desktop/1.csv")
##1996-2016
table <- matrix(rep(NA,164),41,4)
rownames(table) <- a
colnames(table) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
for(j in c(1:41)){
  b <- a[j]
  pi <- CPI_headline[CPI_headline[,1]==b,7,drop=F]
  pi1 <- data.frame(rep(NA,270))
  for (i in 1:270) {
    pi1[i,1] <- (pi[i+1,1,drop=F]-pi[i,1,drop=F])/pi[i,1,drop=F]
  }
  pi<- ((1+pi1[,1,drop=F])^4-1)*100
  y <- Industrial.Production.Index[Industrial.Production.Index[,1]==b,7,drop=F]
  y1 <- rep(NA,246)
  for (i in 1:246) {
    y1[i] <- (y[i+1,1]-y[i,1])/y[i,1]
  }
  y <- ((1+y1)^4-1)*100
  depen <- pi[25:270,1]
  indust <- y[1:246]
  indepen <- NA
  indepen <- pi[24:269,1,drop=F]
  for (i in 1:23) {
    indepen <- cbind(indepen,pi[(24-i):(269-i),1,drop=F])
  }
  (formula=depen ~ alpha+indepen[,1]*gamma+indepen[,2]*gamma*(1-gamma)+indepen[,3]*gamma*(1-gamma)^2
  +indepen[,4]*gamma*(1-gamma)^3+indepen[,5]*gamma*(1-gamma)^4+indepen[,6]*gamma*(1-gamma)^5
  +indepen[,7]*gamma*(1-gamma)^6+indepen[,8]*gamma*(1-gamma)^7+indepen[,9]*gamma*(1-gamma)^8
  +indepen[,10]*gamma*(1-gamma)^9+indepen[,11]*gamma*(1-gamma)^10+indepen[,12]*gamma*(1-gamma)^11
  +indepen[,13]*gamma*(1-gamma)^12+indepen[,14]*gamma*(1-gamma)^13+indepen[,15]*gamma*(1-gamma)^14
  +indepen[,16]*gamma*(1-gamma)^15+indepen[,17]*gamma*(1-gamma)^16+indepen[,18]*gamma*(1-gamma)^17
  +indepen[,19]*gamma*(1-gamma)^18+indepen[,20]*gamma*(1-gamma)^19+indepen[,21]*gamma*(1-gamma)^20
  +indepen[,22]*gamma*(1-gamma)^21+indepen[,23]*gamma*(1-gamma)^22+indepen[,24]*gamma*(1-gamma)^23
  +lamda*indust)
  m <- nls(formula,start=list(alpha = 0, gamma = 0.2, lamda = 0),control = list(maxiter=500))
  table[j,] <- coeftest(m)[2,]
}
table[order(table[,1],decreasing = TRUE),]
write.csv(table[order(table[,1],decreasing = TRUE),],file="C:/Users/wang_/Desktop/2.csv")
