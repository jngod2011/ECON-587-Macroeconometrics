library(car)
library(lmtest)
CPIAUCSL2 <- read.csv("D:/Dropbox/16summer/Macroeconometrics/Paper/CPIAUCSL (3).csv")
INDPRO <- read.csv("D:/Dropbox/16summer/Macroeconometrics/Paper/INDPRO.csv")
pi <- CPIAUCSL2[,2,drop=F]
y <- INDPRO[,2,drop=F]
##US result 83-89
depen <- pi[432:515,]
indust <- y[1:84,]
indepen <- NA
indepen <- pi[431:514,,drop=F]
for (i in 1:47) {
  indepen <- cbind(indepen,pi[(431-i):(514-i),,drop=F])
}
(formula=depen ~ alpha+indepen[,1]*gamma+indepen[,2]*gamma*(1-gamma)+indepen[,3]*gamma*(1-gamma)^2
+indepen[,4]*gamma*(1-gamma)^3+indepen[,5]*gamma*(1-gamma)^4+indepen[,6]*gamma*(1-gamma)^5
+indepen[,7]*gamma*(1-gamma)^6+indepen[,8]*gamma*(1-gamma)^7+indepen[,9]*gamma*(1-gamma)^8
+indepen[,10]*gamma*(1-gamma)^9+indepen[,11]*gamma*(1-gamma)^10+indepen[,12]*gamma*(1-gamma)^11
+indepen[,13]*gamma*(1-gamma)^12+indepen[,14]*gamma*(1-gamma)^13+indepen[,15]*gamma*(1-gamma)^14
+indepen[,16]*gamma*(1-gamma)^15+indepen[,17]*gamma*(1-gamma)^16+indepen[,18]*gamma*(1-gamma)^17
+indepen[,19]*gamma*(1-gamma)^18+indepen[,20]*gamma*(1-gamma)^19+indepen[,21]*gamma*(1-gamma)^20
+indepen[,22]*gamma*(1-gamma)^21+indepen[,23]*gamma*(1-gamma)^22+indepen[,24]*gamma*(1-gamma)^23
+indepen[,25]*gamma*(1-gamma)^24+indepen[,26]*gamma*(1-gamma)^25+indepen[,27]*gamma*(1-gamma)^26
+indepen[,28]*gamma*(1-gamma)^27+indepen[,29]*gamma*(1-gamma)^28+indepen[,30]*gamma*(1-gamma)^29
+indepen[,31]*gamma*(1-gamma)^30+indepen[,32]*gamma*(1-gamma)^31+indepen[,33]*gamma*(1-gamma)^32
+indepen[,34]*gamma*(1-gamma)^33+indepen[,35]*gamma*(1-gamma)^34+indepen[,36]*gamma*(1-gamma)^35
+indepen[,37]*gamma*(1-gamma)^36+indepen[,38]*gamma*(1-gamma)^37+indepen[,39]*gamma*(1-gamma)^38
+indepen[,40]*gamma*(1-gamma)^39+indepen[,41]*gamma*(1-gamma)^40+indepen[,42]*gamma*(1-gamma)^41
+indepen[,43]*gamma*(1-gamma)^42+indepen[,44]*gamma*(1-gamma)^43+indepen[,45]*gamma*(1-gamma)^44
+indepen[,46]*gamma*(1-gamma)^45+indepen[,47]*gamma*(1-gamma)^46+indepen[,48]*gamma*(1-gamma)^47
+lamda*indust)
m1 <- nls(formula,start=list(alpha = 0, gamma = 0.246, lamda = 0))
coeftest(m1)

##US result 90-99
depen <- pi[516:635,]
indust <- y[85:204,]
indepen <- NA
indepen <- pi[515:634,,drop=F]
for (i in 1:47) {
  indepen <- cbind(indepen,pi[(515-i):(634-i),,drop=F])
}
m2 <- nls(formula,start=list(alpha = 0, gamma = 0.088, lamda = 0))
coeftest(m2)

##US result 00-07
depen <- pi[636:731,]
indust <- y[205:300,]
indepen <- NA
indepen <- pi[635:730,,drop=F]
for (i in 1:399) {
  indepen <- cbind(indepen,pi[(635-i):(730-i),,drop=F])
}
m3 <- nls(formula,start=list(alpha = 0, gamma = 0.05, lamda = 0))
coeftest(m3)

##US result 08-16
depen <- pi[732:833,]
indust <- y[301:402,]
indepen <- NA
indepen <- pi[731:832,,drop=F]
for (i in 1:47) {
  indepen <- cbind(indepen,pi[(731-i):(832-i),,drop=F])
}
m4 <- nls(formula,start=list(alpha = 0, gamma = 0, lamda = 0))
coeftest(m4)