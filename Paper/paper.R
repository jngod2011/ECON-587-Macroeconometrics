Expected.Inflation <- read.csv("D:/Dropbox/16summer/Macroeconometrics/Paper/Expected Inflation.csv")
CPIAUCSL <- read.csv("D:/Dropbox/16summer/Macroeconometrics/Paper/CPIAUCSL.csv")
five_five_year <- 2*Expected.Inflation[,11]-Expected.Inflation[,6]
real <- CPIAUCSL[,2]/100
one_year <- Expected.Inflation[,2]
f_five_five_year <- five_five_year[-(1:12)]
d_five_five_year <- f_five_five_year-five_five_year[1:403]
x <- real[1:403]-one_year[1:403]
summary(lm(d_five_five_year[1:300]~x[1:300]))
summary(lm(d_five_five_year[1:402]~x[1:402]))
summary(lm(d_five_five_year[1:84]~x[1:84]))
summary(lm(d_five_five_year[85:204]~x[85:204]))
summary(lm(d_five_five_year[205:300]~x[205:300]))
summary(lm(d_five_five_year[301:402]~x[301:402]))