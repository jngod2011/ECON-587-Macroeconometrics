suppressMessages(library(vars))
suppressMessages(library(lmtest))
Inflation <- read.csv("D:/Dropbox/16summer/Macroeconometrics/Problem Set3/CPILFESL.csv")
UNRATE <- read.csv("D:/Dropbox/16summer/Macroeconometrics/Problem Set3/UNRATE.csv")
y <- cbind(Inflation[,2],UNRATE[,2])
colnames(y)[2] <- "UNRATE"
colnames(y)[1] <- "Inflation"
VAR(y,p=5,type="const")
grangertest(Inflation[,2],UNRATE[,2],order = 5)
grangertest(UNRATE[,2],Inflation[,2],order = 5)
