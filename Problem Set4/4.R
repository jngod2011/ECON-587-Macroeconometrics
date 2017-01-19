suppressMessages(library(vars))
UNRATE <- read.csv("D:/Dropbox/16summer/Macroeconometrics/Problem Set4/UNRATE.csv")
CPILFESL <- read.csv("D:/Dropbox/16summer/Macroeconometrics/Problem Set4/CPILFESL.csv")
PPIACO <- read.csv("D:/Dropbox/16summer/Macroeconometrics/Problem Set4/PPIACO.csv")
FEDFUNDS <- read.csv("D:/Dropbox/16summer/Macroeconometrics/Problem Set4/FEDFUNDS.csv")
y <- cbind(FEDFUNDS[,2],CPILFESL[,2],UNRATE[,2])
colnames(y) <- c("FEDFUNDS","CPILFESL","UNRATE")
policy.var <- VAR(y,p=6)
amat <- diag(3)
bmat <- diag(3)
diag(bmat) <- NA
amat[2,1] <- NA
amat[3,1] <- NA
amat[3,2] <- NA
SVAR(policy.var,Amat=amat,Bmat=bmat,lrtest = FALSE)
policy.svar <- SVAR(policy.var,Amat=amat,Bmat=bmat,lrtest = FALSE)
plot(irf(policy.svar,impulse = "FEDFUNDS", response ="CPILFESL"))
fevd(policy.svar)$CPILFESL

y <- cbind(FEDFUNDS[,2],UNRATE[,2],CPILFESL[,2])
colnames(y) <- c("FEDFUNDS","UNRATE","CPILFESL")
policy.var <- VAR(y,p=6)
amat <- diag(3)
bmat <- diag(3)
diag(bmat) <- NA
amat[2,1] <- NA
amat[3,1] <- NA
amat[3,2] <- NA
SVAR(policy.var,Amat=amat,Bmat=bmat,lrtest = FALSE)
policy.svar <- SVAR(policy.var,Amat=amat,Bmat=bmat,lrtest = FALSE)
plot(irf(policy.svar,impulse = "FEDFUNDS", response ="CPILFESL"))
fevd(policy.svar)$CPILFESL

y <- cbind(CPILFESL[,2],UNRATE[,2],FEDFUNDS[,2])
colnames(y) <- c("CPILFESL","UNRATE","FEDFUNDS")
policy.var <- VAR(y,p=6)
amat <- diag(3)
bmat <- diag(3)
diag(bmat) <- NA
amat[2,1] <- NA
amat[3,1] <- NA
amat[3,2] <- NA
SVAR(policy.var,Amat=amat,Bmat=bmat,lrtest = FALSE)
policy.svar <- SVAR(policy.var,Amat=amat,Bmat=bmat,lrtest = FALSE)
plot(irf(policy.svar,impulse = "FEDFUNDS", response ="CPILFESL"))
fevd(policy.svar)$CPILFESL

y <- cbind(UNRATE[,2],CPILFESL[,2],FEDFUNDS[,2])
colnames(y) <- c("UNRATE","CPILFESL","FEDFUNDS")
policy.var <- VAR(y,p=6)
amat <- diag(3)
bmat <- diag(3)
diag(bmat) <- NA
amat[2,1] <- NA
amat[3,1] <- NA
amat[3,2] <- NA
SVAR(policy.var,Amat=amat,Bmat=bmat,lrtest = FALSE)
policy.svar <- SVAR(policy.var,Amat=amat,Bmat=bmat,lrtest = FALSE)
plot(irf(policy.svar,impulse = "FEDFUNDS", response ="CPILFESL"))
fevd(policy.svar)$CPILFESL

##NEW
y.new <- cbind(PPIACO[,2],UNRATE[,2],CPILFESL[,2],FEDFUNDS[,2])
colnames(y.new) <- c("PPIACO","UNRATE","CPILFESL","FEDFUNDS")
policy.var.new <- VAR(y.new,p=6)
amat.new <- diag(4)
bmat.new <- diag(4)
diag(bmat.new) <- NA
amat.new[2,1] <- NA
amat.new[3,1] <- NA
amat.new[3,2] <- NA
amat.new[4,1] <- NA
amat.new[4,2] <- NA
amat.new[4,3] <- NA
SVAR(policy.var.new,Amat=amat.new,Bmat=bmat.new,lrtest = FALSE)
policy.svar.new <- SVAR(policy.var.new,Amat=amat.new,Bmat=bmat.new,lrtest = FALSE)
plot(irf(policy.svar.new,impulse = "FEDFUNDS", response ="CPILFESL"))
fevd(policy.svar.new)$CPILFESL

y.new <- cbind(PPIACO[,2],FEDFUNDS[,2],UNRATE[,2],CPILFESL[,2])
colnames(y.new) <- c("PPIACO","FEDFUNDS","UNRATE","CPILFESL")
policy.var.new <- VAR(y.new,p=6)
amat.new <- diag(4)
bmat.new <- diag(4)
diag(bmat.new) <- NA
amat.new[2,1] <- NA
amat.new[3,1] <- NA
amat.new[3,2] <- NA
amat.new[4,1] <- NA
amat.new[4,2] <- NA
amat.new[4,3] <- NA
SVAR(policy.var.new,Amat=amat.new,Bmat=bmat.new,lrtest = FALSE)
policy.svar.new <- SVAR(policy.var.new,Amat=amat.new,Bmat=bmat.new,lrtest = FALSE)
plot(irf(policy.svar.new,impulse = "FEDFUNDS", response ="CPILFESL"))
fevd(policy.svar.new)$CPILFESL

y.new <- cbind(FEDFUNDS[,2],PPIACO[,2],UNRATE[,2],CPILFESL[,2])
colnames(y.new) <- c("FEDFUNDS","PPIACO","UNRATE","CPILFESL")
policy.var.new <- VAR(y.new,p=6)
amat.new <- diag(4)
bmat.new <- diag(4)
diag(bmat.new) <- NA
amat.new[2,1] <- NA
amat.new[3,1] <- NA
amat.new[3,2] <- NA
amat.new[4,1] <- NA
amat.new[4,2] <- NA
amat.new[4,3] <- NA
SVAR(policy.var.new,Amat=amat.new,Bmat=bmat.new,lrtest = FALSE)
policy.svar.new <- SVAR(policy.var.new,Amat=amat.new,Bmat=bmat.new,lrtest = FALSE)
plot(irf(policy.svar.new,impulse = "FEDFUNDS", response ="CPILFESL"))
fevd(policy.svar.new)$CPILFESL