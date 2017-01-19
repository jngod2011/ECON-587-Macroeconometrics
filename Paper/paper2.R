CPIAUCSL2 <- read.csv("D:/Dropbox/16summer/Macroeconometrics/Paper/CPIAUCSL (3).csv")
INDPRO <- read.csv("D:/Dropbox/16summer/Macroeconometrics/Paper/INDPRO.csv")
pi <- CPIAUCSL2[,2,drop=F]
y <- INDPRO[,2,drop=F]
##US result 83-89
depen <- pi[432:515,,drop=F]
indust <- y[1:84,,drop=F]
indepen <- NA
indepen <- pi[431:514,,drop=F]
for (i in 1:47) {
  indepen <- cbind(indepen,pi[(431-i):(514-i),,drop=F])
}

ols = function(v){
  alpha=v[1]
  gamma=v[2]
  lamda=v[3]
  beta=rep(NA,48)
  for(i in 1:48){
    beta[i]=gamma*(1-gamma)^(i-1)
  }
  theta=c(alpha,beta,lamda)
  ls = 0
  for(j in 1:84){
    ls = ls +(as.numeric(depen[j,])-as.numeric(c(1,indepen[j,],indust[j,]))%*%theta)^2
  }
  return(ls/84)
}
res = optim(par=rep(0,3),fn = ols,method="BFGS")
res$par

##US result 90-99
depen <- pi[516:635,,drop=F]
indust <- y[85:204,,drop=F]
indepen <- NA
indepen <- pi[515:634,,drop=F]
for (i in 1:47) {
  indepen <- cbind(indepen,pi[(515-i):(634-i),,drop=F])
}

ols = function(v){
  alpha=v[1]
  gamma=v[2]
  lamda=v[3]
  beta=rep(NA,48)
  for(i in 1:48){
    beta[i]=gamma*(1-gamma)^(i-1)
  }
  theta=c(alpha,beta,lamda)
  ls = 0
  for(j in 1:120){
    ls = ls +(as.numeric(depen[j,])-as.numeric(c(1,indepen[j,],indust[j,]))%*%theta)^2
  }
  return(ls/120)
}
res = optim(par=rep(0,3),fn = ols,method="BFGS")
res$par

##US result 00-07
depen <- pi[636:731,,drop=F]
indust <- y[205:300,,drop=F]
indepen <- NA
indepen <- pi[635:730,,drop=F]
for (i in 1:399) {
  indepen <- cbind(indepen,pi[(635-i):(730-i),,drop=F])
}

ols = function(v){
  alpha=v[1]
  gamma=v[2]
  lamda=v[3]
  beta=rep(NA,400)
  for(i in 1:400){
    beta[i]=gamma*(1-gamma)^(i-1)
  }
  theta=c(alpha,beta,lamda)
  ls = 0
  for(j in 1:96){
    ls = ls +(as.numeric(depen[j,])-as.numeric(c(1,indepen[j,],indust[j,]))%*%theta)^2
  }
  return(ls/96)
}
res = optim(par=rep(0,3),fn = ols,method="BFGS")
res$par

##US result 08-16
depen <- pi[732:833,,drop=F]
indust <- y[301:402,,drop=F]
indepen <- NA
indepen <- pi[731:832,,drop=F]
for (i in 1:47) {
  indepen <- cbind(indepen,pi[(731-i):(832-i),,drop=F])
}

ols = function(v){
  alpha=v[1]
  gamma=v[2]
  lamda=v[3]
  beta=rep(NA,48)
  for(i in 1:48){
    beta[i]=gamma*(1-gamma)^(i-1)
  }
  theta=c(alpha,beta,lamda)
  ls = 0
  for(j in 1:102){
    ls = ls +(as.numeric(depen[j,])-as.numeric(c(1,indepen[j,],indust[j,]))%*%theta)^2
  }
  return(ls/102)
}
res = optim(par=rep(0,3),fn = ols,method="BFGS")
res$par

