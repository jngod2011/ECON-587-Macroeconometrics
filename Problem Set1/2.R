##assign assumed values
a <- 50
b <- 1
gama <- 1
beta <- 0.5
t <- c(1:100)
d <- rep(NA,100)
s <- rep(NA,100)
p <- rep(NA,100)
epsilon <- 1
##start from steady state
p[1] <- (a-b)/(beta+gama)
d[1] <- a-gama*p[1]
s[1] <- d[1]
##give the system a shock
s[2] <- b+beta*p[1]+epsilon
d[2] <- s[2]
p[2] <- (a-d[2])/gama
for(i in 3:100){
  s[i] <- b+beta*p[i-1]
  d[i] <- s[i]
  p[i] <- (a-d[i])/gama
}
##calculate IM and IRF
IM <- (p[2]-p[1])/epsilon
IRF <- rep(NA,100)
for(i in 1:100){
  IRF[i] <- (p[i+2]-p[1])/epsilon
}
##plot IRF
plot(t,IRF,type = "l")
##theoretical values of IM and IRF,test whether our results are valid
True.IM <- -1/gama
True.IRF <- -(1/gama)*(-beta/gama)^t
IM-True.IM
IRF-True.IRF
##draw the cobweb plot
cobweb.Q <- rep(NA,200)
cobweb.P <- rep(NA,200)
for(i in 1:100){
  cobweb.Q[2*i-1] <- s[i]
  cobweb.Q[2*i] <- d[i+1]
  cobweb.P[2*i-1] <- p[i]
  cobweb.P[2*i] <- p[i]
}
plot(cobweb.Q,cobweb.P,type="l",main = "Cobweb Dynamics")
lines(d,p,col="blue")
lines(s[-c(1,2)],p[-c(1,100)],col="red")
legend("topright",c("cobweb","demand curve","supply curve"),
       lty = c(1,1,1),pch = c(46,46,46),col = c("black","blue","red"))
