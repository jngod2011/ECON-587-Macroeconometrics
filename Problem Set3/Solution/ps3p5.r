require(nleqslv)
nlsys <- function(x) {
  y <- numeric(4)
  s2 <- x[1]; t1 <- x[2]; t2 <- x[3]; t3 <- x[4]	
  y[1] <- 1-s2*(1+t1^2+t2^2+t3^2)
  y[2] <- .35-(t1+t2*t1+t3*t2)*s2
  y[3] <- .15-s2*(t3*t1+t2)
  y[4] <- .1-t3*s2
  #x <- c(s2,t1,t2,t3)
  y	
}

xstart <- c(.01,0.01,.01,.01)
fstart <- nlsys(xstart)
xstart
fstart


nleqslv(xstart, nlsys, control=list(btol=.01))