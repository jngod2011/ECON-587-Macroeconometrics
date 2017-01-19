suppressMessages(library(BB))
fun <- function(x) {   
  f <- numeric(length(x))                      
  f[1] <-  x[1]+x[1]*x[2]+x[2]*x[3]-0.35*(1+x[1]^2+x[2]^2+x[3]^2) 
  f[2] <-  x[2]+x[1]*x[3]-0.15*(1+x[1]^2+x[2]^2+x[3]^2) 
  f[3] <-  x[3]-0.1*(1+x[1]^2+x[2]^2+x[3]^2) 
  f   
}   
startx <- c(0.5,0.5,0.5)  
result = dfsane(startx,fun,control=list(maxit=2500,trace = TRUE))  
theta = result$par
sigma2 = 1/(1+t(theta)%*%theta)
theta
sigma2
(theta[1]+theta[1]*theta[2]+theta[2]*theta[3])*sigma2
(theta[2]+theta[1]*theta[3])*sigma2
(theta[3])*sigma2
